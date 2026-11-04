# =============================================================================
# 12_espacial_inequidades.R
# A. Modelos BYM2 (INLA) por municipio con covariables, por epidemia y edad:
#    2025 (todas las edades, <1 año, 10+ años) y 2011-2013 (todas las edades).
#    Lógica de control negativo: el control prenatal (vía de la Tdap materna)
#    debería asociarse con la incidencia en lactantes y no en 10+ años.
# B. Modelo de cambio: 2025 frente a la línea base propia (2015-2019) de cada
#    municipio (ajusta la capacidad diagnóstica local): dos BYM2 (base y cambio).
# C. Autocorrelación espacial (I de Moran con ajuste bayesiano empírico) y
#    escaneo espacial de Kulldorff (2025).
# D. Desigualdades según el IPM municipal: incidencia estandarizada, mortalidad
#    infantil y letalidad por quintil; índice relativo de desigualdad (RII).
# =============================================================================
suppressMessages({ library(data.table); library(sf); library(spdep); library(INLA); library(readxl)
                   library(SpatialEpi); library(MASS) })
INLA::inla.setOption(num.threads = "4:1")
set.seed(20254)
sf_use_s2(FALSE)
cs <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
pm <- readRDS("01_datos/procesados/pob_mun_grupos.rds")
covt <- readRDS("01_datos/procesados/covariables_municipales_terridata.rds")

recode_mpio <- function(x) fcase(x == "94663", "50325", x == "27086", "05480", default = x)
cs[, MPIO := recode_mpio(mpio_res)]
pm[, MPIO := recode_mpio(MPIO)]; pm <- pm[, .(pop = sum(pop)), by = .(MPIO, anio, area, grupo)]
covt[, MPIO := recode_mpio(MPIO)]; covt <- covt[, lapply(.SD, mean, na.rm = TRUE), by = MPIO]

gj <- st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE)
gj <- st_make_valid(st_transform(gj, 9377)); gj$MPIO <- gj$MPIO_CDPMP
gj <- gj[order(gj$MPIO), ]; gj$area_km2 <- as.numeric(gj$AREA) / 1e6
nb <- poly2nb(gj, queen = TRUE, snap = 1e-4)
cent <- st_coordinates(st_centroid(st_geometry(gj)))
for (i in which(card(nb) == 0)) { dd <- sqrt(colSums((t(cent) - cent[i, ])^2)); dd[i] <- Inf; j <- which.min(dd)
  nb[[i]] <- as.integer(j); nb[[j]] <- sort(unique(c(nb[[j]], as.integer(i)))) }
nb2INLA("01_datos/procesados/grafo_municipios.adj", nb); g <- inla.read.graph("01_datos/procesados/grafo_municipios.adj")
base <- data.table(MPIO = gj$MPIO, idx = seq_len(nrow(gj)), area_km2 = gj$area_km2, DP = substr(gj$MPIO, 1, 2))

# Covariables municipales
ipm <- as.data.table(read_excel("01_datos/raw/dane_ipm/anexo-censal-pobreza-municipal-2018.xlsx", sheet = "2_IPM Mpio y Dpt", col_types = "text"))
setnames(ipm, 1:3, c("MPIO", "nombre", "ipm")); ipm <- ipm[grepl("^[0-9]{5}$", MPIO), .(MPIO = recode_mpio(MPIO), ipm = as.numeric(ipm))][, .(ipm = mean(ipm)), by = MPIO]
et <- as.data.table(read_excel("01_datos/raw/dane_etnia/anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun.xlsx", col_names = FALSE, col_types = "text", skip = 7))
setnames(et, c("dp", "dpnom", "MPIO", "mpnom", "anio", "area", "total", "indigena", "rrom", "raizal", "palenquero", "afro", "ninguno"))
et <- et[area == "Total" & anio == "2025", .(MPIO = recode_mpio(MPIO), tot = as.numeric(total), ind = as.numeric(indigena),
                                             afr = as.numeric(afro) + as.numeric(raizal) + as.numeric(palenquero))][, .(p_ind = sum(ind) / sum(tot), p_afro = sum(afr) / sum(tot)), by = MPIO]
rur <- pm[anio == 2025, .(p_rural = sum(pop[area == "rural"]) / sum(pop[area == "total"]), pop25 = sum(pop[area == "total"])), by = MPIO]
cov <- Reduce(function(x, y) merge(x, y, by = "MPIO", all.x = TRUE), list(base, ipm, et, rur, covt))
cov[, log_dens := log(pop25 / area_km2)]
zs <- function(v) { z <- (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE); z[is.na(z)] <- 0; z }
for (v in c("ipm", "p_rural", "p_ind", "p_afro", "log_dens", "penta_1519", "penta_0810", "cpn4_1924", "cpn4_0810", "subsidiado_1924"))
  cov[, (paste0("z_", v)) := zs(get(v))]
cor_tab <- round(cor(cov[, .(ipm, p_rural, p_ind, p_afro, log_dens, penta_1519, cpn4_1924)], use = "pairwise.complete.obs"), 2)
fwrite(as.data.table(cor_tab, keep.rownames = "variable"), "03_resultados/espacial_correlacion_covariables.csv")

# Conteos observados y esperados (estandarización indirecta por grupo de edad)
expected <- function(yrs, grupos = NULL) {
  pg <- pm[area == "total" & anio %in% yrs, .(pop = sum(pop)), by = .(MPIO, grupo)]
  cc <- cs[anio %in% yrs & !is.na(grupo_pob)]
  if (!is.null(grupos)) { pg <- pg[grupo %in% grupos]; cc <- cc[grupo_pob %in% grupos] }
  nat <- merge(pg[, .(pop = sum(pop)), by = grupo], cc[, .(casos = .N), by = .(grupo = grupo_pob)], by = "grupo", all.x = TRUE)
  nat[is.na(casos), casos := 0][, tasa := casos / pop]
  e <- merge(pg, nat[, .(grupo, tasa)], by = "grupo")[, .(E = sum(pop * tasa)), by = MPIO]
  o <- cc[!is.na(MPIO), .(O = .N), by = MPIO]
  x <- merge(e, o, by = "MPIO", all.x = TRUE); x[is.na(O), O := 0L]; x
}
pc_bym <- list(phi = list(prior = "pc", param = c(0.5, 2 / 3)), prec = list(prior = "pc.prec", param = c(1, 0.01)))

fit_bym <- function(yrs, grupos, covs, lab) {
  d <- merge(cov, expected(yrs, grupos), by = "MPIO", all.x = TRUE)[order(idx)]
  d[is.na(O), O := 0L]; d <- d[!is.na(E) & E > 0]
  f <- as.formula(paste("O ~ 1 +", paste(covs, collapse = " + "),
                        "+ f(idx, model = 'bym2', graph = g, scale.model = TRUE, constr = TRUE, hyper = pc_bym)"))
  fit <- inla(f, family = "poisson", data = as.data.frame(d), E = E, control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE, return.marginals.predictor = TRUE))
  fe <- as.data.table(fit$summary.fixed, keep.rownames = "termino")[, .(modelo = lab, termino, rr = exp(`0.5quant`),
                                                                       lo = exp(`0.025quant`), hi = exp(`0.975quant`))]
  pex <- vapply(fit$marginals.fitted.values, function(mg) 1 - inla.pmarginal(1, mg), numeric(1))
  mun <- d[, .(MPIO, O, E)][, `:=`(rr = fit$summary.fitted.values$`0.5quant`, p_exc = pex, modelo = lab)]
  hyp <- as.data.table(fit$summary.hyperpar, keep.rownames = "param")[, modelo := lab]
  list(fe = fe, mun = mun, hyp = hyp, waic = fit$waic$waic)
}
cv25 <- c("z_ipm", "z_p_rural", "z_p_ind", "z_p_afro", "z_log_dens", "z_penta_1519", "z_cpn4_1924")
cv11 <- c("z_ipm", "z_p_rural", "z_p_ind", "z_p_afro", "z_log_dens", "z_penta_0810", "z_cpn4_0810")
mods <- list(
  fit_bym(2025, NULL, cv25, "2025 todas las edades"),
  fit_bym(2025, "0", cv25, "2025 <1 año"),
  fit_bym(2025, c("10-14", "15-19", "20-39", "40-59", "60+"), cv25, "2025 10+ años (control negativo)"),
  fit_bym(2011:2013, NULL, cv11, "2011-2013 todas las edades"),
  fit_bym(2011:2013, "0", cv11, "2011-2013 <1 año"))
FE <- rbindlist(lapply(mods, `[[`, "fe")); MUN <- rbindlist(lapply(mods, `[[`, "mun")); HYP <- rbindlist(lapply(mods, `[[`, "hyp"))
fwrite(FE, "03_resultados/espacial_bym2_efectos_fijos.csv"); fwrite(MUN, "03_resultados/espacial_bym2_rr_municipal.csv")
fwrite(HYP, "03_resultados/espacial_bym2_hiperparametros.csv")

# =============================================================================
# B. Modelo de cambio 2025 vs línea base propia (2015-2019)
# =============================================================================
b0 <- merge(cov, expected(2015:2019), by = "MPIO", all.x = TRUE)[order(idx)][, per := 0]
b1 <- merge(cov, expected(2025), by = "MPIO", all.x = TRUE)[order(idx)][, per := 1]
dd <- rbind(b0, b1)[!is.na(E) & E > 0]
dd[is.na(O), O := 0L]
dd[, `:=`(idx_b = idx, idx_c = fifelse(per == 1, idx, NA_integer_))]
for (v in cv25) dd[, (paste0(v, "_x25")) := get(v) * per]
fC <- as.formula(paste("O ~ 1 + per +", paste(paste0(cv25, "_x25"), collapse = " + "),
                       "+ f(idx_b, model = 'bym2', graph = g, scale.model = TRUE, constr = TRUE, hyper = pc_bym)",
                       "+ f(idx_c, model = 'bym2', graph = g, scale.model = TRUE, constr = TRUE, hyper = pc_bym)"))
fitC <- inla(fC, family = "poisson", data = as.data.frame(dd), E = E, control.predictor = list(compute = TRUE),
             control.compute = list(dic = TRUE, waic = TRUE))
feC <- as.data.table(fitC$summary.fixed, keep.rownames = "termino")[, .(modelo = "Cambio 2025 vs 2015-2019", termino,
                                                                       rr = exp(`0.5quant`), lo = exp(`0.025quant`), hi = exp(`0.975quant`))]
nn <- nrow(gj)
chg <- data.table(MPIO = gj$MPIO, cambio = exp(fitC$summary.random$idx_c$`0.5quant`[1:nn]),
                  cambio_lo = exp(fitC$summary.random$idx_c$`0.025quant`[1:nn]), cambio_hi = exp(fitC$summary.random$idx_c$`0.975quant`[1:nn]))
fwrite(feC, "03_resultados/espacial_cambio_efectos_fijos.csv"); fwrite(chg, "03_resultados/espacial_cambio_municipal.csv")

# =============================================================================
# C. I de Moran (bayes empírico) y escaneo de Kulldorff (2025)
# =============================================================================
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
x25 <- merge(base, expected(2025), by = "MPIO", all.x = TRUE)[order(idx)]; x25[is.na(O), O := 0L]
p25 <- merge(base, pm[area == "total" & anio == 2025, .(pop = sum(pop)), by = MPIO], by = "MPIO", all.x = TRUE)[order(idx)]
x11 <- merge(base, expected(2011:2013), by = "MPIO", all.x = TRUE)[order(idx)]; x11[is.na(O), O := 0L]
p11 <- merge(base, pm[area == "total" & anio %in% 2011:2013, .(pop = sum(pop)), by = MPIO], by = "MPIO", all.x = TRUE)[order(idx)]
mor <- rbindlist(lapply(list(`2025` = list(x25, p25), `2011-2013` = list(x11, p11)), function(z) {
  ok <- !is.na(z[[2]]$pop) & z[[2]]$pop > 0
  lws <- if (all(ok)) lw else subset.listw(lw, ok, zero.policy = TRUE)
  m <- EBImoran.mc(z[[1]]$O[ok], z[[2]]$pop[ok], lws, nsim = 999, zero.policy = TRUE)
  data.table(I = m$statistic, p = m$p.value)
}), idcol = "periodo")
fwrite(mor, "03_resultados/espacial_moran.csv")
geo_km <- cent / 1000
ok <- !is.na(p25$pop) & p25$pop > 0
kd <- kulldorff(geo_km[ok, ], cases = x25$O[ok], population = p25$pop[ok], expected.cases = x25$E[ok],
                pop.upper.bound = 0.1, n.simulations = 999, alpha.level = 0.05, plot = FALSE)
cl_list <- c(list(kd$most.likely.cluster), kd$secondary.clusters)
mp <- gj$MPIO[ok]
clus <- rbindlist(lapply(seq_along(cl_list), function(i) { cl <- cl_list[[i]]
  data.table(cluster = i, n_municipios = length(cl$location.IDs.included), observados = cl$number.of.cases,
             esperados = cl$expected.cases, smr = cl$SMR, p = cl$p.value,
             municipios = paste(head(mp[cl$location.IDs.included], 12), collapse = " "))
}))
fwrite(clus, "03_resultados/espacial_kulldorff_2025.csv")
# Sensibilidad: ventana máxima del 20% de la población (permite que Bogotá, ~15%, forme conglomerado)
kd20 <- kulldorff(geo_km[ok, ], cases = x25$O[ok], population = p25$pop[ok], expected.cases = x25$E[ok],
                  pop.upper.bound = 0.2, n.simulations = 999, alpha.level = 0.05, plot = FALSE)
cl20 <- c(list(kd20$most.likely.cluster), kd20$secondary.clusters)
clus20 <- rbindlist(lapply(seq_along(cl20), function(i) { cl <- cl20[[i]]
  data.table(cluster = i, n_municipios = length(cl$location.IDs.included), observados = cl$number.of.cases,
             esperados = cl$expected.cases, smr = cl$SMR, p = cl$p.value,
             municipios = paste(head(mp[cl$location.IDs.included], 12), collapse = " ")) }))
fwrite(clus20, "03_resultados/espacial_kulldorff_2025_ventana20.csv")
saveRDS(list(MUN = MUN, chg = chg, clus = clus, kd_ids = lapply(cl_list, function(cl) mp[cl$location.IDs.included])),
        "01_datos/procesados/espacial_resultados.rds")

# =============================================================================
# D. Desigualdades según IPM (quintiles de municipios ponderados por población)
# =============================================================================
q <- merge(cov[, .(MPIO, ipm)], pm[area == "total" & anio == 2018, .(pop18 = sum(pop)), by = MPIO], by = "MPIO")[!is.na(ipm)]
setorder(q, ipm); q[, cum := cumsum(pop18) / sum(pop18)]
q[, quintil := pmin(5L, as.integer(ceiling(cum * 5)))]; q[, ridit := (cumsum(pop18) - pop18 / 2) / sum(pop18)]
inq <- rbindlist(lapply(list(`2011-2013` = 2011:2013, `2014-2019` = 2014:2019, `2025` = 2025), function(yrs) {
  e <- merge(q, expected(yrs), by = "MPIO", all.x = TRUE); e[is.na(O), O := 0L]
  i0 <- cs[anio %in% yrs & lactante == TRUE & !is.na(MPIO), .(lact = .N, muertes = sum(muerte)), by = MPIO]
  p0 <- pm[area == "total" & grupo == "0" & anio %in% yrs, .(pop0 = sum(pop)), by = MPIO]
  e <- merge(merge(e, i0, by = "MPIO", all.x = TRUE), p0, by = "MPIO", all.x = TRUE)
  for (v in c("lact", "muertes")) e[is.na(get(v)), (v) := 0L]
  qq <- e[, .(O = sum(O), E = sum(E), lact = sum(lact), muertes = sum(muertes), pop0 = sum(pop0, na.rm = TRUE)), by = quintil][order(quintil)]
  qq[, `:=`(sir = O / E, mort_inf_100k = muertes / pop0 * 1e5, letalidad = muertes / lact)]
  # RII (incidencia estandarizada y mortalidad infantil) por regresión NB en el ridit municipal
  e2 <- e[E > 0]
  rii_inc <- glm.nb(O ~ ridit + offset(log(E)), data = e2)
  rii_mort <- glm(muertes ~ ridit + offset(log(pop0)), family = quasipoisson, data = e[pop0 > 0])
  f <- function(m) { b <- coef(summary(m))["ridit", ]; sprintf("%.2f (%.2f–%.2f)", exp(b[1]), exp(b[1] - 1.96 * b[2]), exp(b[1] + 1.96 * b[2])) }
  qq[, `:=`(RII_incidencia = f(rii_inc), RII_mortalidad_infantil = f(rii_mort))]
  qq
}), idcol = "periodo")
fwrite(inq, "03_resultados/inequidades_ipm_quintiles.csv")

options(width = 220)
print(cor_tab)
print(FE[termino != "(Intercept)", .(modelo, termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(feC[, .(termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(mor); print(clus[, .(cluster, n_municipios, observados, esperados = round(esperados, 1), smr = round(smr, 2), p, municipios)])
print(clus20[, .(cluster, n_municipios, observados, esperados = round(esperados, 1), smr = round(smr, 2), p, municipios)])
print(inq[, .(periodo, quintil, O, sir = round(sir, 2), mort_inf_100k = round(mort_inf_100k, 2), letalidad = round(letalidad, 3), RII_incidencia, RII_mortalidad_infantil)])
for (m in mods) cat(m$mun$modelo[1], "WAIC:", round(m$waic, 1), " municipios P(RR>1)>0.95:", m$mun[p_exc > 0.95, .N], "\n")
