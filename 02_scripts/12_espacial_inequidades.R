# =============================================================================
# 12_espacial_inequidades.R  (versión del manuscrito v4, 12-11-2026)
# A. Modelos BYM2 (INLA) por unidad municipal con covariables, por epidemia y edad:
#    2025 (todas las edades, <1 año, 10+ años) y 2011-2013 (todas las edades, <1 año).
#    El control prenatal (CPN) se usa como exposición con controles negativos
#    (análisis exploratorio: no estaba en la propuesta inicial).
#    A1. Modelo conjunto con pendientes del CPN por grupo de edad (<2 m, 2-5 m,
#        6-11 m, 1-9 a, 10+ a) y contrastes formales entre pendientes.
#    A2. Sensibilidades: previas PC alternativas, sin Antioquia, BN con efectos fijos
#        de departamento (confusión espacial) y chequeo predictivo posterior.
# B. Modelo de cambio: 2025 frente a la línea base propia (2015-2019) de cada unidad.
# C. I de Moran (bayes empírico) y escaneo espacial de Kulldorff (2025).
# D. Desigualdades según el IPM municipal: tasas por quintil con IC exactos,
#    índice relativo (RII) y de la pendiente (SII) de desigualdad, cortes de los quintiles.
# Cambios frente a la v3:
#   - Mapiripana (94663), área no municipalizada que en 2019 pasó a formar el municipio de
#     Barrancominas (94343), se une a Barrancominas; la v3 la unía por error a Mapiripán (50325).
#   - Nuevo Belén de Bajirá (27493), que aparece en las proyecciones desde 2024 con población
#     separada de Riosucio, se une a Riosucio (27615); la v3 la dejaba fuera del análisis.
#   - Los polígonos unidos se disuelven: 1.121 unidades espaciales.
#   - Se registran las covariables faltantes (imputadas con la media, z = 0).
# =============================================================================
suppressMessages({ library(data.table); library(sf); library(spdep); library(INLA); library(readxl)
                   library(SpatialEpi); library(MASS) })
INLA::inla.setOption(num.threads = "4:1")
set.seed(20254)
sf_use_s2(FALSE)
cs <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
pm <- readRDS("01_datos/procesados/pob_mun_grupos.rds")
covt <- readRDS("01_datos/procesados/covariables_municipales_terridata.rds")

# Unidades que cambiaron después del Marco Geoestadístico Nacional (MGN) de 2018 -> unidad de destino
recode_mpio <- function(x) fcase(x == "94663", "94343", x == "27493", "27615", default = x)
cs[, MPIO := recode_mpio(mpio_res)]
pm[, MPIO := recode_mpio(MPIO)]; pm <- pm[, .(pop = sum(pop)), by = .(MPIO, anio, area, grupo)]
covt[, MPIO := recode_mpio(MPIO)]; covt <- covt[, lapply(.SD, mean, na.rm = TRUE), by = MPIO]
for (v in setdiff(names(covt), "MPIO")) covt[is.nan(get(v)), (v) := NA_real_]

gj0 <- st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE)
gj0 <- st_make_valid(st_transform(gj0, 9377))
gj0$MPIO <- recode_mpio(gj0$MPIO_CDPMP)
nm0 <- as.data.table(st_drop_geometry(gj0))[, .(MPIO, MPIO_CNMBR, MPIO_CDPMP)]
gj <- aggregate(gj0[, "AREA"], by = list(MPIO = gj0$MPIO), FUN = sum)
gj <- st_make_valid(gj); gj <- gj[order(gj$MPIO), ]; gj$area_km2 <- as.numeric(gj$AREA) / 1e6
gj$nombre <- nm0[match(gj$MPIO, MPIO_CDPMP), MPIO_CNMBR]; gj$dpto <- substr(gj$MPIO, 1, 2)
cat("Unidades espaciales:", nrow(gj), "(MGN 2018:", nrow(gj0), ")\n")
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
VARS <- c("ipm", "p_rural", "p_ind", "p_afro", "log_dens", "penta_1519", "penta_0810", "cpn4_1924", "cpn4_0810", "subsidiado_1924")
falt <- data.table(variable = VARS, unidades = nrow(cov), faltantes = vapply(VARS, function(v) sum(is.na(cov[[v]])), 1L),
                   unidades_faltantes = vapply(VARS, function(v) paste(head(cov[is.na(get(v)), MPIO], 30), collapse = " "), ""))
fwrite(falt, "03_resultados/espacial_covariables_faltantes.csv")
# Media y DE entre unidades de cada covariable (para traducir los resultados «por DE» a sus unidades)
fwrite(data.table(variable = VARS, media = vapply(VARS, function(v) mean(cov[[v]], na.rm = TRUE), 1),
                  de = vapply(VARS, function(v) sd(cov[[v]], na.rm = TRUE), 1), unidades_con_dato = vapply(VARS, function(v) sum(!is.na(cov[[v]])), 1L)),
       "03_resultados/espacial_covariables_resumen.csv")
zs <- function(v) { z <- (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE); z[is.na(z)] <- 0; z }   # faltante -> media
for (v in VARS) cov[, (paste0("z_", v)) := zs(get(v))]
cor_tab <- round(cor(cov[, .(ipm, p_rural, p_ind, p_afro, log_dens, penta_1519, cpn4_1924)], use = "pairwise.complete.obs"), 2)
fwrite(as.data.table(cor_tab, keep.rownames = "variable"), "03_resultados/espacial_correlacion_covariables.csv")

# Conteos observados y esperados (estandarización indirecta por grupo de edad, tasas nacionales del periodo)
expected <- function(yrs, grupos = NULL, excluir_dp = NULL) {
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
PRIORS <- list(principal = pc_bym,
               `prec U=0.5` = list(phi = list(prior = "pc", param = c(0.5, 2 / 3)), prec = list(prior = "pc.prec", param = c(0.5, 0.01))),
               `prec U=2` = list(phi = list(prior = "pc", param = c(0.5, 2 / 3)), prec = list(prior = "pc.prec", param = c(2, 0.01))),
               `phi mediana 0.5` = list(phi = list(prior = "pc", param = c(0.5, 0.5)), prec = list(prior = "pc.prec", param = c(1, 0.01))))

# Chequeo predictivo posterior (500 extracciones): ceros, máximo y discrepancia de Pearson
# T(y, mu_s) frente a T(yrep_s, mu_s), calculada en cada extracción s
ppc <- function(fit, d, lab, nsim = 500) {
  smp <- inla.posterior.sample(nsim, fit, selection = list(Predictor = 0), num.threads = "4:1")
  eta <- sapply(smp, function(s) s$latent[, 1])
  mu <- d$E * exp(eta)
  yrep <- matrix(rpois(length(mu), mu), nrow = nrow(mu))
  pe_obs <- colSums((d$O - mu)^2 / mu); pe_rep <- colSums((yrep - mu)^2 / mu)
  t_obs <- c(ceros = sum(d$O == 0), maximo = max(d$O), pearson = median(pe_obs))
  t_rep <- rbind(ceros = colSums(yrep == 0), maximo = apply(yrep, 2, max), pearson = pe_rep)
  pp <- c(mean(t_rep[1, ] >= t_obs[1]), mean(t_rep[2, ] >= t_obs[2]), mean(pe_rep >= pe_obs))
  data.table(modelo = lab, estadistico = names(t_obs), observado = t_obs, replicado_mediana = apply(t_rep, 1, median),
             replicado_p2.5 = apply(t_rep, 1, quantile, .025), replicado_p97.5 = apply(t_rep, 1, quantile, .975),
             p_predictivo = pp)
}

fit_bym <- function(yrs, grupos, covs, lab, prior = pc_bym, excluir_dp = NULL, chequeo = FALSE, familia = "poisson") {
  d <- merge(cov, expected(yrs, grupos), by = "MPIO", all.x = TRUE)[order(idx)]
  d[is.na(O), O := 0L]; d <- d[!is.na(E) & E > 0]
  if (!is.null(excluir_dp)) d <- d[!DP %in% excluir_dp]
  f <- as.formula(paste("O ~ 1 +", paste(covs, collapse = " + "),
                        "+ f(idx, model = 'bym2', graph = g, scale.model = TRUE, constr = TRUE, hyper = prior)"))
  fit <- inla(f, family = familia, data = as.data.frame(d), E = E, control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, waic = TRUE, config = chequeo, return.marginals.predictor = TRUE))
  fe <- as.data.table(fit$summary.fixed, keep.rownames = "termino")[, .(modelo = lab, termino, rr = exp(`0.5quant`),
                                                                       lo = exp(`0.025quant`), hi = exp(`0.975quant`))]
  pex <- vapply(fit$marginals.fitted.values, function(mg) 1 - inla.pmarginal(1, mg), numeric(1))
  mun <- d[, .(MPIO, O, E)][, `:=`(rr = fit$summary.fitted.values$`0.5quant`, p_exc = pex, modelo = lab)]
  hyp <- as.data.table(fit$summary.hyperpar, keep.rownames = "param")[, modelo := lab]
  diag <- data.table(modelo = lab, unidades = nrow(d), casos = sum(d$O), WAIC = fit$waic$waic, DIC = fit$dic$dic,
                     pD = fit$dic$p.eff)
  list(fe = fe, mun = mun, hyp = hyp, waic = fit$waic$waic, diag = diag, ppc = if (chequeo) ppc(fit, d, lab) else NULL)
}
cv25 <- c("z_ipm", "z_p_rural", "z_p_ind", "z_p_afro", "z_log_dens", "z_penta_1519", "z_cpn4_1924")
cv11 <- c("z_ipm", "z_p_rural", "z_p_ind", "z_p_afro", "z_log_dens", "z_penta_0810", "z_cpn4_0810")
ADULT <- c("10-14", "15-19", "20-39", "40-59", "60+")
ESPEC <- list(`2025 todas las edades` = list(2025, NULL, cv25), `2025 <1 año` = list(2025, "0", cv25),
              `2025 10+ años (control negativo)` = list(2025, ADULT, cv25),
              `2011-2013 todas las edades` = list(2011:2013, NULL, cv11), `2011-2013 <1 año` = list(2011:2013, "0", cv11))
mods <- lapply(names(ESPEC), function(k) { s <- ESPEC[[k]]; fit_bym(s[[1]], s[[2]], s[[3]], k, chequeo = TRUE) })
FE <- rbindlist(lapply(mods, `[[`, "fe")); MUN <- rbindlist(lapply(mods, `[[`, "mun")); HYP <- rbindlist(lapply(mods, `[[`, "hyp"))
fwrite(FE, "03_resultados/espacial_bym2_efectos_fijos.csv"); fwrite(MUN, "03_resultados/espacial_bym2_rr_municipal.csv")
fwrite(HYP, "03_resultados/espacial_bym2_hiperparametros.csv")
fwrite(rbindlist(lapply(mods, `[[`, "diag")), "03_resultados/espacial_bym2_diagnostico.csv")
fwrite(rbindlist(lapply(mods, `[[`, "ppc")), "03_resultados/espacial_bym2_chequeo_predictivo.csv")
cat("A lista: modelos BYM2 principales\n")

# -----------------------------------------------------------------------------
# A1. Modelo conjunto: pendientes del CPN por grupo de edad, efecto espacial BYM2 común
#     y término iid por unidad x grupo. Contrastes entre pendientes (razón de RR por DE).
# -----------------------------------------------------------------------------
ESTR <- c("<2 m", "2-5 m", "6-11 m", "1-9 a", "10+ a")
FR_INF <- c(`<2 m` = 61, `2-5 m` = 122, `6-11 m` = 182) / 365.25
estrato_caso <- function(x) x[, est := fcase(grupo_pob == "0" & edad_dias < 61, "<2 m", grupo_pob == "0" & edad_dias < 183, "2-5 m",
                                             grupo_pob == "0", "6-11 m", grupo_pob %in% c("1-4", "5-9"), "1-9 a",
                                             !is.na(grupo_pob), "10+ a", default = NA_character_)]
expected_estratos <- function(yrs) {
  pg <- pm[area == "total" & anio %in% yrs, .(pop = sum(pop)), by = .(MPIO, grupo)]
  inf <- rbindlist(lapply(names(FR_INF), function(s) pg[grupo == "0", .(MPIO, g = s, pop = pop * FR_INF[[s]])]))
  pg <- rbind(inf, pg[grupo != "0", .(MPIO, g = as.character(grupo), pop)])
  cc <- estrato_caso(cs[anio %in% yrs & !is.na(grupo_pob)])
  cc[, g := fifelse(grupo_pob == "0", est, as.character(grupo_pob))]
  nat <- merge(pg[, .(pop = sum(pop)), by = g], cc[, .(casos = .N), by = g], by = "g", all.x = TRUE)[is.na(casos), casos := 0][, tasa := casos / pop]
  mapa <- data.table(g = c(names(FR_INF), "1-4", "5-9", ADULT), est = c(names(FR_INF), "1-9 a", "1-9 a", rep("10+ a", 5)))
  e <- merge(merge(pg, nat[, .(g, tasa)], by = "g"), mapa, by = "g")[, .(E = sum(pop * tasa)), by = .(MPIO, est)]
  o <- cc[!is.na(MPIO), .(O = .N), by = .(MPIO, est)]
  x <- merge(e, o, by = c("MPIO", "est"), all.x = TRUE); x[is.na(O), O := 0L]; x
}
fit_conjunto <- function(yrs, covs, cpn, lab, prior = pc_bym, excluir_dp = NULL, especificas = FALSE) {
  d <- merge(cov, expected_estratos(yrs), by = "MPIO")[!is.na(E) & E > 0]
  if (!is.null(excluir_dp)) d <- d[!DP %in% excluir_dp]
  d[, est := factor(est, levels = ESTR)]; d[, id := .I]
  for (k in seq_along(ESTR)) { d[, (paste0("a", k)) := as.integer(est == ESTR[k])]; d[, (paste0("b", k)) := get(paste0("a", k)) * get(cpn)] }
  otras <- setdiff(covs, cpn)
  if (especificas) {
    for (v in otras) for (k in seq_along(ESTR)) d[, (paste0(v, "_e", k)) := get(paste0("a", k)) * get(v)]
    otras <- as.vector(outer(otras, paste0("_e", seq_along(ESTR)), paste0))
  }
  f <- as.formula(paste("O ~ -1 +", paste(c(paste0("a", 1:5), paste0("b", 1:5), otras), collapse = " + "),
                        "+ f(idx, model = 'bym2', graph = g, scale.model = TRUE, constr = TRUE, hyper = prior)",
                        "+ f(id, model = 'iid', hyper = list(prec = list(prior = 'pc.prec', param = c(1, 0.01))))"))
  lc <- c(inla.make.lincomb(b1 = 1, b3 = -1), inla.make.lincomb(b1 = 1, b5 = -1), inla.make.lincomb(b3 = 1, b5 = -1),
          inla.make.lincomb(b2 = 1, b3 = -1), inla.make.lincomb(b1 = 1, b4 = -1))
  names(lc) <- c("<2 m vs 6-11 m", "<2 m vs 10+ a", "6-11 m vs 10+ a", "2-5 m vs 6-11 m", "<2 m vs 1-9 a")
  fit <- inla(f, family = "poisson", data = as.data.frame(d), E = E, lincomb = lc,
              control.compute = list(dic = TRUE, waic = TRUE))
  sf_ <- as.data.table(fit$summary.fixed, keep.rownames = "termino")
  pend <- sf_[termino %in% paste0("b", 1:5)][, .(modelo = lab, termino = paste("CPN,", ESTR[as.integer(sub("b", "", termino))]),
                                                 rr = exp(`0.5quant`), lo = exp(`0.025quant`), hi = exp(`0.975quant`), media = mean, de = sd)]
  con <- as.data.table(fit$summary.lincomb.derived, keep.rownames = "termino")[, .(modelo = lab, termino = paste("Razón de pendientes,", termino),
                                                                                  rr = exp(`0.5quant`), lo = exp(`0.025quant`), hi = exp(`0.975quant`), media = mean, de = sd)]
  rbind(pend, con)[, `:=`(casos = sum(d$O), filas = nrow(d))]
}
CONJ <- rbind(
  fit_conjunto(2025, cv25, "z_cpn4_1924", "Conjunto 2025"),
  fit_conjunto(2011:2013, cv11, "z_cpn4_0810", "Conjunto 2011-2013"),
  fit_conjunto(2025, cv25, "z_cpn4_1924", "Conjunto 2025, covariables por grupo", especificas = TRUE),
  fit_conjunto(2025, cv25, "z_cpn4_1924", "Conjunto 2025, sin Antioquia", excluir_dp = "05"))
# Diferencia de diferencias entre periodos (posteriores independientes, aproximación normal)
dd_ <- merge(CONJ[modelo == "Conjunto 2025" & grepl("^Razón", termino), .(termino, m25 = media, s25 = de)],
             CONJ[modelo == "Conjunto 2011-2013" & grepl("^Razón", termino), .(termino, m11 = media, s11 = de)], by = "termino")
dd_ <- dd_[, .(modelo = "2025 frente a 2011-2013 (diferencia de diferencias)", termino, rr = exp(m25 - m11),
               lo = exp(m25 - m11 - 1.96 * sqrt(s25^2 + s11^2)), hi = exp(m25 - m11 + 1.96 * sqrt(s25^2 + s11^2)),
               media = m25 - m11, de = sqrt(s25^2 + s11^2), casos = NA_integer_, filas = NA_integer_)]
CONJ <- rbind(CONJ, dd_)
fwrite(CONJ, "03_resultados/espacial_cpn_modelo_conjunto.csv")
cat("A1 lista: modelo conjunto del CPN\n")

# -----------------------------------------------------------------------------
# A2. Sensibilidades del CPN: previas, sin Antioquia, BN con efectos fijos de departamento
# -----------------------------------------------------------------------------
SENS_K <- c("2025 todas las edades", "2025 <1 año", "2025 10+ años (control negativo)", "2011-2013 <1 año")
sens_prior <- rbindlist(lapply(names(PRIORS)[-1], function(p) rbindlist(lapply(SENS_K, function(k) { s <- ESPEC[[k]]
  fit_bym(s[[1]], s[[2]], s[[3]], k, prior = PRIORS[[p]])$fe[, analisis := paste("Previa:", p)] }))))
sens_prior_conj <- rbindlist(lapply(names(PRIORS)[-1], function(p)
  fit_conjunto(2025, cv25, "z_cpn4_1924", "Conjunto 2025", prior = PRIORS[[p]])[grepl("^Razón|<2 m$|10\\+ a$|6-11 m$", termino)][, analisis := paste("Previa:", p)]))
sens_antq <- rbindlist(lapply(SENS_K, function(k) { s <- ESPEC[[k]]
  fit_bym(s[[1]], s[[2]], s[[3]], k, excluir_dp = "05")$fe[, analisis := "Sin Antioquia"] }))
nb_dep <- function(yrs, grupos, covs, lab) {
  d <- merge(cov, expected(yrs, grupos), by = "MPIO", all.x = TRUE)[!is.na(E) & E > 0]; d[is.na(O), O := 0L]
  m <- glm.nb(as.formula(paste("O ~", paste(covs, collapse = " + "), "+ factor(DP) + offset(log(E))")), data = d)
  b <- coef(summary(m)); b <- b[rownames(b) %in% covs, , drop = FALSE]
  data.table(modelo = lab, termino = rownames(b), rr = exp(b[, 1]), lo = exp(b[, 1] - 1.96 * b[, 2]), hi = exp(b[, 1] + 1.96 * b[, 2]),
             analisis = "Binomial negativa con efectos fijos de departamento")
}
sens_nb <- rbindlist(lapply(SENS_K, function(k) { s <- ESPEC[[k]]; nb_dep(s[[1]], s[[2]], s[[3]], k) }))
# Verosimilitud binomial negativa (el chequeo predictivo de 2025 mostró más ceros de los esperados)
sens_nbl <- rbindlist(lapply(SENS_K, function(k) { s <- ESPEC[[k]]
  r <- fit_bym(s[[1]], s[[2]], s[[3]], k, familia = "nbinomial")
  r$fe[, analisis := "BYM2 con verosimilitud binomial negativa"][, WAIC := r$waic] }))
# Modelo conjunto en BN con efectos fijos de departamento (contraste <2 m vs 6-11 m y vs 10+ a)
nb_conj <- function(yrs, covs, cpn, lab) {
  d <- merge(cov, expected_estratos(yrs), by = "MPIO")[!is.na(E) & E > 0]; d[, est := factor(est, levels = ESTR)]
  for (k in seq_along(ESTR)) d[, (paste0("b", k)) := as.integer(est == ESTR[k]) * get(cpn)]
  m <- glm.nb(as.formula(paste("O ~ 0 + est +", paste(c(paste0("b", 1:5), setdiff(covs, cpn)), collapse = " + "), "+ factor(DP) + offset(log(E))")), data = d)
  b <- coef(m); V <- vcov(m)
  cc <- list(`<2 m vs 6-11 m` = c(b1 = 1, b3 = -1), `<2 m vs 10+ a` = c(b1 = 1, b5 = -1), `6-11 m vs 10+ a` = c(b3 = 1, b5 = -1),
             `2-5 m vs 6-11 m` = c(b2 = 1, b3 = -1), `<2 m vs 1-9 a` = c(b1 = 1, b4 = -1))
  rbind(data.table(modelo = lab, termino = paste("CPN,", ESTR), rr = exp(b[paste0("b", 1:5)]),
                   lo = exp(b[paste0("b", 1:5)] - 1.96 * sqrt(diag(V)[paste0("b", 1:5)])), hi = exp(b[paste0("b", 1:5)] + 1.96 * sqrt(diag(V)[paste0("b", 1:5)]))),
        rbindlist(lapply(names(cc), function(k) { v <- cc[[k]]; e <- sum(v * b[names(v)]); se <- sqrt(drop(t(v) %*% V[names(v), names(v)] %*% v))
          data.table(modelo = lab, termino = paste("Razón de pendientes,", k), rr = exp(e), lo = exp(e - 1.96 * se), hi = exp(e + 1.96 * se)) })))[,
    analisis := "Binomial negativa con efectos fijos de departamento"]
}
sens_nb_conj <- rbind(nb_conj(2025, cv25, "z_cpn4_1924", "Conjunto 2025"), nb_conj(2011:2013, cv11, "z_cpn4_0810", "Conjunto 2011-2013"))
SENS <- rbind(sens_prior, sens_antq, sens_nb, sens_nbl, fill = TRUE)[grepl("cpn4", termino)]
SENS_CONJ <- rbind(sens_prior_conj, sens_nb_conj, fill = TRUE)
fwrite(SENS, "03_resultados/espacial_cpn_sensibilidad.csv"); fwrite(SENS_CONJ, "03_resultados/espacial_cpn_conjunto_sensibilidad.csv")
cat("A2 lista: sensibilidades del CPN\n")

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
cat("B lista: modelo de cambio\n")

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
# Proporción indígena (DANE 2025) de las unidades de cada conglomerado
pind <- cov[, .(MPIO, p_ind, pop25)]
clus_ind <- rbindlist(lapply(seq_along(cl_list), function(i) { ids <- mp[cl_list[[i]]$location.IDs.included]
  x <- pind[MPIO %in% ids]; data.table(cluster = i, unidades = paste(ids, collapse = " "),
                                       nombres = paste(gj$nombre[match(ids, gj$MPIO)], collapse = "; "),
                                       p_indigena = sum(x$p_ind * x$pop25, na.rm = TRUE) / sum(x$pop25, na.rm = TRUE)) }))
fwrite(clus_ind, "03_resultados/espacial_kulldorff_2025_composicion.csv")
cat("C lista: Moran y Kulldorff\n")

# =============================================================================
# D. Desigualdades según IPM (quintiles de unidades ponderados por población de 2018)
# =============================================================================
q <- merge(cov[, .(MPIO, ipm)], pm[area == "total" & anio == 2018, .(pop18 = sum(pop)), by = MPIO], by = "MPIO")[!is.na(ipm)]
setorder(q, ipm); q[, cum := cumsum(pop18) / sum(pop18)]
q[, quintil := pmin(5L, as.integer(ceiling(cum * 5)))]; q[, ridit := (cumsum(pop18) - pop18 / 2) / sum(pop18)]
cortes <- q[, .(unidades = .N, ipm_min = min(ipm), ipm_max = max(ipm), pob_2018 = sum(pop18)), by = quintil][order(quintil)]
cortes[, `:=`(prop_pob = pob_2018 / sum(pob_2018), ridit_medio = cumsum(pob_2018) / sum(pob_2018) - pob_2018 / sum(pob_2018) / 2)]
cortes <- rbind(cortes, data.table(quintil = NA_integer_, unidades = nrow(cov) - nrow(q), ipm_min = NA, ipm_max = NA, pob_2018 = NA, prop_pob = NA, ridit_medio = NA))
fwrite(cortes, "03_resultados/inequidades_ipm_cortes.csv")
ci_p <- function(x, den, k = 1) { if (is.na(den) || den <= 0) return(c(NA, NA)); poisson.test(x)$conf.int[1:2] / den * k }
ci_b <- function(x, n) if (n > 0) binom.test(x, n)$conf.int[1:2] else c(NA, NA)
inq <- rbindlist(lapply(list(`2011-2013` = 2011:2013, `2014-2019` = 2014:2019, `2025` = 2025), function(yrs) {
  e <- merge(q, expected(yrs), by = "MPIO", all.x = TRUE); e[is.na(O), O := 0L]
  i0 <- cs[anio %in% yrs & lactante == TRUE & !is.na(MPIO), .(lact = .N, muertes = sum(muerte)), by = MPIO]
  p0 <- pm[area == "total" & grupo == "0" & anio %in% yrs, .(pop0 = sum(pop)), by = MPIO]
  e <- merge(merge(e, i0, by = "MPIO", all.x = TRUE), p0, by = "MPIO", all.x = TRUE)
  for (v in c("lact", "muertes")) e[is.na(get(v)), (v) := 0L]
  qq <- e[, .(O = sum(O), E = sum(E), lact = sum(lact), muertes = sum(muertes), pop0 = sum(pop0, na.rm = TRUE)), by = quintil][order(quintil)]
  qq[, `:=`(sir = O / E, mort_inf_100k = muertes / pop0 * 1e5, letalidad = muertes / lact)]
  qq[, c("sir_lo", "sir_hi") := transpose(lapply(seq_len(.N), function(i) ci_p(O[i], E[i])))]
  qq[, c("mort_lo", "mort_hi") := transpose(lapply(seq_len(.N), function(i) ci_p(muertes[i], pop0[i], 1e5)))]
  qq[, c("let_lo", "let_hi") := transpose(lapply(seq_len(.N), function(i) ci_b(muertes[i], lact[i])))]
  # RII (incidencia estandarizada y mortalidad infantil) por regresión en el ridit municipal
  e2 <- e[E > 0]
  rii_inc <- glm.nb(O ~ ridit + offset(log(E)), data = e2)
  rii_mort <- glm(muertes ~ ridit + offset(log(pop0)), family = quasipoisson, data = e[pop0 > 0])
  f <- function(m) { b <- coef(summary(m))["ridit", ]; sprintf("%.2f (%.2f–%.2f)", exp(b[1]), exp(b[1] - 1.96 * b[2]), exp(b[1] + 1.96 * b[2])) }
  # SII de la mortalidad infantil (por 100.000 lactante-años): pendiente ponderada de las tasas por quintil en el
  # ridit medio del quintil; incertidumbre por simulación de las tasas (gamma de Jeffreys, 10.000 extracciones)
  rm_ <- merge(qq[, .(quintil, muertes, pop0)], cortes[!is.na(quintil), .(quintil, w = prop_pob, r = ridit_medio)], by = "quintil")
  sl <- function(y) { X <- cbind(1, rm_$r); W <- diag(rm_$w); drop(solve(t(X) %*% W %*% X, t(X) %*% W %*% y))[2] }
  sims <- replicate(10000, sl(rgamma(nrow(rm_), shape = rm_$muertes + 0.5, rate = rm_$pop0) * 1e5))
  sii <- sl(rm_$muertes / rm_$pop0 * 1e5)
  qq[, `:=`(RII_incidencia = f(rii_inc), RII_mortalidad_infantil = f(rii_mort),
            SII_mortalidad_infantil = sprintf("%.2f (%.2f–%.2f)", sii, quantile(sims, .025), quantile(sims, .975)))]
  qq
}), idcol = "periodo")
fwrite(inq, "03_resultados/inequidades_ipm_quintiles.csv")

saveRDS(list(MUN = MUN, chg = chg, clus = clus, kd_ids = lapply(cl_list, function(cl) mp[cl$location.IDs.included]),
             geo = gj[, c("MPIO", "nombre", "dpto", "area_km2")], conj = CONJ),
        "01_datos/procesados/espacial_resultados.rds")

options(width = 220)
print(falt[, .(variable, faltantes)])
print(cor_tab)
print(FE[termino != "(Intercept)", .(modelo, termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(CONJ[, .(modelo, termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(SENS[, .(analisis, modelo, termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(SENS_CONJ[, .(analisis, modelo, termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(rbindlist(lapply(mods, `[[`, "diag"))); print(rbindlist(lapply(mods, `[[`, "ppc")))
print(feC[, .(termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(mor); print(clus[, .(cluster, n_municipios, observados, esperados = round(esperados, 1), smr = round(smr, 2), p, municipios)])
print(clus_ind)
print(clus20[, .(cluster, n_municipios, observados, esperados = round(esperados, 1), smr = round(smr, 2), p, municipios)])
print(cortes)
print(inq[, .(periodo, quintil, O, sir = round(sir, 2), mort_inf_100k = round(mort_inf_100k, 2), mort_lo = round(mort_lo, 2), mort_hi = round(mort_hi, 2),
              letalidad = round(letalidad, 3), RII_incidencia, RII_mortalidad_infantil, SII_mortalidad_infantil)])
for (m in mods) cat(m$mun$modelo[1], "WAIC:", round(m$waic, 1), " municipios P(RR>1)>0.95:", m$mun[p_exc > 0.95, .N], "\n")
