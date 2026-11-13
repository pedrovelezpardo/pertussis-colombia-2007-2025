# =============================================================================
# 05_modelos.R
# A. ¿Exceso de riesgo en cohortes nacidas en la pandemia? (2025 vs 2015-2019)
# B. Determinantes de muerte en lactantes con tos ferina (2011-2025)
# C. Modelo espacial bayesiano BYM2 (INLA) por municipio: epidemia 2025 y 2011-2013
# Ejecutar desde la raíz: Rscript 02_scripts/05_modelos.R
# =============================================================================
suppressMessages({
  library(data.table); library(MASS); library(splines); library(lme4)
  library(sf); library(spdep); library(INLA); library(readxl)
})
INLA::inla.setOption(num.threads = "4:1")   # deja núcleos libres para el usuario
set.seed(2026)
cs  <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
lex <- readRDS("01_datos/procesados/lexis_edad_anio.rds")
res <- list()

# =============================================================================
# A. Cohortes pandémicas: desviación respecto a la curva suave de edad
#    log E[c_2025(a)] = log pop_2025(a) + log tasa_base(a) + s(a) + b*pandemia(cohorte)
# =============================================================================
b0 <- lex[anio %in% 2015:2019, .(c0 = sum(casos), p0 = sum(pop)), by = edad]
a  <- merge(lex[anio == 2025, .(edad, cohorte, c = casos, p = pop)], b0, by = "edad")[edad >= 1]
a[, log_base := log((c0 + 0.5) / p0)]
cohort_defs <- list(`2019-2021` = 2019:2021, `2020-2021` = 2020:2021, `2020-2022` = 2020:2022)
coh <- rbindlist(lapply(names(cohort_defs), function(k) {
  a[, pand := as.integer(cohorte %in% cohort_defs[[k]])]
  m <- glm.nb(c ~ ns(edad, df = 3) + pand + offset(log(p) + log_base), data = a)
  est <- coef(summary(m))["pand", ]
  data.table(definicion = k, irr = exp(est[1]), lo = exp(est[1] - 1.96 * est[2]),
             hi = exp(est[1] + 1.96 * est[2]), p = est[4], theta = m$theta)
}))
fwrite(coh, "03_resultados/modelo_A_cohortes_pandemicas.csv")
res$A <- coh

# =============================================================================
# B. Muerte en lactantes (2011-2025), regresión logística mixta (intercepto por depto.)
# =============================================================================
inf <- cs[lactante == TRUE & anio >= 2011 & !is.na(grupo_edad)]
inf[, `:=`(
  grupo_edad = droplevels(factor(grupo_edad, levels = c("6-11 m", "4-5 m", "2-3 m", "<2 m"))),
  periodo4 = factor(fcase(anio <= 2013, "2011-2013", anio <= 2019, "2014-2019",
                          anio <= 2024, "2020-2024", anio == 2025, "2025"),
                    levels = c("2011-2013", "2014-2019", "2020-2024", "2025")),
  etnia3 = factor(fifelse(etnia == "Rrom", "Otro/ninguno", etnia),
                  levels = c("Otro/ninguno", "Afrodescendiente", "Indígena")),
  regimen4 = factor(fcase(regimen == "Contributivo", "Contributivo", regimen == "Subsidiado", "Subsidiado",
                          regimen == "No asegurado", "No asegurado", default = "Otro"),
                    levels = c("Contributivo", "Subsidiado", "No asegurado", "Otro")),
  rural = as.integer(area == "Rural"),
  mujer = as.integer(sexo == "Mujer"),
  lab = as.integer(confirmacion == "Laboratorio"),
  consulta_tardia = as.integer(ret_consulta > 7),
  dp = factor(dp_res))]
infm <- inf[complete.cases(inf[, .(muerte, grupo_edad, periodo4, etnia3, regimen4, rural, mujer, lab, dp)])]
mB <- glmer(muerte ~ grupo_edad + periodo4 + etnia3 + regimen4 + rural + mujer + lab + (1 | dp),
            data = infm, family = binomial, control = glmerControl(optimizer = "bobyqa"))
cf <- summary(mB)$coefficients
orB <- data.table(termino = rownames(cf), or = exp(cf[, 1]), lo = exp(cf[, 1] - 1.96 * cf[, 2]),
                  hi = exp(cf[, 1] + 1.96 * cf[, 2]), p = cf[, 4])[termino != "(Intercept)"]
orB[, n_lactantes := nrow(infm)][, n_muertes := sum(infm$muerte)]
fwrite(orB, "03_resultados/modelo_B_muerte_lactantes_OR.csv")
res$B <- orB
# Letalidad cruda por etnia y periodo (lactantes)
letal <- inf[, .(lactantes = .N, muertes = sum(muerte), letalidad = sum(muerte) / .N), by = .(periodo4, etnia3)][order(periodo4, etnia3)]
fwrite(letal, "03_resultados/letalidad_lactantes_etnia_periodo.csv")
res$letal <- letal

# =============================================================================
# C. Modelo espacial BYM2 por municipio
# =============================================================================
sf_use_s2(FALSE)
gj <- st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE)
gj <- st_make_valid(st_transform(gj, 9377))   # MAGNA-SIRGAS / Origen Nacional (planar)
gj$MPIO <- gj$MPIO_CDPMP
gj$area_km2 <- as.numeric(gj$AREA) / 1e6

pm <- readRDS("01_datos/procesados/pob_mun_grupos.rds")
# Municipios creados después de 2018 se asignan al municipio de origen en el MGN 2018
recode_mpio <- function(x) fcase(x == "94663", "50325",  # Barrancominas -> Mapiripán
                                 x == "27086", "05480",  # Belén de Bajirá -> Mutatá
                                 default = x)
pm[, MPIO := recode_mpio(MPIO)]
pm <- pm[, .(pop = sum(pop)), by = .(MPIO, anio, area, grupo)]
cs[, mpio_mgn := recode_mpio(mpio_res)]

# Tasas nacionales por grupo para estandarización indirecta
std_exp <- function(yrs) {
  pg <- pm[area == "total" & anio %in% yrs, .(pop = sum(pop)), by = .(MPIO, grupo)]
  cg <- cs[anio %in% yrs & !is.na(grupo_pob), .(casos = .N), by = .(grupo = grupo_pob)]
  nat <- merge(pg[, .(pop = sum(pop)), by = grupo], cg, by = "grupo", all.x = TRUE)
  nat[is.na(casos), casos := 0][, tasa := casos / pop]
  e <- merge(pg, nat[, .(grupo, tasa)], by = "grupo")[, .(E = sum(pop * tasa), pop = sum(pop) / length(yrs)), by = MPIO]
  o <- cs[anio %in% yrs & !is.na(mpio_mgn), .(O = .N), by = .(MPIO = mpio_mgn)]
  x <- merge(e, o, by = "MPIO", all.x = TRUE); x[is.na(O), O := 0L]; x
}

# Covariables municipales
ipm <- as.data.table(read_excel("01_datos/raw/dane_ipm/anexo-censal-pobreza-municipal-2018.xlsx",
                                sheet = "2_IPM Mpio y Dpt", col_types = "text"))
setnames(ipm, 1:3, c("MPIO", "nombre", "ipm"))
ipm <- ipm[grepl("^[0-9]{5}$", MPIO), .(MPIO, ipm = as.numeric(ipm))]
et <- as.data.table(read_excel("01_datos/raw/dane_etnia/anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun.xlsx",
                               col_names = FALSE, col_types = "text", skip = 7))
setnames(et, c("dp", "dpnom", "MPIO", "mpnom", "anio", "area", "total", "indigena", "rrom", "raizal",
               "palenquero", "afro", "ninguno"))
et <- et[area == "Total" & anio == "2025", .(MPIO = recode_mpio(MPIO), total = as.numeric(total),
                                             indigena = as.numeric(indigena),
                                             afro = as.numeric(afro) + as.numeric(raizal) + as.numeric(palenquero))]
et <- et[, .(p_indigena = sum(indigena) / sum(total), p_afro = sum(afro) / sum(total)), by = MPIO]
rur <- pm[anio == 2025, .(p_rural = sum(pop[area == "rural"]) / sum(pop[area == "total"])), by = MPIO]
cov <- Reduce(function(x, y) merge(x, y, by = "MPIO", all.x = TRUE), list(ipm, et, rur))
cov[, MPIO := recode_mpio(MPIO)]
cov <- cov[, lapply(.SD, mean, na.rm = TRUE), by = MPIO]

# Cobertura departamental de pentavalente (3a dosis <1 año), promedio 2019-2022
cob <- fread("01_datos/raw/minsalud_coberturas/coberturas_departamento_6i25-2hdt.csv", encoding = "UTF-8")
cob <- cob[grepl("PENTA3", biol_gico)]
cob[, v := as.numeric(cobertura_de_vacunaci_n)][, v := fifelse(v < 2, v * 100, v / 100)]
cob <- cob[, .(penta3 = mean(pmin(v, 100))), by = .(DP = sprintf("%02d", as.integer(coddepto)))]

# Grafo de vecindad (contigüidad tipo reina); islas conectadas al vecino más cercano
gj <- gj[order(gj$MPIO), ]
nb <- poly2nb(gj, queen = TRUE, snap = 1e-4)
isl <- which(card(nb) == 0)
if (length(isl)) {
  cent <- st_coordinates(st_centroid(st_geometry(gj)))
  for (i in isl) { dd <- sqrt(colSums((t(cent) - cent[i, ])^2)); dd[i] <- Inf; j <- which.min(dd)
                   nb[[i]] <- as.integer(j); nb[[j]] <- sort(unique(c(nb[[j]], as.integer(i)))) }
}
nb2INLA("01_datos/procesados/grafo_municipios.adj", nb)
g <- inla.read.graph("01_datos/procesados/grafo_municipios.adj")

fit_bym2 <- function(yrs, lab) {
  x <- std_exp(yrs)
  m <- data.table(MPIO = gj$MPIO, DP = substr(gj$MPIO, 1, 2), area_km2 = gj$area_km2, idx = seq_len(nrow(gj)))
  m <- merge(m, x, by = "MPIO", all.x = TRUE)
  m <- merge(m, cov, by = "MPIO", all.x = TRUE)
  m <- merge(m, cob, by = "DP", all.x = TRUE)
  setorder(m, idx)
  m[is.na(O), O := 0L]
  m[, log_dens := log(pop / area_km2)]
  zs <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
  for (v in c("ipm", "p_rural", "p_indigena", "p_afro", "log_dens", "penta3"))
    m[, (paste0("z_", v)) := { z <- zs(get(v)); z[is.na(z)] <- 0; z }]
  m <- m[!is.na(E) & E > 0]
  f <- O ~ 1 + z_ipm + z_p_rural + z_p_indigena + z_p_afro + z_log_dens + z_penta3 +
    f(idx, model = "bym2", graph = g, scale.model = TRUE, constr = TRUE,
      hyper = list(phi = list(prior = "pc", param = c(0.5, 2 / 3)),
                   prec = list(prior = "pc.prec", param = c(1, 0.01))))
  fit <- inla(f, family = "poisson", data = as.data.frame(m), E = E,
              control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, return.marginals.predictor = TRUE),
              verbose = FALSE)
  fe <- as.data.table(fit$summary.fixed, keep.rownames = "termino")
  fe <- fe[, .(termino, rr = exp(mean), lo = exp(`0.025quant`), hi = exp(`0.975quant`), epidemia = lab)]
  # RR municipal suavizado y probabilidad de exceder 1
  rr <- fit$summary.fitted.values$mean
  pex <- vapply(fit$marginals.fitted.values, function(mg) 1 - inla.pmarginal(1, mg), numeric(1))
  out <- m[, .(MPIO, DP, O, E, pop, sir = O / E)][, `:=`(rr_bym2 = rr, p_exc = pex, epidemia = lab)]
  hyp <- as.data.table(fit$summary.hyperpar, keep.rownames = "param")[, epidemia := lab]
  list(fe = fe, mun = out, hyp = hyp, waic = fit$waic$waic)
}
sp25 <- fit_bym2(2025, "2025")
sp11 <- fit_bym2(2011:2013, "2011-2013")
fwrite(rbind(sp25$fe, sp11$fe), "03_resultados/modelo_C_bym2_efectos_fijos.csv")
fwrite(rbind(sp25$hyp, sp11$hyp), "03_resultados/modelo_C_bym2_hiperparametros.csv")
mun <- rbind(sp25$mun, sp11$mun)
fwrite(mun, "03_resultados/modelo_C_bym2_rr_municipal.csv")
saveRDS(list(mun = mun, geo = gj[, c("MPIO", "MPIO_CNMBR", "DPTO_CCDGO")]), "01_datos/procesados/bym2_resultados.rds")
res$C <- rbind(sp25$fe, sp11$fe)
res$C_hot <- sp25$mun[p_exc > 0.95][order(-rr_bym2)][1:min(.N, 25)]
saveRDS(res, "01_datos/procesados/modelos_resumen.rds")

# --- Resumen -------------------------------------------------------------------------------
options(width = 180)
cat("\n== A. Cohortes pandémicas (IRR vs curva de edad)\n"); print(coh)
cat("\n== B. OR muerte en lactantes (n =", nrow(infm), "; muertes =", sum(infm$muerte), ")\n"); print(orB[, .(termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
cat("\nLetalidad cruda lactantes por etnia y periodo\n"); print(letal)
cat("\n== C. BYM2 efectos fijos (RR por 1 DE)\n"); print(res$C[, .(epidemia, termino, rr = round(rr, 2), lo = round(lo, 2), hi = round(hi, 2))])
cat("\nMunicipios con P(RR>1) > 0.95 en 2025:", sp25$mun[p_exc > 0.95, .N], "\n")
print(res$C_hot[, .(MPIO, O, E = round(E, 1), sir = round(sir, 1), rr = round(rr_bym2, 2), p_exc = round(p_exc, 3))])
