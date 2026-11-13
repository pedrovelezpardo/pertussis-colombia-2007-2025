# =============================================================================
# 11_cohortes_letalidad.R  (versión del manuscrito v4, 12-11-2026)
# A. Cohortes nacidas en la pandemia: desviación respecto a la curva suave de edad
#    (2025 vs línea base), con sensibilidad a grados de libertad y línea base.
# B. Modelo edad-periodo-cohorte bayesiano (INLA, RW2), edades 0-19, 2007-2025.
#    v4: contrastes identificables (invariantes a la tendencia lineal) en lugar del
#    contraste de la v3, previas alternativas y chequeo predictivo posterior.
# C. Muerte en lactantes (2011-2025): regresión logística mixta.
#    v4: el modelo principal no incluye el tipo de confirmación, que depende del
#    desenlace (estaba en el plan inicial y pasa a sensibilidad); el régimen
#    indeterminado se trata como faltante; OR crudos; muertes por categoría;
#    interacción periodo x indígena con prueba de razón de verosimilitud; control
#    prenatal del periodo de cada caso; diagnóstico con residuos simulados.
# =============================================================================
suppressMessages({ library(data.table); library(MASS); library(splines); library(lme4)
                   library(logistf); library(INLA); library(readxl); library(DHARMa) })
INLA::inla.setOption(num.threads = "4:1")
set.seed(20253)
cs  <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
lex <- readRDS("01_datos/procesados/lexis_edad_anio.rds")

# =============================================================================
# A. Cohortes pandémicas
# =============================================================================
cohA <- rbindlist(lapply(list(`2015-2019` = 2015:2019, `2014-2019` = 2014:2019), function(base) {
  b0 <- lex[anio %in% base, .(c0 = sum(casos), p0 = sum(pop)), by = edad]
  a <- merge(lex[anio == 2025, .(edad, cohorte, c = casos, p = pop)], b0, by = "edad")[edad >= 1]
  a[, log_base := log((c0 + 0.5) / p0)]
  rbindlist(lapply(c(3, 4, 5), function(df) rbindlist(lapply(list(`2019-2021` = 2019:2021, `2020-2021` = 2020:2021, `2020-2022` = 2020:2022), function(cc) {
    a[, pand := as.integer(cohorte %in% cc)]
    m <- glm.nb(c ~ ns(edad, df = df) + pand + offset(log(p) + log_base), data = a)
    e <- coef(summary(m))["pand", ]
    data.table(cohortes = paste(range(cc), collapse = "-"), df = df, irr = exp(e[1]), lo = exp(e[1] - 1.96 * e[2]),
               hi = exp(e[1] + 1.96 * e[2]), p = e[4])
  }))))[, base := paste(range(base), collapse = "-")]
}))
fwrite(cohA, "03_resultados/cohortes_pandemicas_sensibilidad.csv")

# =============================================================================
# B. APC bayesiano (INLA)
#    y ~ Poisson(pop * exp(mu + f_age + f_period + f_cohort + e)), RW2 + iid
#    Solo son estimables las combinaciones de efectos de cohorte con pesos que suman
#    cero y son ortogonales a una tendencia lineal (Holford, 1983). Se reportan:
#    D = media de f(2019..2022) menos la recta de f(2014..2018) extrapolada al año medio
#        de 2019-2022 (exceso de las cohortes pandémicas frente a la tendencia previa);
#    S = [f(2022) - f(2019)] - [f(2018) - f(2015)] (cambio de pendiente).
# =============================================================================
ap <- copy(lex)[, `:=`(i_age = edad + 1, i_per = anio - 2006, i_coh = cohorte - min(cohorte) + 1, id = .I)]
coh_years <- sort(unique(ap$cohorte))
pesos_D <- function() {
  B <- 2014:2018; P <- 2019:2022
  w <- setNames(numeric(length(coh_years)), coh_years)
  w[as.character(P)] <- 1 / length(P)
  X <- cbind(1, B); H <- c(1, mean(P)) %*% solve(crossprod(X), t(X))   # extrapolación lineal desde B
  w[as.character(B)] <- w[as.character(B)] - drop(H)
  w
}
wD <- pesos_D()
wS <- setNames(numeric(length(coh_years)), coh_years); wS[c("2022", "2019", "2018", "2015")] <- c(1, -1, -1, 1)
stopifnot(abs(sum(wD)) < 1e-10, abs(sum(wD * coh_years)) < 1e-8, abs(sum(wS)) < 1e-10, abs(sum(wS * coh_years)) < 1e-8)
fit_apc <- function(U) {
  pc <- list(prec = list(prior = "pc.prec", param = c(U, 0.01)))
  f <- casos ~ 1 + f(i_age, model = "rw2", hyper = pc, scale.model = TRUE) +
    f(i_per, model = "rw2", hyper = pc, scale.model = TRUE) +
    f(i_coh, model = "rw2", hyper = pc, scale.model = TRUE) + f(id, model = "iid", hyper = pc)
  inla(f, family = "poisson", data = as.data.frame(ap), E = pop,
       control.compute = list(dic = TRUE, waic = TRUE, config = TRUE), verbose = FALSE)
}
resumen_apc <- function(fit, lab, nsamp = 4000) {
  # Semilla fija (INLA exige un solo hilo para muestrear con semilla): resultados reproducibles
  smp <- inla.posterior.sample(nsamp, fit, seed = 20253L, num.threads = "1:1")
  ci <- grep("^i_coh:", rownames(smp[[1]]$latent))
  co <- sapply(smp, function(s) s$latent[ci, 1])
  D <- colSums(co * wD); S <- colSums(co * wS)
  k <- seq_len(nrow(co)); X <- cbind(1, k)
  detr <- co - X %*% solve(crossprod(X), crossprod(X, co))
  q <- function(x) c(est = exp(median(x)), lo = exp(quantile(x, .025)), hi = exp(quantile(x, .975)), p_pos = mean(x > 0))
  list(con = rbind(data.table(previa = lab, contraste = "Cohortes 2019-2022 frente a la tendencia lineal de 2014-2018 (exp D)", t(q(D))),
                   data.table(previa = lab, contraste = "Cambio de pendiente [f(2022)-f(2019)]-[f(2018)-f(2015)] (exp S)", t(q(S)))),
       coh = data.table(cohorte = coh_years, dev = apply(detr, 1, median), lo = apply(detr, 1, quantile, .025),
                        hi = apply(detr, 1, quantile, .975), p_pos = rowMeans(detr > 0)),
       smp = smp)
}
apc <- fit_apc(1)
r_main <- resumen_apc(apc, "PC(1, 0.01)")
r_alt <- lapply(c(0.5, 2), function(U) resumen_apc(fit_apc(U), sprintf("PC(%s, 0.01)", U))$con)
apc_con <- rbind(r_main$con, rbindlist(r_alt))
setnames(apc_con, c("lo.2.5%", "hi.97.5%"), c("lo", "hi"), skip_absent = TRUE)
apc_coh <- r_main$coh
# Chequeo predictivo posterior (Pearson por extracción, ceros y máximo)
pred <- grep("^Predictor:", rownames(r_main$smp[[1]]$latent))
eta <- sapply(r_main$smp[seq_len(500)], function(s) s$latent[pred, 1])
mu <- ap$pop * exp(eta); yrep <- matrix(rpois(length(mu), mu), nrow = nrow(mu))
pe_obs <- colSums((ap$casos - mu)^2 / mu); pe_rep <- colSums((yrep - mu)^2 / mu)
apc_ppc <- data.table(estadistico = c("ceros", "maximo", "pearson"),
                      observado = c(sum(ap$casos == 0), max(ap$casos), median(pe_obs)),
                      replicado_mediana = c(median(colSums(yrep == 0)), median(apply(yrep, 2, max)), median(pe_rep)),
                      p_predictivo = c(mean(colSums(yrep == 0) >= sum(ap$casos == 0)), mean(apply(yrep, 2, max) >= max(ap$casos)), mean(pe_rep >= pe_obs)))
fwrite(apc_coh, "03_resultados/apc_cohortes_curvatura.csv"); fwrite(apc_con, "03_resultados/apc_contraste_pandemia.csv")
fwrite(apc_ppc, "03_resultados/apc_chequeo_predictivo.csv")
fwrite(data.table(cohorte = coh_years, peso_D = wD, peso_S = wS), "03_resultados/apc_pesos_contrastes.csv")
saveRDS(list(coh = apc_coh, age = apc$summary.random$i_age, per = apc$summary.random$i_per, con = apc_con),
        "01_datos/procesados/apc_inla.rds")
cat("B listo: APC\n")

# =============================================================================
# C. Muerte en lactantes
# =============================================================================
covm <- readRDS("01_datos/procesados/covariables_municipales_terridata.rds")
inf0 <- cs[lactante == TRUE & anio >= 2011 & !is.na(grupo_edad)]
inf0[, `:=`(
  edad_g = factor(fcase(edad_dias < 42, "<6 semanas", edad_dias < 61, "6-8 semanas", edad_dias < 122, "2-3 meses",
                        edad_dias < 183, "4-5 meses", default = "6-11 meses"),
                  levels = c("6-11 meses", "4-5 meses", "2-3 meses", "6-8 semanas", "<6 semanas")),
  periodo4 = factor(fcase(anio <= 2013, "2011-2013", anio <= 2019, "2014-2019", anio <= 2024, "2020-2024", default = "2025"),
                    levels = c("2011-2013", "2014-2019", "2020-2024", "2025")),
  etnia3 = factor(fifelse(etnia == "Rrom", "Otro/ninguno", etnia), levels = c("Otro/ninguno", "Afrodescendiente", "Indígena")),
  regimen4 = factor(fcase(regimen == "Contributivo", "Contributivo", regimen == "Subsidiado", "Subsidiado",
                          regimen == "No asegurado", "No asegurado", regimen == "Especial/Excepción", "Especial",
                          default = NA_character_),
                    levels = c("Contributivo", "Subsidiado", "No asegurado", "Especial")),
  rural = as.integer(area == "Rural"), mujer = as.integer(sexo == "Mujer"),
  lab = as.integer(confirmacion == "Laboratorio"), migrante = as.integer(migrante),
  dp = factor(dp_res), MPIO = mpio_res)]
# Control prenatal (>=4 controles) del periodo de cada caso; 2025 usa 2020-2024
inf0 <- merge(inf0, covm[, .(MPIO, cpn4_1113, cpn4_1419, cpn4_2024, cpn4_1924)], by = "MPIO", all.x = TRUE)
inf0[, cpn4_per := fcase(periodo4 == "2011-2013", cpn4_1113, periodo4 == "2014-2019", cpn4_1419,
                         periodo4 %in% c("2020-2024", "2025"), cpn4_2024)]
inf0[, `:=`(z_cpn4 = (cpn4_1924 - mean(cpn4_1924, na.rm = TRUE)) / sd(cpn4_1924, na.rm = TRUE),
            z_cpn4_per = (cpn4_per - mean(cpn4_per, na.rm = TRUE)) / sd(cpn4_per, na.rm = TRUE))]
# Flujo de exclusiones (análisis de casos completos)
flujo <- data.table(paso = c("Lactantes notificados 2011-2025", "Sin departamento de residencia", "Régimen de salud indeterminado",
                             "Analizados (casos completos)"),
                    n = c(nrow(inf0), inf0[is.na(dp), .N], inf0[!is.na(dp) & is.na(regimen4), .N],
                          inf0[!is.na(dp) & !is.na(regimen4), .N]),
                    muertes = c(sum(inf0$muerte), inf0[is.na(dp), sum(muerte)], inf0[!is.na(dp) & is.na(regimen4), sum(muerte)],
                                inf0[!is.na(dp) & !is.na(regimen4), sum(muerte)]))
fwrite(flujo, "03_resultados/letalidad_flujo_exclusiones.csv")
cc <- inf0[complete.cases(inf0[, .(muerte, edad_g, periodo4, etnia3, regimen4, rural, mujer, migrante, dp)])]

or_tab <- function(m, lab) { cf <- summary(m)$coefficients
  data.table(modelo = lab, termino = rownames(cf), or = exp(cf[, 1]), lo = exp(cf[, 1] - 1.96 * cf[, 2]),
             hi = exp(cf[, 1] + 1.96 * cf[, 2]), p = cf[, 4])[termino != "(Intercept)"] }
ctl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
f_main <- muerte ~ edad_g + periodo4 + etnia3 + regimen4 + rural + mujer + migrante + (1 | dp)
mP   <- glmer(f_main, data = cc, family = binomial, control = ctl)
mLab <- glmer(update(f_main, . ~ . + lab), data = cc, family = binomial, control = ctl)
ccL  <- cc[lab == 1]
mSoloLab <- glmer(f_main, data = ccL, family = binomial, control = ctl)
mMun <- glmer(update(f_main, . ~ . + (1 | MPIO)), data = cc, family = binomial, control = ctl)
ccA  <- cc[!is.na(z_cpn4_per)]
mA0  <- glmer(f_main, data = ccA, family = binomial, control = ctl)
mA1  <- update(mA0, . ~ . + z_cpn4_per)
ccA2 <- cc[!is.na(z_cpn4)]
mA2  <- glmer(update(f_main, . ~ . + z_cpn4), data = ccA2, family = binomial, control = ctl)
# Interacción con 2011-2013 como referencia: razón del OR indígena en cada periodo frente a 2011-2013
cc[, indig := as.integer(etnia3 == "Indígena")]
cc[, `:=`(ind_1419 = indig * (periodo4 == "2014-2019"), ind_2024 = indig * (periodo4 == "2020-2024"), ind_25 = indig * (periodo4 == "2025"))]
mInt <- glmer(update(f_main, . ~ . + ind_1419 + ind_2024 + ind_25), data = cc, family = binomial, control = ctl)
lrt_int <- anova(mP, mInt)
fir <- logistf(muerte ~ edad_g + periodo4 + etnia3 + regimen4 + rural + mujer + migrante, data = as.data.frame(cc))
fir_tab <- data.table(modelo = "Firth (sin efecto aleatorio)", termino = names(coef(fir)), or = exp(coef(fir)),
                      lo = exp(fir$ci.lower), hi = exp(fir$ci.upper), p = fir$prob)[termino != "(Intercept)"]
orB <- rbind(or_tab(mP, "Principal (sin tipo de confirmación)"),
             or_tab(mLab, "Con tipo de confirmación (plan inicial)"),
             or_tab(mSoloLab, "Solo confirmados por laboratorio"),
             or_tab(mMun, "Efectos aleatorios de departamento y municipio"),
             or_tab(mA0, "Submuestra con control prenatal del periodo"),
             or_tab(mA1, "Submuestra + control prenatal del periodo (municipal, DE)"),
             or_tab(mA2, "Control prenatal 2019-2024 (especificación de la v3)"),
             or_tab(mInt, "Con interacción periodo x indígena"), fir_tab)
nmuestra <- list(`Principal (sin tipo de confirmación)` = cc, `Con tipo de confirmación (plan inicial)` = cc,
                 `Solo confirmados por laboratorio` = ccL, `Efectos aleatorios de departamento y municipio` = cc,
                 `Submuestra con control prenatal del periodo` = ccA, `Submuestra + control prenatal del periodo (municipal, DE)` = ccA,
                 `Control prenatal 2019-2024 (especificación de la v3)` = ccA2, `Con interacción periodo x indígena` = cc,
                 `Firth (sin efecto aleatorio)` = cc)
orB[, `:=`(n = vapply(modelo, function(k) nrow(nmuestra[[k]]), 1L), muertes = vapply(modelo, function(k) sum(nmuestra[[k]]$muerte), 1L))]
fwrite(orB, "03_resultados/letalidad_modelos_OR.csv")
fwrite(data.table(prueba = "Interacción periodo x indígena (razón de verosimilitud)", chi2 = lrt_int$Chisq[2], gl = lrt_int$Df[2],
                  p = lrt_int$`Pr(>Chisq)`[2]), "03_resultados/letalidad_interaccion_lrt.csv")

# OR crudos (regresión logística simple, misma muestra) y muertes por categoría
vars <- c("edad_g", "periodo4", "etnia3", "regimen4", "rural", "mujer", "migrante")
crudo <- rbindlist(lapply(vars, function(v) { m <- glm(as.formula(paste("muerte ~", v)), data = cc, family = binomial)
  b <- coef(summary(m))[-1, , drop = FALSE]
  data.table(termino = rownames(b), or_crudo = exp(b[, 1]), lo = exp(b[, 1] - 1.96 * b[, 2]), hi = exp(b[, 1] + 1.96 * b[, 2])) }))
cat_n <- rbindlist(lapply(vars, function(v) cc[, .(lactantes = .N, muertes = sum(muerte)), by = .(nivel = as.character(get(v)))][, variable := v]))
cat_n[, termino := fifelse(variable %in% c("rural", "mujer", "migrante"), fifelse(nivel == "1", variable, paste0(variable, "_ref")), paste0(variable, nivel))]
fwrite(crudo, "03_resultados/letalidad_OR_crudos.csv"); fwrite(cat_n[order(variable, nivel)], "03_resultados/letalidad_muertes_por_categoria.csv")

# Diagnóstico del modelo principal (residuos simulados) y correlación intraclase
simP <- simulateResiduals(mP, n = 1000, plot = FALSE)
icc <- as.numeric(VarCorr(mP)$dp) / (as.numeric(VarCorr(mP)$dp) + pi^2 / 3)
diagP <- data.table(modelo = "Principal (sin tipo de confirmación)", n = nrow(cc), muertes = sum(cc$muerte), icc_departamento = icc,
                    p_uniformidad_KS = testUniformity(simP, plot = FALSE)$p.value, p_dispersion = testDispersion(simP, plot = FALSE)$p.value,
                    auc = { p <- fitted(mP); y <- cc$muerte; r <- rank(p); (sum(r[y == 1]) - sum(y == 1) * (sum(y == 1) + 1) / 2) / (sum(y == 1) * sum(y == 0)) })
fwrite(diagP, "03_resultados/letalidad_diagnostico.csv")

# Confirmación por laboratorio entre lactantes que murieron y que sobrevivieron
labm <- inf0[, .(lactantes = .N, laboratorio = sum(lab)), by = .(periodo4, murio = muerte)][order(periodo4, murio)][, pct_laboratorio := 100 * laboratorio / lactantes]
fwrite(labm, "03_resultados/letalidad_laboratorio_muertes.csv")

# Letalidad cruda con IC exacto (Clopper-Pearson) por etnia, periodo y edad
cp <- function(x, n) { b <- binom.test(x, n); sprintf("%.1f%% (%.1f–%.1f)", 100 * x / n, 100 * b$conf.int[1], 100 * b$conf.int[2]) }
let <- inf0[, .(lactantes = .N, muertes = sum(muerte)), by = .(periodo4, etnia3)][order(periodo4, etnia3)]
let[, letalidad := mapply(cp, muertes, lactantes)]
let2 <- inf0[, .(lactantes = .N, muertes = sum(muerte)), by = .(periodo4, grupo = fifelse(etnia3 == "Indígena", "Indígena", "No indígena"))][order(periodo4, grupo)]
let2[, letalidad := mapply(cp, muertes, lactantes)]
fwrite(let, "03_resultados/letalidad_cruda_etnia_periodo.csv"); fwrite(let2, "03_resultados/letalidad_cruda_indigena_periodo.csv")
# Tendencia de la letalidad en indígenas (2011-2025)
tend_ind <- glm(muerte ~ as.numeric(periodo4), family = binomial, data = inf0[etnia3 == "Indígena"])
fwrite(data.table(prueba = "Tendencia de la letalidad indígena por periodo (logística, periodo 1-4)",
                  or_por_periodo = exp(coef(tend_ind)[2]), p = coef(summary(tend_ind))[2, 4]),
       "03_resultados/letalidad_tendencia_indigena.csv")

# Mortalidad poblacional por etnia (aproximada): lactantes indígenas = sum_m pob0_m x %indígena_m
et <- as.data.table(read_excel("01_datos/raw/dane_etnia/anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun.xlsx",
                               col_names = FALSE, col_types = "text", skip = 7))
setnames(et, c("dp", "dpnom", "MPIO", "mpnom", "anio", "area", "total", "indigena", "rrom", "raizal", "palenquero", "afro", "ninguno"))
et <- et[area == "Total", .(MPIO, anio = as.integer(anio), p_ind = as.numeric(indigena) / as.numeric(total))]
pm <- readRDS("01_datos/procesados/pob_mun_grupos.rds")[area == "total" & grupo == "0", .(MPIO, anio, pop0 = pop)]
pe <- merge(pm[anio %in% 2011:2025], et[, .(MPIO, anio_et = anio, p_ind)], by = "MPIO", allow.cartesian = TRUE)
pe <- pe[anio_et == pmax(pmin(anio, 2025), 2018)]
pinf <- pe[, .(ind = sum(pop0 * p_ind, na.rm = TRUE), tot = sum(pop0)), by = anio][, no_ind := tot - ind]
dth <- inf0[, .(m_ind = sum(muerte & etnia3 == "Indígena"), m_noind = sum(muerte & etnia3 != "Indígena")), by = anio]
mr <- merge(pinf, dth, by = "anio")
mr_per <- mr[, .(m_ind = sum(m_ind), py_ind = sum(ind), m_noind = sum(m_noind), py_noind = sum(no_ind)),
             by = .(periodo = fcase(anio <= 2013, "2011-2013", anio <= 2019, "2014-2019", anio <= 2024, "2020-2024", default = "2025"))]
mr_tot <- mr[, .(periodo = "2011-2025", m_ind = sum(m_ind), py_ind = sum(ind), m_noind = sum(m_noind), py_noind = sum(no_ind))]
mr_per <- rbind(mr_per, mr_tot)
mr_per[, `:=`(tasa_ind = m_ind / py_ind * 1e5, tasa_noind = m_noind / py_noind * 1e5)]
mr_per[, rr := tasa_ind / tasa_noind]
mr_per[, c("rr_lo", "rr_hi") := { t <- mapply(function(a, b, c, d) { x <- poisson.test(c(a, c), c(b, d))$conf.int; x }, m_ind, py_ind, m_noind, py_noind); list(t[1, ], t[2, ]) }]
fwrite(mr_per, "03_resultados/mortalidad_lactantes_etnia_aproximada.csv")

options(width = 220)
print(cohA[order(base, cohortes, df)])
print(apc_con); print(apc_ppc); print(apc_coh[cohorte >= 2010])
print(flujo)
print(orB[modelo %in% c("Principal (sin tipo de confirmación)", "Con tipo de confirmación (plan inicial)", "Solo confirmados por laboratorio", "Firth (sin efecto aleatorio)"),
          .(modelo, termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
print(orB[grepl("prenatal", modelo) & grepl("etnia3Ind|z_cpn4|rural|periodo", termino), .(modelo, termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(orB[grepl("interacción", modelo) & grepl("ind_|etnia3Ind", termino), .(termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
print(orB[grepl("municipio", modelo) & grepl("etnia3Ind|periodo|rural|migrante", termino), .(termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(lrt_int); print(crudo); print(cat_n[order(variable, nivel)]); print(diagP); print(labm)
cat("ICC departamento:", round(icc, 3), " | tendencia letalidad indígena OR/periodo:", round(exp(coef(tend_ind)[2]), 2), " p=", signif(coef(summary(tend_ind))[2, 4], 2), "\n")
print(let); print(let2); print(mr_per)
