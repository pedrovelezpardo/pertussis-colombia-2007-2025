# =============================================================================
# 11_cohortes_letalidad.R
# A. Cohortes nacidas en la pandemia: desviación respecto a la curva suave de edad
#    (2025 vs línea base), con sensibilidad a grados de libertad y línea base.
# B. Modelo edad-periodo-cohorte bayesiano (INLA, RW2), edades 0-19, 2007-2025:
#    efectos de cohorte sin tendencia lineal (curvatura identificable).
# C. Letalidad en lactantes (2011-2025): regresión logística mixta, Firth,
#    interacción etnia x periodo, contexto municipal (control prenatal) y
#    mortalidad poblacional por etnia (aproximada).
# =============================================================================
suppressMessages({ library(data.table); library(MASS); library(splines); library(lme4)
                   library(logistf); library(INLA); library(readxl) })
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
# =============================================================================
ap <- copy(lex)[, `:=`(i_age = edad + 1, i_per = anio - 2006, i_coh = cohorte - min(cohorte) + 1, id = .I)]
pc <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
fapc <- casos ~ 1 + f(i_age, model = "rw2", hyper = pc, scale.model = TRUE) +
  f(i_per, model = "rw2", hyper = pc, scale.model = TRUE) +
  f(i_coh, model = "rw2", hyper = pc, scale.model = TRUE) + f(id, model = "iid", hyper = pc)
apc <- inla(fapc, family = "poisson", data = as.data.frame(ap), E = pop,
            control.compute = list(dic = TRUE, waic = TRUE, config = TRUE), verbose = FALSE)
# Curvatura de cohorte: efecto de cohorte menos su tendencia lineal (muestras posteriores)
smp <- inla.posterior.sample(2000, apc)
ci <- grep("^i_coh:", rownames(smp[[1]]$latent))
co <- sapply(smp, function(s) s$latent[ci, 1])
k <- seq_len(nrow(co)); X <- cbind(1, k)
detr <- co - X %*% solve(crossprod(X), crossprod(X, co))
coh_years <- sort(unique(ap$cohorte))
apc_coh <- data.table(cohorte = coh_years, dev = apply(detr, 1, median), lo = apply(detr, 1, quantile, .025),
                      hi = apply(detr, 1, quantile, .975), p_pos = rowMeans(detr > 0))
# Contraste: cohortes 2019-2022 vs cohortes 2014-2018 (curvatura media)
pan <- colMeans(detr[coh_years %in% 2019:2022, , drop = FALSE]) - colMeans(detr[coh_years %in% 2014:2018, , drop = FALSE])
apc_con <- data.table(contraste = "Cohortes 2019-2022 vs 2014-2018 (desviación media, escala log)",
                      exp_dif = exp(median(pan)), lo = exp(quantile(pan, .025)), hi = exp(quantile(pan, .975)), p_pos = mean(pan > 0))
fwrite(apc_coh, "03_resultados/apc_cohortes_curvatura.csv"); fwrite(apc_con, "03_resultados/apc_contraste_pandemia.csv")
saveRDS(list(coh = apc_coh, age = apc$summary.random$i_age, per = apc$summary.random$i_per, con = apc_con),
        "01_datos/procesados/apc_inla.rds")

# =============================================================================
# C. Letalidad en lactantes
# =============================================================================
covm <- readRDS("01_datos/procesados/covariables_municipales_terridata.rds")
inf <- cs[lactante == TRUE & anio >= 2011 & !is.na(grupo_edad)]
inf[, `:=`(
  edad_g = factor(fcase(edad_dias < 42, "<6 semanas", edad_dias < 61, "6-8 semanas", edad_dias < 122, "2-3 meses",
                        edad_dias < 183, "4-5 meses", default = "6-11 meses"),
                  levels = c("6-11 meses", "4-5 meses", "2-3 meses", "6-8 semanas", "<6 semanas")),
  periodo4 = factor(fcase(anio <= 2013, "2011-2013", anio <= 2019, "2014-2019", anio <= 2024, "2020-2024", default = "2025"),
                    levels = c("2011-2013", "2014-2019", "2020-2024", "2025")),
  etnia3 = factor(fifelse(etnia == "Rrom", "Otro/ninguno", etnia), levels = c("Otro/ninguno", "Afrodescendiente", "Indígena")),
  regimen4 = factor(fcase(regimen == "Contributivo", "Contributivo", regimen == "Subsidiado", "Subsidiado",
                          regimen == "No asegurado", "No asegurado", default = "Otro"),
                    levels = c("Contributivo", "Subsidiado", "No asegurado", "Otro")),
  rural = as.integer(area == "Rural"), mujer = as.integer(sexo == "Mujer"),
  lab = as.integer(confirmacion == "Laboratorio"), migrante = as.integer(migrante),
  dp = factor(dp_res), MPIO = mpio_res)]
inf <- merge(inf, covm[, .(MPIO, cpn4 = cpn4_1924)], by = "MPIO", all.x = TRUE)
inf[, z_cpn4 := (cpn4 - mean(cpn4, na.rm = TRUE)) / sd(cpn4, na.rm = TRUE)]
cc <- inf[complete.cases(inf[, .(muerte, edad_g, periodo4, etnia3, regimen4, rural, mujer, lab, dp)])]
or_tab <- function(m, lab) { cf <- summary(m)$coefficients
  data.table(modelo = lab, termino = rownames(cf), or = exp(cf[, 1]), lo = exp(cf[, 1] - 1.96 * cf[, 2]),
             hi = exp(cf[, 1] + 1.96 * cf[, 2]), p = cf[, 4])[termino != "(Intercept)"] }
ctl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
mB1 <- glmer(muerte ~ edad_g + periodo4 + etnia3 + regimen4 + rural + mujer + lab + migrante + (1 | dp),
             data = cc, family = binomial, control = ctl)
ccA <- cc[!is.na(z_cpn4)]
mB2a <- glmer(muerte ~ edad_g + periodo4 + etnia3 + regimen4 + rural + mujer + lab + migrante + (1 | dp),
              data = ccA, family = binomial, control = ctl)
mB2b <- update(mB2a, . ~ . + z_cpn4)
mB3 <- glmer(muerte ~ edad_g + periodo4 * I(etnia3 == "Indígena") + regimen4 + rural + mujer + lab + (1 | dp),
             data = cc, family = binomial, control = ctl)
fir <- logistf(muerte ~ edad_g + periodo4 + etnia3 + regimen4 + rural + mujer + lab + migrante, data = as.data.frame(cc))
fir_tab <- data.table(modelo = "Firth (sin efecto aleatorio)", termino = names(coef(fir)), or = exp(coef(fir)),
                      lo = exp(fir$ci.lower), hi = exp(fir$ci.upper), p = fir$prob)[termino != "(Intercept)"]
orB <- rbind(or_tab(mB1, "Mixto principal"), or_tab(mB2a, "Mixto, submuestra con control prenatal"),
             or_tab(mB2b, "Mixto + % nacidos con >=4 controles prenatales (municipal, DE)"),
             or_tab(mB3, "Mixto con interacción periodo x indígena"), fir_tab)
orB[, `:=`(n = fcase(modelo == "Mixto principal", nrow(cc), grepl("submuestra|prenatales", modelo), nrow(ccA), default = nrow(cc)),
           muertes = fcase(modelo == "Mixto principal", sum(cc$muerte), grepl("submuestra|prenatales", modelo), sum(ccA$muerte), default = sum(cc$muerte)))]
fwrite(orB, "03_resultados/letalidad_modelos_OR.csv")
icc <- as.numeric(VarCorr(mB1)$dp) / (as.numeric(VarCorr(mB1)$dp) + pi^2 / 3)

# Letalidad cruda con IC exacto (Clopper-Pearson) por etnia, periodo y edad
cp <- function(x, n) { b <- binom.test(x, n); sprintf("%.1f%% (%.1f–%.1f)", 100 * x / n, 100 * b$conf.int[1], 100 * b$conf.int[2]) }
let <- inf[, .(lactantes = .N, muertes = sum(muerte)), by = .(periodo4, etnia3)][order(periodo4, etnia3)]
let[, letalidad := mapply(cp, muertes, lactantes)]
fwrite(let, "03_resultados/letalidad_cruda_etnia_periodo.csv")
# Tendencia de la letalidad en indígenas (2011-2025)
tend_ind <- glm(muerte ~ as.numeric(periodo4), family = binomial, data = inf[etnia3 == "Indígena"])

# Mortalidad poblacional por etnia (aproximada): lactantes indígenas = sum_m pob0_m x %indígena_m
et <- as.data.table(read_excel("01_datos/raw/dane_etnia/anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun.xlsx",
                               col_names = FALSE, col_types = "text", skip = 7))
setnames(et, c("dp", "dpnom", "MPIO", "mpnom", "anio", "area", "total", "indigena", "rrom", "raizal", "palenquero", "afro", "ninguno"))
et <- et[area == "Total", .(MPIO, anio = as.integer(anio), p_ind = as.numeric(indigena) / as.numeric(total))]
pm <- readRDS("01_datos/procesados/pob_mun_grupos.rds")[area == "total" & grupo == "0", .(MPIO, anio, pop0 = pop)]
pe <- merge(pm[anio %in% 2011:2025], et[, .(MPIO, anio_et = anio, p_ind)], by = "MPIO", allow.cartesian = TRUE)
pe <- pe[anio_et == pmax(pmin(anio, 2025), 2018)]
pinf <- pe[, .(ind = sum(pop0 * p_ind, na.rm = TRUE), tot = sum(pop0)), by = anio][, no_ind := tot - ind]
dth <- inf[, .(m_ind = sum(muerte & etnia3 == "Indígena"), m_noind = sum(muerte & etnia3 != "Indígena")), by = anio]
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
print(apc_con); print(apc_coh[cohorte >= 2010])
print(orB[modelo %in% c("Mixto principal", "Firth (sin efecto aleatorio)"), .(modelo, termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
print(orB[grepl("prenatal", modelo) & grepl("etnia3Ind|z_cpn4|rural", termino), .(modelo, termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(orB[grepl("interacción", modelo) & grepl("Indígena", termino), .(termino, or = round(or, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
cat("ICC departamento:", round(icc, 3), " | tendencia letalidad indígena OR/periodo:", round(exp(coef(tend_ind)[2]), 2), " p=", signif(coef(summary(tend_ind))[2, 4], 2), "\n")
print(let); print(mr_per)
