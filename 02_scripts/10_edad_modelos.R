# =============================================================================
# 10_edad_modelos.R  (versión del manuscrito v4, 12-11-2026)
# Estructura de edad de las epidemias y patrón esperado si la Tdap materna protege
# al lactante pequeño. Conteos anuales por grupo de edad x año (2007-2025) con offset.
#
# Modelo 1 (RTI por edad vs línea base; incluye la variabilidad entre años):
#   NB:  log mu[a,y] = beta[a, P(y)] + log pop[a,y],   P = B (2015-2019), E1 (2011-2013), E2 (2025)
# Modelo 2 (razones de RTI dentro del mismo año; efectos fijos de año):
#   NB:  log mu[a,y] = gamma[y] + alpha[a] + delta[a,E1]*1(y in E1) + delta[a,E2]*1(y = 2025) + log pop
#   delta[a,E] = log RoR(a vs 1-4 años; epidemia E vs línea base).
#   RoRR = exp(delta[a,E2] - delta[a,E1]).
# Contrafactual (hipotético): casos y muertes en 2025 entre <6 semanas y <2 meses si la
#   razón de RTI hubiera sido la de 2011-2013.
# Sensibilidad: laboratorio, duplicados, año de inicio, líneas base, sin Antioquia,
#   cuasi-Poisson, referencia 6-11 meses.
# Nuevo en la v4 (exploratorio):
#   - Holm sobre las cinco RoRR de los grupos de lactantes y prueba de tendencia con la edad.
#   - Estudio de eventos: modelo 2 con una interacción edad x año para cada año fuera de la
#     línea base (2007-2014 y 2020-2025), con 1-4 años y 6-11 meses como referencia.
#   - RoRR de 2025 frente a otros años previos a la Tdap (2007-2012, 2007-2013, 2011-2012, 2007-2010).
#   - Diagnósticos (Pearson/gl y residuos simulados con DHARMa).
#   - Confirmación por laboratorio por grupo de edad y periodo.
#   - La fila principal de la tabla de sensibilidad toma cada columna del mismo ajuste que la tabla 3.
# =============================================================================
suppressMessages({ library(data.table); library(MASS); library(DHARMa) })
set.seed(20252)
NSIM <- 10000
cs0 <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
cs0[, anio_inicio := as.integer(format(f_inicio, "%Y"))]
pn  <- readRDS("01_datos/procesados/pob_nac_edad_sexo.rds")[area == "total"]
pdp <- readRDS("01_datos/procesados/pob_dep_edad_sexo.rds")[area == "total"]

GR_FINE <- c("<6 wk", "6–8 wk", "2–3 mo", "4–5 mo", "6–11 mo", "1–4 y", "5–9 y", "10–14 y", "15–19 y",
             "20–39 y", "40–59 y", "60+ y")
GR_2M   <- c("<2 mo", "2–3 mo", "4–5 mo", "6–11 mo", "1–4 y", "5–9 y", "10–14 y", "15–19 y",
             "20–39 y", "40–59 y", "60+ y")
W_FINE <- c(`<6 wk` = 42, `6–8 wk` = 19, `2–3 mo` = 61, `4–5 mo` = 61, `6–11 mo` = 182) / 365.25
W_2M   <- c(`<2 mo` = 61, `2–3 mo` = 61, `4–5 mo` = 61, `6–11 mo` = 182) / 365.25
grupo <- function(dias, anios, fine = TRUE) {
  g0 <- if (fine) fifelse(dias < 42, "<6 wk", "6–8 wk") else rep("<2 mo", length(dias))
  fcase(dias < 61, g0, dias < 122, "2–3 mo", dias < 183, "4–5 mo", anios == 0, "6–11 mo",
        anios <= 4, "1–4 y", anios <= 9, "5–9 y", anios <= 14, "10–14 y", anios <= 19, "15–19 y",
        anios <= 39, "20–39 y", anios <= 59, "40–59 y", default = "60+ y")
}
pop_grupos <- function(p, fine = TRUE) {
  w <- if (fine) W_FINE else W_2M
  p0 <- p[edad == 0, .(anio, pop0 = pop)]
  inf <- rbindlist(lapply(names(w), function(g) p0[, .(anio, g = g, pop = pop0 * w[[g]])]))
  oth <- p[edad >= 1, .(pop = sum(pop)), by = .(anio, g = grupo(rep(400, length(edad)), edad))]
  rbind(inf, oth)
}
POP <- list(nac = pn[, .(pop = sum(pop)), by = .(anio, edad)],
            sin_ant = pdp[DP != "05", .(pop = sum(pop)), by = .(anio, edad)])

fit_age <- function(cs, pop, base = 2015:2019, year_var = "anio", family = "nb", ref = "1–4 y", fine = TRUE, e1 = 2011:2013) {
  GR <- if (fine) GR_FINE else GR_2M
  lab1 <- paste(range(e1), collapse = "-")
  cs <- cs[!is.na(edad_dias)]
  cs[, g := grupo(edad_dias, edad_anios, fine)]
  cnt <- cs[, .N, by = .(anio = get(year_var), g)]
  d <- merge(CJ(anio = 2007:2025, g = GR), cnt, by = c("anio", "g"), all.x = TRUE)[is.na(N), N := 0L]
  d <- merge(d, pop_grupos(pop, fine), by = c("anio", "g"))
  d[, P := fcase(anio %in% base, "B", anio %in% e1, "E1", anio == 2025, "E2", default = NA_character_)]
  d <- d[!is.na(P)]
  fitf <- function(f, data) if (family == "nb") glm.nb(f, data = data) else glm(f, family = quasipoisson, data = data)
  # --- Modelo 1: RTI por edad ---
  d[, cell := factor(paste(g, P, sep = "|"))]
  m1 <- fitf(N ~ 0 + cell + offset(log(pop)), d)
  b1 <- coef(m1); V1 <- vcov(m1)
  irr <- rbindlist(lapply(GR, function(gg) rbindlist(lapply(c("E1", "E2"), function(E) {
    v <- setNames(numeric(length(b1)), names(b1)); v[paste0("cell", gg, "|", E)] <- 1; v[paste0("cell", gg, "|B")] <- -1
    e <- sum(v * b1); se <- sqrt(drop(t(v) %*% V1 %*% v))
    data.table(grupo = gg, epidemia = E, irr = exp(e), lo = exp(e - 1.96 * se), hi = exp(e + 1.96 * se))
  }))))
  # --- Modelo 2: RoR con efectos fijos de año ---
  d[, g := relevel(factor(g, levels = GR), ref = ref)]
  others <- setdiff(GR, ref)
  for (gg in others) for (E in c("E1", "E2")) d[, (paste0("d_", make.names(gg), "_", E)) := as.integer(g == gg & P == E)]
  dv <- grep("^d_", names(d), value = TRUE)
  f2 <- as.formula(paste("N ~ factor(anio) + g +", paste(dv, collapse = " + "), "+ offset(log(pop))"))
  m2 <- fitf(f2, d)
  b2 <- coef(m2); V2 <- vcov(m2)
  ror <- rbindlist(lapply(others, function(gg) {
    k1 <- paste0("d_", make.names(gg), "_E1"); k2 <- paste0("d_", make.names(gg), "_E2")
    one <- function(v) { e <- sum(v * b2[names(v)]); se <- sqrt(drop(t(v) %*% V2[names(v), names(v)] %*% v)); c(exp(e), exp(e - 1.96 * se), exp(e + 1.96 * se), 2 * pnorm(-abs(e / se))) }
    r1 <- one(setNames(1, k1)); r2 <- one(setNames(1, k2)); r3 <- one(setNames(c(1, -1), c(k2, k1)))
    data.table(grupo = gg, contraste = c(paste("RoR", lab1), "RoR 2025", paste0("RoRR 2025/", lab1)),
               est = c(r1[1], r2[1], r3[1]), lo = c(r1[2], r2[2], r3[2]), hi = c(r1[3], r2[3], r3[3]), p = c(r1[4], r2[4], r3[4]))
  }))
  list(d = d, m1 = m1, m2 = m2, irr = irr, ror = ror, b2 = b2, V2 = V2, theta2 = if (family == "nb") m2$theta else NA)
}

# ---------------------------------------------------------------------------------
# Análisis principal
# ---------------------------------------------------------------------------------
m_fine <- fit_age(copy(cs0), POP$nac, fine = TRUE)
m_2m   <- fit_age(copy(cs0), POP$nac, fine = FALSE)
ror_main <- rbind(m_fine$ror, m_2m$ror[grupo == "<2 mo"])
# Holm sobre las cinco RoRR de los grupos finos de lactantes (familia definida en la v4)
INF5 <- c("<6 wk", "6–8 wk", "2–3 mo", "4–5 mo", "6–11 mo")
ror_main[, p_holm_lactantes := NA_real_]
ror_main[grupo %in% INF5 & contraste == "RoRR 2025/2011-2013", p_holm_lactantes := p.adjust(p, method = "holm")]
fwrite(m_fine$irr, "03_resultados/edad_irr_nb.csv")
fwrite(ror_main, "03_resultados/edad_ror_nb_efectos_fijos_anio.csv")
saveRDS(list(irr = m_fine$irr, ror = ror_main), "01_datos/procesados/edad_modelos.rds")

# Tendencia de log RoRR con la edad dentro del primer año (punto medio del grupo, en meses)
mid <- c(`<6 wk` = 21 / 30.4375, `6–8 wk` = 51.5 / 30.4375, `2–3 mo` = 91.5 / 30.4375, `4–5 mo` = 152.5 / 30.4375, `6–11 mo` = 274 / 30.4375)
kE2 <- paste0("d_", make.names(INF5), "_E2"); kE1 <- paste0("d_", make.names(INF5), "_E1")
L <- matrix(0, 5, length(m_fine$b2), dimnames = list(INF5, names(m_fine$b2)))
for (i in 1:5) { L[i, kE2[i]] <- 1; L[i, kE1[i]] <- -1 }
y <- drop(L %*% m_fine$b2); Vy <- L %*% m_fine$V2 %*% t(L)
X <- cbind(1, mid); W <- solve(Vy)
bt <- solve(t(X) %*% W %*% X, t(X) %*% W %*% y); Vb <- solve(t(X) %*% W %*% X)
tend <- data.table(contraste = "Cambio de log RoRR (2025 vs 2011-2013) por mes de edad, 0-11 meses (MCG)",
                   razon_por_mes = exp(bt[2]), lo = exp(bt[2] - 1.96 * sqrt(Vb[2, 2])), hi = exp(bt[2] + 1.96 * sqrt(Vb[2, 2])),
                   p = 2 * pnorm(-abs(bt[2] / sqrt(Vb[2, 2]))))
fwrite(tend, "03_resultados/edad_rorr_tendencia_lactantes.csv")

# ---------------------------------------------------------------------------------
# Contrafactual hipotético (modelo 2): si el lactante pequeño hubiera tenido en 2025 la
# misma razón de RTI (vs 1-4 años) que en 2011-2013:  E_cf = O / RoRR.
# Supone que toda la diferencia de la RoRR viene del riesgo del lactante.
# Muertes = casos adicionales x letalidad (Beta-Jeffreys; 2025 o 2011-2013).
# ---------------------------------------------------------------------------------
cf_fun <- function(m, gg, max_dias) {
  k1 <- paste0("d_", make.names(gg), "_E1"); k2 <- paste0("d_", make.names(gg), "_E2")
  bb <- mvrnorm(NSIM, m$b2[c(k1, k2)], m$V2[c(k1, k2), c(k1, k2)])
  rorr <- exp(bb[, 2] - bb[, 1])
  O <- m$d[anio == 2025 & g == gg, N]
  E_cf <- O / rorr; av <- E_cf - O
  cc <- cs0[!is.na(edad_dias) & edad_dias < max_dias]
  d25 <- cc[anio == 2025, .(m = sum(muerte), n = .N)]; d11 <- cc[anio %in% 2011:2013, .(m = sum(muerte), n = .N)]
  cfr25 <- rbeta(NSIM, d25$m + .5, d25$n - d25$m + .5); cfr11 <- rbeta(NSIM, d11$m + .5, d11$n - d11$m + .5)
  q <- function(x, dg = 0) sprintf(paste0("%.", dg, "f (%.", dg, "f–%.", dg, "f)"), median(x), quantile(x, .025), quantile(x, .975))
  data.table(grupo = gg, observados_2025 = O, RoRR = q(rorr, 2), esperados_contrafactual = q(E_cf),
             casos_evitados = q(av), muertes_evitadas_letalidad2025 = q(av * cfr25),
             muertes_evitadas_letalidad2011_13 = q(av * cfr11),
             letalidad_2025 = sprintf("%d/%d", d25$m, d25$n), letalidad_2011_13 = sprintf("%d/%d", d11$m, d11$n))
}
cf <- rbind(cf_fun(m_fine, "<6 wk", 42), cf_fun(m_2m, "<2 mo", 61))
fwrite(cf, "03_resultados/edad_contrafactual_evitados.csv")

# ---------------------------------------------------------------------------------
# Sensibilidad (RoR y RoRR del modelo 2; IRR del modelo 1). Cada columna sale del mismo
# ajuste que la tabla 3: grupos finos, salvo las de <2 meses (ajuste con <2 meses combinado).
# ---------------------------------------------------------------------------------
specs <- list(
  "Principal (confirmados; base 2015-2019; NB con efectos fijos de año)" = list(),
  "Solo confirmados por laboratorio" = list(cs = cs0[confirmacion == "Laboratorio"]),
  "Sin duplicados probables" = list(cs = cs0[flag_duplicado_probable == FALSE]),
  "Sin posibles renotificaciones (marcador amplio)" = list(cs = cs0[flag_posible_duplicado == FALSE]),
  "Año de inicio de síntomas" = list(cs = cs0[!is.na(anio_inicio) & anio_inicio >= 2007], year_var = "anio_inicio"),
  "Línea base 2014-2019" = list(base = 2014:2019),
  "Línea base 2017-2019" = list(base = 2017:2019),
  "Sin Antioquia" = list(cs = cs0[is.na(dp_res) | dp_res != "05"], pop = POP$sin_ant),
  "Cuasi-Poisson" = list(family = "qp"),
  "Referencia 6-11 meses" = list(ref = "6–11 mo"))
fmt <- function(e, l, h) sprintf("%.2f (%.2f–%.2f)", e, l, h)
sens <- rbindlist(lapply(names(specs), function(k) {
  s <- specs[[k]]
  args <- list(cs = copy(if (is.null(s$cs)) cs0 else s$cs), pop = if (is.null(s$pop)) POP$nac else s$pop,
               base = if (is.null(s$base)) 2015:2019 else s$base, year_var = if (is.null(s$year_var)) "anio" else s$year_var,
               family = if (is.null(s$family)) "nb" else s$family, ref = if (is.null(s$ref)) "1–4 y" else s$ref)
  mf <- do.call(fit_age, c(args, fine = TRUE)); m2 <- do.call(fit_age, c(args, fine = FALSE))
  r <- function(m, gg, cc) m$ror[grupo == gg & contraste == cc, fmt(est, lo, hi)]
  i <- function(m, gg, E) m$irr[grupo == gg & epidemia == E, fmt(irr, lo, hi)]
  data.table(analisis = k,
             `IRR <6 sem 2011-13` = i(mf, "<6 wk", "E1"), `IRR <6 sem 2025` = i(mf, "<6 wk", "E2"),
             `RoR <6 sem 2011-13` = r(mf, "<6 wk", "RoR 2011-2013"), `RoR <6 sem 2025` = r(mf, "<6 wk", "RoR 2025"),
             `RoRR <6 sem` = r(mf, "<6 wk", "RoRR 2025/2011-2013"),
             `RoR <2 m 2011-13` = r(m2, "<2 mo", "RoR 2011-2013"), `RoR <2 m 2025` = r(m2, "<2 mo", "RoR 2025"),
             `RoRR <2 m` = r(m2, "<2 mo", "RoRR 2025/2011-2013"), `RoRR 2-3 m` = r(mf, "2–3 mo", "RoRR 2025/2011-2013"),
             `RoR 10-14 a 2025` = r(mf, "10–14 y", "RoR 2025"))
}))
fwrite(sens, "03_resultados/edad_sensibilidad.csv")

# ---------------------------------------------------------------------------------
# RoRR de 2025 frente a otros periodos previos a la Tdap (modelo 2, base 2015-2019)
# ---------------------------------------------------------------------------------
PER <- list(`2011-2013` = 2011:2013, `2007-2012` = 2007:2012, `2007-2013` = 2007:2013, `2011-2012` = 2011:2012, `2007-2010` = 2007:2010)
per_cmp <- rbindlist(lapply(names(PER), function(k) rbindlist(lapply(c("1–4 y", "6–11 mo"), function(rf) {
  mf <- fit_age(copy(cs0), POP$nac, fine = TRUE, ref = rf, e1 = PER[[k]])
  m2 <- fit_age(copy(cs0), POP$nac, fine = FALSE, ref = rf, e1 = PER[[k]])
  x <- rbind(mf$ror[grupo %in% c("<6 wk", "6–11 mo")], m2$ror[grupo == "<2 mo"])
  x[, `:=`(periodo_comparacion = k, referencia = rf)]
}))))
fwrite(per_cmp, "03_resultados/edad_rorr_periodos_comparacion.csv")

# ---------------------------------------------------------------------------------
# Estudio de eventos: una interacción edad x año para cada año fuera de la línea base
# 2015-2019 (los años de la base identifican la sobredispersión). Grupos: <2 m, 2-5 m,
# 6-11 m, 1-4 a, 5-9 a, 10-19 a, 20+ a. RoR_t de <2 m frente a 1-4 años y frente a 6-11 meses.
# ---------------------------------------------------------------------------------
GR_EV <- c("<2 mo", "2–5 mo", "6–11 mo", "1–4 y", "5–9 y", "10–19 y", "20+ y")
grupo_ev <- function(dias, anios) fcase(dias < 61, "<2 mo", dias < 183, "2–5 mo", anios == 0, "6–11 mo", anios <= 4, "1–4 y",
                                       anios <= 9, "5–9 y", anios <= 19, "10–19 y", default = "20+ y")
pop_ev <- function(p) {
  p0 <- p[edad == 0, .(anio, pop0 = pop)]
  w <- c(`<2 mo` = 61, `2–5 mo` = 122, `6–11 mo` = 182) / 365.25
  inf <- rbindlist(lapply(names(w), function(g) p0[, .(anio, g = g, pop = pop0 * w[[g]])]))
  oth <- p[edad >= 1, .(pop = sum(pop)), by = .(anio, g = grupo_ev(rep(400, length(edad)), edad))]
  rbind(inf, oth)
}
event_study <- function(cs, pop, base = 2015:2019, family = "nb") {
  cs <- cs[!is.na(edad_dias)]; cs[, g := grupo_ev(edad_dias, edad_anios)]
  d <- merge(CJ(anio = 2007:2025, g = GR_EV), cs[, .N, by = .(anio, g)], by = c("anio", "g"), all.x = TRUE)[is.na(N), N := 0L]
  d <- merge(d, pop_ev(pop), by = c("anio", "g"))
  d[, g := relevel(factor(g, levels = GR_EV), ref = "1–4 y")]
  yrs <- setdiff(2007:2025, base); others <- setdiff(GR_EV, "1–4 y")
  for (gg in others) for (yy in yrs) d[, (paste0("e_", make.names(gg), "_", yy)) := as.integer(g == gg & anio == yy)]
  ev <- grep("^e_", names(d), value = TRUE)
  f <- as.formula(paste("N ~ factor(anio) + g +", paste(ev, collapse = " + "), "+ offset(log(pop))"))
  # Cada año fuera de la base queda saturado y empuja theta a infinito si se estima en el mismo ajuste;
  # theta se estima con los años de la línea base (modelo con efectos de año y de edad) y se fija.
  m0 <- glm.nb(N ~ factor(anio) + g + offset(log(pop)), data = d[anio %in% base])
  th <- m0$theta
  m <- if (family == "nb") glm(f, family = negative.binomial(th), data = d) else glm(f, family = quasipoisson, data = d)
  b <- coef(m); V <- if (family == "nb") vcov(m, dispersion = 1) else vcov(m)
  est <- function(v) { e <- sum(v * b[names(v)]); se <- sqrt(drop(t(v) %*% V[names(v), names(v)] %*% v))
    c(est = exp(e), lo = exp(e - 1.96 * se), hi = exp(e + 1.96 * se)) }
  out <- rbindlist(lapply(yrs, function(yy) {
    k <- paste0("e_", make.names("<2 mo"), "_", yy); k6 <- paste0("e_", make.names("6–11 mo"), "_", yy)
    rbind(data.table(anio = yy, referencia = "1–4 y", t(est(setNames(1, k)))),
          data.table(anio = yy, referencia = "6–11 mo", t(est(setNames(c(1, -1), c(k, k6))))))
  }))
  cnt <- dcast(d, anio ~ g, value.var = "N")
  list(m = m, m0 = m0, theta = th, rr = out, cnt = cnt, d = d)
}
es <- event_study(copy(cs0), POP$nac)
es_lab <- event_study(copy(cs0[confirmacion == "Laboratorio"]), POP$nac)
es_qp <- event_study(copy(cs0), POP$nac, family = "qp")
es_out <- rbind(es$rr[, analisis := "Todos los confirmados"], es_lab$rr[, analisis := "Solo laboratorio"],
                es_qp$rr[, analisis := "Cuasi-Poisson"])
fwrite(es_out, "03_resultados/edad_estudio_eventos.csv")
fwrite(es$cnt, "03_resultados/edad_estudio_eventos_conteos.csv")
# Razón cruda anual de incidencia (<2 meses frente a 1-4 años y frente a 6-11 meses), todos los años
cr <- es$d[, .(N = sum(N), pop = sum(pop)), by = .(anio, g)]
cr <- dcast(cr, anio ~ g, value.var = c("N", "pop"))
cr[, `:=`(razon_vs_1a4 = (`N_<2 mo` / `pop_<2 mo`) / (`N_1–4 y` / `pop_1–4 y`),
          razon_vs_6a11m = (`N_<2 mo` / `pop_<2 mo`) / (`N_6–11 mo` / `pop_6–11 mo`))]
fwrite(cr[, .(anio, casos_menor2m = `N_<2 mo`, casos_6a11m = `N_6–11 mo`, casos_1a4 = `N_1–4 y`, razon_vs_1a4, razon_vs_6a11m)],
       "03_resultados/edad_razon_cruda_anual.csv")

# ---------------------------------------------------------------------------------
# Diagnósticos: Pearson/gl, theta y residuos simulados (DHARMa, 1.000 simulaciones)
# ---------------------------------------------------------------------------------
dg <- function(m, lab, theta = m$theta) {
  sim <- simulateResiduals(m, n = 1000, plot = FALSE)
  data.table(modelo = lab, n = nobs(m), gl_residuales = df.residual(m), theta = theta,
             pearson_gl = sum(residuals(m, type = "pearson")^2) / df.residual(m),
             p_uniformidad_KS = testUniformity(sim, plot = FALSE)$p.value,
             p_dispersion = testDispersion(sim, plot = FALSE)$p.value,
             p_ceros = testZeroInflation(sim, plot = FALSE)$p.value)
}
diag_age <- rbind(dg(m_fine$m1, "Modelo 1 (grupos finos)"), dg(m_fine$m2, "Modelo 2 (grupos finos)"),
                  dg(m_2m$m2, "Modelo 2 (<2 meses combinado)"), dg(es$m0, "Estudio de eventos: años de la línea base"))
# (los años fuera de la base quedan saturados en el estudio de eventos: sus residuos no informan el ajuste)
fwrite(diag_age, "03_resultados/edad_diagnosticos.csv")

# ---------------------------------------------------------------------------------
# Confirmación por laboratorio por grupo de edad y periodo
# ---------------------------------------------------------------------------------
lab <- cs0[!is.na(edad_dias)][, `:=`(
  g = factor(fcase(edad_dias < 61, "<2 mo", edad_dias < 183, "2–5 mo", edad_anios == 0, "6–11 mo", edad_anios <= 4, "1–4 y",
                   edad_anios <= 9, "5–9 y", default = "≥10 y"), levels = c("<2 mo", "2–5 mo", "6–11 mo", "1–4 y", "5–9 y", "≥10 y")),
  per = factor(fcase(anio <= 2010, "2007-2010", anio <= 2013, "2011-2013", anio <= 2019, "2014-2019", anio <= 2024, "2020-2024", default = "2025"),
               levels = c("2007-2010", "2011-2013", "2014-2019", "2020-2024", "2025")))][,
  .(casos = .N, laboratorio = sum(confirmacion == "Laboratorio"), clinica = sum(confirmacion == "Clínica"),
    nexo = sum(confirmacion == "Nexo epidemiológico")), by = .(per, g)][order(per, g)]
lab[, pct_laboratorio := 100 * laboratorio / casos]
fwrite(lab, "03_resultados/calidad_laboratorio_edad_periodo.csv")

options(width = 250)
cat("theta modelo 1:", m_fine$m1$theta, " | theta modelo 2:", m_fine$theta2, "\n")
print(m_fine$irr[, .(grupo, epidemia, irr = round(irr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(ror_main[, .(grupo, contraste, est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2), p_holm_lactantes = signif(p_holm_lactantes, 2))])
print(tend)
print(cf)
print(sens)
print(per_cmp[grepl("RoRR", contraste), .(periodo_comparacion, referencia, grupo, contraste, est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
print(es_out[, .(analisis, anio, referencia, est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(es$cnt)
print(cr[, .(anio, razon_vs_1a4 = round(razon_vs_1a4, 1), razon_vs_6a11m = round(razon_vs_6a11m, 1))])
print(diag_age)
print(dcast(lab, g ~ per, value.var = "pct_laboratorio")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
