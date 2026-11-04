# =============================================================================
# 10_edad_modelos.R
# Firma de la protección del lactante pequeño y desplazamiento de edad.
# Conteos anuales por grupo de edad x año (2007-2025) con offset poblacional.
#
# Modelo 1 (RTI por edad vs línea base; incluye la variabilidad entre años):
#   NB:  log mu[a,y] = beta[a, P(y)] + log pop[a,y],   P = B (2015-2019), E1 (2011-2013), E2 (2025)
# Modelo 2 (razones de RTI dentro del mismo año; efectos fijos de año):
#   NB:  log mu[a,y] = gamma[y] + alpha[a] + delta[a,E1]*1(y in E1) + delta[a,E2]*1(y = 2025) + log pop
#   delta[a,E] = log RoR(a vs 1-4 años; epidemia E vs línea base). La variación común
#   de cada año (intensidad epidémica, vigilancia) queda absorbida por gamma[y].
#   RoRR = exp(delta[a,E2] - delta[a,E1]): protección relativa adicional en 2025.
# Contrafactual: casos y muertes evitados en 2025 entre <6 semanas y <2 meses.
# Sensibilidad: laboratorio, duplicados, año de inicio, líneas base, sin Antioquia,
#   cuasi-Poisson, referencia 6-11 meses.
# =============================================================================
suppressMessages({ library(data.table); library(MASS) })
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

fit_age <- function(cs, pop, base = 2015:2019, year_var = "anio", family = "nb", ref = "1–4 y", fine = TRUE) {
  GR <- if (fine) GR_FINE else GR_2M
  cs <- cs[!is.na(edad_dias)]
  cs[, g := grupo(edad_dias, edad_anios, fine)]
  cnt <- cs[, .N, by = .(anio = get(year_var), g)]
  d <- merge(CJ(anio = 2007:2025, g = GR), cnt, by = c("anio", "g"), all.x = TRUE)[is.na(N), N := 0L]
  d <- merge(d, pop_grupos(pop, fine), by = c("anio", "g"))
  d[, P := fcase(anio %in% base, "B", anio %in% 2011:2013, "E1", anio == 2025, "E2", default = NA_character_)]
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
    data.table(grupo = gg, contraste = c("RoR 2011-2013", "RoR 2025", "RoRR 2025/2011-2013"),
               est = c(r1[1], r2[1], r3[1]), lo = c(r1[2], r2[2], r3[2]), hi = c(r1[3], r2[3], r3[3]), p = c(r1[4], r2[4], r3[4]))
  }))
  list(d = d, m1 = m1, m2 = m2, irr = irr, ror = ror, b2 = b2, V2 = V2, theta2 = if (family == "nb") m2$theta else NA)
}

# ---------------------------------------------------------------------------------
# Análisis principal
# ---------------------------------------------------------------------------------
m_fine <- fit_age(copy(cs0), POP$nac, fine = TRUE)
m_2m   <- fit_age(copy(cs0), POP$nac, fine = FALSE)
fwrite(m_fine$irr, "03_resultados/edad_irr_nb.csv")
fwrite(rbind(m_fine$ror, m_2m$ror[grupo == "<2 mo"]), "03_resultados/edad_ror_nb_efectos_fijos_anio.csv")
saveRDS(list(irr = m_fine$irr, ror = rbind(m_fine$ror, m_2m$ror[grupo == "<2 mo"])), "01_datos/procesados/edad_modelos.rds")

# ---------------------------------------------------------------------------------
# Contrafactual (modelo 2): si el lactante pequeño hubiera tenido en 2025 la misma
# razón de RTI (vs 1-4 años) que en 2011-2013:  E_cf = O / RoRR
# Muertes evitadas = casos evitados x letalidad (Beta-Jeffreys; 2025 o 2011-2013).
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
# Sensibilidad (RoR y RoRR del modelo 2; IRR del modelo 1)
# ---------------------------------------------------------------------------------
specs <- list(
  "Principal (confirmados; base 2015-2019; NB con efectos fijos de año)" = list(),
  "Solo confirmados por laboratorio" = list(cs = cs0[confirmacion == "Laboratorio"]),
  "Sin duplicados probables" = list(cs = cs0[flag_duplicado_probable == FALSE]),
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
             `RoRR <2 m` = r(m2, "<2 mo", "RoRR 2025/2011-2013"), `RoRR 2-3 m` = r(m2, "2–3 mo", "RoRR 2025/2011-2013"),
             `RoR 10-14 a 2025` = r(m2, "10–14 y", "RoR 2025"))
}))
fwrite(sens, "03_resultados/edad_sensibilidad.csv")

options(width = 250)
cat("theta modelo 1:", m_fine$m1$theta, " | theta modelo 2:", m_fine$theta2, "\n")
print(m_fine$irr[, .(grupo, epidemia, irr = round(irr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(rbind(m_fine$ror, m_2m$ror[grupo == "<2 mo"])[, .(grupo, contraste, est = round(est, 2), lo = round(lo, 2), hi = round(hi, 2), p = signif(p, 2))])
print(cf)
print(sens)
