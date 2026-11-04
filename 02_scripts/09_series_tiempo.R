# =============================================================================
# 09_series_tiempo.R
# A. Casos esperados según la línea base prepandémica (2015-2019): déficit
#    acumulado 2020-2024 ("deuda de inmunidad") y exceso 2025, por grupo de edad.
#    Modelo binomial negativo mensual con estacionalidad (Fourier) y offset
#    poblacional; intervalos de predicción por simulación (incertidumbre de
#    parámetros + muestreo NB).
# B. Cronología de la epidemia 2025 por edad: mediana de inicio de síntomas
#    (IC95% bootstrap) y correlación cruzada semanal (adolescentes/adultos -> lactantes).
# =============================================================================
suppressMessages({ library(data.table); library(MASS) })
set.seed(20251)
NSIM <- 5000
cs <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
pn <- readRDS("01_datos/procesados/pob_nac_edad_sexo.rds")[area == "total"]

banda <- function(a) fcase(a == 0, "<1 year", a %in% 1:9, "1–9 years", a >= 10, "10+ years")
cs[, band := banda(edad_anios)]
cs[, mes := as.IDate(format(f_inicio, "%Y-%m-01"))]
pb <- pn[, .(pop = sum(pop)), by = .(anio, band = banda(edad))]
pb <- rbind(pb, pn[, .(pop = sum(pop), band = "All ages"), by = anio])

grid <- CJ(mes = seq(as.IDate("2007-01-01"), as.IDate("2025-12-01"), by = "month"),
           band = c("All ages", "<1 year", "1–9 years", "10+ years"))
cnt <- rbind(cs[!is.na(band) & !is.na(mes), .N, by = .(mes, band)],
             cs[!is.na(mes), .(N = .N, band = "All ages"), by = mes])
ts <- merge(grid, cnt, by = c("mes", "band"), all.x = TRUE)[is.na(N), N := 0L]
ts[, `:=`(anio = as.integer(format(mes, "%Y")), m = as.integer(format(mes, "%m")))]
ts <- merge(ts, pb, by = c("anio", "band"))
ts[, `:=`(logpop = log(pop / 12), s1 = sin(2 * pi * m / 12), c1 = cos(2 * pi * m / 12),
          s2 = sin(4 * pi * m / 12), c2 = cos(4 * pi * m / 12))]

sim_expected <- function(d, base_years, trend = FALSE) {
  fb <- d[anio %in% base_years]
  form <- if (trend) N ~ t + s1 + c1 + s2 + c2 + offset(logpop) else N ~ s1 + c1 + s2 + c2 + offset(logpop)
  d[, t := (anio - 2017) + (m - 1) / 12]; fb[, t := (anio - 2017) + (m - 1) / 12]
  fit <- glm.nb(form, data = fb)
  X <- model.matrix(delete.response(terms(form)), d)
  B <- mvrnorm(NSIM, coef(fit), vcov(fit))
  mu <- exp(X %*% t(B) + d$logpop)                        # meses x simulaciones
  y <- matrix(rnbinom(length(mu), size = fit$theta, mu = mu), nrow = nrow(mu))
  list(fit = fit, mu = mu, y = y)
}

resumen <- function(d, s, years, lab) {
  idx <- which(d$anio %in% years)
  obs <- sum(d$N[idx]); expd <- colSums(s$y[idx, , drop = FALSE])
  data.table(periodo = lab, observados = obs, esperados = median(expd),
             esp_lo = quantile(expd, .025), esp_hi = quantile(expd, .975),
             diferencia = obs - median(expd), dif_lo = obs - quantile(expd, .975), dif_hi = obs - quantile(expd, .025),
             razon_obs_esp = obs / median(expd), razon_lo = obs / quantile(expd, .975), razon_hi = obs / quantile(expd, .025))
}

res <- list(); curvas <- list()
for (b in unique(ts$band)) for (spec in c("principal", "tendencia_2014_2019")) {
  d <- ts[band == b][order(mes)]
  s <- if (spec == "principal") sim_expected(d, 2015:2019) else sim_expected(d, 2014:2019, trend = TRUE)
  r <- rbind(resumen(d, s, 2020:2024, "Déficit 2020-2024"), resumen(d, s, 2025, "Exceso 2025"),
             resumen(d, s, 2020:2025, "Balance 2020-2025"))
  # Cuánto del déficit acumulado se "recuperó" en 2025 (distribución conjunta de simulaciones)
  i1 <- which(d$anio %in% 2020:2024); i2 <- which(d$anio == 2025)
  def <- colSums(s$y[i1, ]) - sum(d$N[i1]); exc <- sum(d$N[i2]) - colSums(s$y[i2, ])
  rec <- exc / def
  r[, `:=`(banda = b, especificacion = spec)]
  res[[paste(b, spec)]] <- cbind(r, recuperacion_mediana = median(rec), rec_lo = quantile(rec, .025), rec_hi = quantile(rec, .975))
  if (spec == "principal") curvas[[b]] <- d[, .(mes, band, N, esp = apply(s$y, 1, median),
                                                lo = apply(s$y, 1, quantile, .025), hi = apply(s$y, 1, quantile, .975))]
}
tab <- rbindlist(res)
fwrite(tab, "03_resultados/series_deficit_exceso.csv")
saveRDS(rbindlist(curvas), "01_datos/procesados/series_mensuales_esperado.rds")

# =============================================================================
# B. Cronología 2025 por edad
# =============================================================================
c25 <- cs[anio == 2025 & !is.na(f_inicio) & f_inicio >= as.IDate("2025-01-01") & !is.na(band)]
c25[, grp := fcase(edad_anios == 0 & edad_dias < 61, "<2 months", edad_anios == 0, "2–11 months",
                   edad_anios %in% 1:9, "1–9 years", edad_anios %in% 10:19, "10–19 years", edad_anios >= 20, "20+ years")]
boot_med <- function(x, B = 4000) { b <- replicate(B, median(sample(x, replace = TRUE))); c(median(x), quantile(b, c(.025, .975))) }
tim <- c25[, { z <- boot_med(as.numeric(f_inicio)); .(n = .N, mediana = as.IDate(z[1]), lo = as.IDate(z[2]), hi = as.IDate(z[3])) }, by = grp]
fwrite(tim, "03_resultados/cronologia_2025_mediana_inicio.csv")
# Diferencia de medianas (adolescentes/adultos - lactantes <2 meses), bootstrap
xa <- as.numeric(c25[grp %in% c("10–19 years", "20+ years")]$f_inicio); xi <- as.numeric(c25[grp == "<2 months"]$f_inicio)
db <- replicate(4000, median(sample(xi, replace = TRUE)) - median(sample(xa, replace = TRUE)))
dif_med <- data.table(comparacion = "Mediana inicio <2 meses menos mediana 10+ años (días)",
                      dias = median(xi) - median(xa), lo = quantile(db, .025), hi = quantile(db, .975))
fwrite(dif_med, "03_resultados/cronologia_2025_diferencia_medianas.csv")
# Correlación cruzada semanal (series suavizadas 3 semanas)
wk <- c25[, .N, by = .(sem = f_inicio - (wday(f_inicio) - 1L), g2 = fifelse(edad_anios >= 10, "older", fifelse(edad_anios == 0, "infant", "child")))]
wk <- dcast(merge(CJ(sem = seq(as.IDate("2024-12-29"), as.IDate("2025-12-28"), by = 7), g2 = c("older", "infant", "child")),
                  wk, by = c("sem", "g2"), all.x = TRUE)[is.na(N), N := 0L], sem ~ g2, value.var = "N")
sm <- function(v) as.numeric(stats::filter(v, rep(1 / 3, 3), sides = 2))
ccf_o <- ccf(sm(wk$older)[2:(nrow(wk) - 1)], sm(wk$infant)[2:(nrow(wk) - 1)], lag.max = 12, plot = FALSE)
ccf_tab <- data.table(lag_semanas = as.integer(ccf_o$lag), r = as.numeric(ccf_o$acf))
fwrite(ccf_tab, "03_resultados/cronologia_2025_correlacion_cruzada.csv")
saveRDS(list(wk = wk, tim = tim), "01_datos/procesados/cronologia_2025.rds")

options(width = 200)
print(tab[especificacion == "principal", .(banda, periodo, observados, esperados = round(esperados), esp_lo = round(esp_lo), esp_hi = round(esp_hi),
                                        razon = round(razon_obs_esp, 2), lo = round(razon_lo, 2), hi = round(razon_hi, 2), rec = round(recuperacion_mediana, 2))])
print(tab[especificacion != "principal" & periodo != "Balance 2020-2025", .(banda, periodo, observados, esperados = round(esperados), razon = round(razon_obs_esp, 2))])
print(tim); print(dif_med); print(ccf_tab[which.max(r)])
