# =============================================================================
# 04_descriptivo.R
# Incidencia anual, distribución por edad, comparación entre epidemias
# (2011-2013 vs 2025) y tablas por periodo y departamento.
# Salidas en 03_resultados/ (csv) y 01_datos/procesados/ (rds para figuras)
# =============================================================================
suppressMessages({ library(data.table); library(openxlsx) })
cs  <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
pn  <- readRDS("01_datos/procesados/pob_nac_edad_sexo.rds")[area == "total"]
pdp <- readRDS("01_datos/procesados/pob_dep_edad_sexo.rds")[area == "total"]
dir.create("03_resultados", showWarnings = FALSE)
YRS <- 2007:2025

# --- Denominadores nacionales -------------------------------------------------------
pop_age <- pn[anio %in% YRS, .(pop = sum(pop)), by = .(anio, edad)]
pop_tot <- pop_age[, .(pop = sum(pop)), by = anio]
pop0    <- pop_age[edad == 0, .(anio, pop0 = pop)]

# Grupos de edad (lactantes en meses: denominador = población de 0 años x fracción del año)
grp_lab <- c("<2 m", "2-3 m", "4-5 m", "6-11 m", "1-4 a", "5-9 a", "10-14 a", "15-19 a",
             "20-39 a", "40-59 a", "60+ a")
pop_grp <- rbind(
  pop0[, .(anio, grupo_edad = "<2 m",   pop = pop0 * 61 / 365.25)],
  pop0[, .(anio, grupo_edad = "2-3 m",  pop = pop0 * 61 / 365.25)],
  pop0[, .(anio, grupo_edad = "4-5 m",  pop = pop0 * 61 / 365.25)],
  pop0[, .(anio, grupo_edad = "6-11 m", pop = pop0 * 182.25 / 365.25)],
  pop_age[, .(pop = sum(pop[edad >= 1 & edad <= 4])),   by = anio][, grupo_edad := "1-4 a"],
  pop_age[, .(pop = sum(pop[edad >= 5 & edad <= 9])),   by = anio][, grupo_edad := "5-9 a"],
  pop_age[, .(pop = sum(pop[edad >= 10 & edad <= 14])), by = anio][, grupo_edad := "10-14 a"],
  pop_age[, .(pop = sum(pop[edad >= 15 & edad <= 19])), by = anio][, grupo_edad := "15-19 a"],
  pop_age[, .(pop = sum(pop[edad >= 20 & edad <= 39])), by = anio][, grupo_edad := "20-39 a"],
  pop_age[, .(pop = sum(pop[edad >= 40 & edad <= 59])), by = anio][, grupo_edad := "40-59 a"],
  pop_age[, .(pop = sum(pop[edad >= 60])),              by = anio][, grupo_edad := "60+ a"],
  use.names = TRUE)
pop_grp[, grupo_edad := factor(grupo_edad, levels = grp_lab)]

# --- 1. Serie anual -------------------------------------------------------------------
anual <- cs[, .(casos = .N, lactantes = sum(lactante, na.rm = TRUE),
                menores2m = sum(grupo_edad == "<2 m", na.rm = TRUE),
                muertes = sum(muerte), muertes_lact = sum(muerte & lactante, na.rm = TRUE),
                hosp = sum(hospitalizado, na.rm = TRUE), hosp_n = sum(!is.na(hospitalizado)),
                lab = sum(confirmacion == "Laboratorio", na.rm = TRUE),
                edad_mediana_m = median(edad_meses, na.rm = TRUE),
                p_10mas = mean(edad_anios >= 10, na.rm = TRUE)), by = anio]
anual <- merge(anual, pop_tot, by = "anio")
anual <- merge(anual, pop0, by = "anio")
anual[, `:=`(inc_100k = casos / pop * 1e5, inc_lact_100k = lactantes / pop0 * 1e5,
             mort_lact_100k = muertes_lact / pop0 * 1e5, letal_lact = muertes_lact / lactantes,
             p_lact = lactantes / casos, p_2m = menores2m / casos)]
setorder(anual, anio)
fwrite(anual, "03_resultados/tabla_serie_anual.csv")

# --- 2. Incidencia por grupo de edad y año ---------------------------------------------
inc_ge <- cs[!is.na(grupo_edad), .(casos = .N, muertes = sum(muerte)), by = .(anio, grupo_edad)]
inc_ge <- merge(CJ(anio = YRS, grupo_edad = factor(grp_lab, levels = grp_lab)), inc_ge,
                by = c("anio", "grupo_edad"), all.x = TRUE)
inc_ge[is.na(casos), `:=`(casos = 0L, muertes = 0L)]
inc_ge <- merge(inc_ge, pop_grp, by = c("anio", "grupo_edad"))
inc_ge[, inc_100k := casos / pop * 1e5]
saveRDS(inc_ge, "01_datos/procesados/incidencia_grupo_edad_anio.rds")
fwrite(dcast(inc_ge, grupo_edad ~ anio, value.var = "inc_100k"), "03_resultados/tabla_incidencia_edad_anio.csv")

# --- 3. Razón de tasas por edad: 2025 vs 2012 y 2025 vs línea base 2015-2019 ------------
irr_fun <- function(a, b, lab) {
  x <- merge(inc_ge[anio %in% a, .(c1 = sum(casos), p1 = sum(pop)), by = grupo_edad],
             inc_ge[anio %in% b, .(c0 = sum(casos), p0 = sum(pop)), by = grupo_edad], by = "grupo_edad")
  x[, `:=`(irr = (c1 / p1) / (c0 / p0), se = sqrt(1 / pmax(c1, 0.5) + 1 / pmax(c0, 0.5)))]
  x[, `:=`(lo = irr * exp(-1.96 * se), hi = irr * exp(1.96 * se), comparacion = lab)]
  x[]
}
irr <- rbind(irr_fun(2025, 2012, "2025 vs 2012"),
             irr_fun(2025, 2015:2019, "2025 vs 2015-2019"),
             irr_fun(2011:2013, 2015:2019, "2011-2013 vs 2015-2019"))
fwrite(irr, "03_resultados/tabla_irr_edad.csv")
saveRDS(irr, "01_datos/procesados/irr_edad.rds")


# --- 3b. Sensibilidad: solo casos confirmados por laboratorio -------------------------------
inc_lab <- cs[!is.na(grupo_edad) & confirmacion == "Laboratorio", .(casos = .N), by = .(anio, grupo_edad)]
inc_lab <- merge(CJ(anio = YRS, grupo_edad = factor(grp_lab, levels = grp_lab)), inc_lab,
                 by = c("anio", "grupo_edad"), all.x = TRUE)[is.na(casos), casos := 0L]
inc_lab <- merge(inc_lab, pop_grp, by = c("anio", "grupo_edad"))
irr_lab_fun <- function(a, b, lab) {
  x <- merge(inc_lab[anio %in% a, .(c1 = sum(casos), p1 = sum(pop)), by = grupo_edad],
             inc_lab[anio %in% b, .(c0 = sum(casos), p0 = sum(pop)), by = grupo_edad], by = "grupo_edad")
  x[, `:=`(irr = (c1 / p1) / (c0 / p0), se = sqrt(1 / pmax(c1, 0.5) + 1 / pmax(c0, 0.5)))]
  x[, `:=`(lo = irr * exp(-1.96 * se), hi = irr * exp(1.96 * se), comparacion = lab)]
  x[]
}
irr_lab <- rbind(irr_lab_fun(2025, 2015:2019, "2025 vs 2015-2019 (solo laboratorio)"),
                 irr_lab_fun(2011:2013, 2015:2019, "2011-2013 vs 2015-2019 (solo laboratorio)"))
fwrite(irr_lab, "03_resultados/tabla_irr_edad_solo_laboratorio.csv")
saveRDS(irr_lab, "01_datos/procesados/irr_edad_lab.rds")
# Razón de razones: <2 meses frente a 1-4 años (firma de protección del lactante pequeño)
ror <- function(tab) {
  a <- tab[grupo_edad == "<2 m"]; b <- tab[grupo_edad == "1-4 a"]
  r <- a$irr / b$irr; se <- sqrt(a$se^2 + b$se^2)
  data.table(comparacion = a$comparacion, ror_2m_vs_1_4 = r, lo = r * exp(-1.96 * se), hi = r * exp(1.96 * se))
}
ror_tab <- rbindlist(lapply(split(rbind(irr, irr_lab), by = "comparacion"), ror))
fwrite(ror_tab, "03_resultados/tabla_razon_de_razones_2m.csv")

# --- 4. Lexis: incidencia por edad simple (0-19) y año; lactantes por meses -------------
lex <- cs[!is.na(edad_anios) & edad_anios <= 19, .(casos = .N), by = .(anio, edad = edad_anios)]
lex <- merge(CJ(anio = YRS, edad = 0:19), lex, by = c("anio", "edad"), all.x = TRUE)
lex[is.na(casos), casos := 0L]
lex <- merge(lex, pop_age, by = c("anio", "edad"))
lex[, `:=`(inc_100k = casos / pop * 1e5, cohorte = anio - edad)]
saveRDS(lex, "01_datos/procesados/lexis_edad_anio.rds")

# --- 5. Tabla 1 por periodo -------------------------------------------------------------
fmt_n <- function(x, n) sprintf("%d (%.1f)", x, 100 * x / n)
per <- cs[, .(
  `Casos confirmados` = .N,
  `Edad mediana, meses (RIC)` = sprintf("%.0f (%.0f-%.0f)", median(edad_meses, na.rm = TRUE),
                                         quantile(edad_meses, .25, na.rm = TRUE), quantile(edad_meses, .75, na.rm = TRUE)),
  `<2 meses, n (%)` = fmt_n(sum(grupo_edad == "<2 m", na.rm = TRUE), .N),
  `<1 año, n (%)` = fmt_n(sum(lactante, na.rm = TRUE), .N),
  `1-9 años, n (%)` = fmt_n(sum(edad_anios %in% 1:9, na.rm = TRUE), .N),
  `>=10 años, n (%)` = fmt_n(sum(edad_anios >= 10, na.rm = TRUE), .N),
  `Mujeres, n (%)` = fmt_n(sum(sexo == "Mujer", na.rm = TRUE), .N),
  `Régimen subsidiado, n (%)` = fmt_n(sum(regimen == "Subsidiado"), .N),
  `No asegurado, n (%)` = fmt_n(sum(regimen == "No asegurado"), .N),
  `Indígena, n (%)` = fmt_n(sum(etnia == "Indígena"), .N),
  `Afrodescendiente, n (%)` = fmt_n(sum(etnia == "Afrodescendiente"), .N),
  `Rural, n (%)` = fmt_n(sum(area == "Rural", na.rm = TRUE), .N),
  `Confirmación por laboratorio, n (%)` = fmt_n(sum(confirmacion == "Laboratorio", na.rm = TRUE), .N),
  `Hospitalizados, n (%)` = sprintf("%d (%.1f)", sum(hospitalizado, na.rm = TRUE), 100 * mean(hospitalizado, na.rm = TRUE)),
  `Muertes, n` = sum(muerte),
  `Letalidad en <1 año, %` = sprintf("%.2f", 100 * sum(muerte & lactante, na.rm = TRUE) / sum(lactante, na.rm = TRUE)),
  `Letalidad en <2 meses, %` = sprintf("%.2f", 100 * sum(muerte & grupo_edad == "<2 m", na.rm = TRUE) / sum(grupo_edad == "<2 m", na.rm = TRUE)),
  `Días inicio-consulta, mediana (RIC)` = sprintf("%.0f (%.0f-%.0f)", median(ret_consulta, na.rm = TRUE),
                                                    quantile(ret_consulta, .25, na.rm = TRUE), quantile(ret_consulta, .75, na.rm = TRUE))
), by = periodo]
per_pop <- merge(cs[, .(casos = .N, lact = sum(lactante, na.rm = TRUE), m2 = sum(grupo_edad == "<2 m", na.rm = TRUE)), by = .(periodo, anio)],
                 merge(pop_tot, pop0, by = "anio"), by = "anio")[, .(
  `Incidencia anual media por 100.000` = sprintf("%.2f", sum(casos) / sum(pop) * 1e5),
  `Incidencia <1 año por 100.000` = sprintf("%.1f", sum(lact) / sum(pop0) * 1e5),
  `Incidencia <2 meses por 100.000` = sprintf("%.1f", sum(m2) / sum(pop0 * 61 / 365.25) * 1e5)), by = periodo]
tabla1 <- merge(per, per_pop, by = "periodo")
ord <- c("2007-2010", "2011-2013 (epidemia)", "2014-2019", "2020-2024 (pandemia/postpandemia)", "2025 (epidemia)")
tabla1 <- tabla1[match(ord, periodo)]
t1 <- data.table(Variable = names(tabla1)[-1], t(as.matrix(tabla1[, -1])))
setnames(t1, c("Variable", tabla1$periodo))
fwrite(t1, "03_resultados/tabla1_caracteristicas_periodo.csv")

# --- 6. Departamentos: incidencia 2025 vs 2011-2013 y 2015-2019 --------------------------
dpop <- pdp[anio %in% YRS, .(pop = sum(pop), pop0 = sum(pop[edad == 0])), by = .(DP, anio)]
dcas <- cs[!is.na(dp_res), .(casos = .N, lact = sum(lactante, na.rm = TRUE), muertes = sum(muerte)), by = .(DP = dp_res, anio)]
dd <- merge(CJ(DP = unique(dpop$DP), anio = YRS), dcas, by = c("DP", "anio"), all.x = TRUE)
for (v in c("casos", "lact", "muertes")) dd[is.na(get(v)), (v) := 0L]
dd <- merge(dd, dpop, by = c("DP", "anio"))
dnom <- unique(cs[!is.na(dp_res), .(DP = dp_res, dp_nom)])[, .SD[1], by = DP]
dep <- dd[, .(casos_2025 = sum(casos[anio == 2025]), inc_2025 = sum(casos[anio == 2025]) / sum(pop[anio == 2025]) * 1e5,
              inc_lact_2025 = sum(lact[anio == 2025]) / sum(pop0[anio == 2025]) * 1e5,
              inc_2011_13 = sum(casos[anio %in% 2011:2013]) / sum(pop[anio %in% 2011:2013]) * 1e5,
              inc_2015_19 = sum(casos[anio %in% 2015:2019]) / sum(pop[anio %in% 2015:2019]) * 1e5,
              muertes_2025 = sum(muertes[anio == 2025])), by = DP]
dep <- merge(dep, dnom, by = "DP", all.x = TRUE)
setorder(dep, -inc_2025)
fwrite(dep, "03_resultados/tabla_departamentos.csv")
saveRDS(dd, "01_datos/procesados/departamento_anio.rds")

# --- Libro Excel con todas las tablas ------------------------------------------------------
wb <- createWorkbook()
for (nm in c("tabla1_caracteristicas_periodo", "tabla_serie_anual", "tabla_irr_edad",
             "tabla_irr_edad_solo_laboratorio", "tabla_razon_de_razones_2m",
             "tabla_incidencia_edad_anio", "tabla_departamentos")) {
  addWorksheet(wb, substr(nm, 1, 31)); writeData(wb, substr(nm, 1, 31), fread(sprintf("03_resultados/%s.csv", nm)))
}
saveWorkbook(wb, "03_resultados/Tablas_descriptivas.xlsx", overwrite = TRUE)

# --- Resumen en consola -------------------------------------------------------------------
print(anual[, .(anio, casos, inc_100k = round(inc_100k, 2), inc_lact = round(inc_lact_100k, 1),
                p_lact = round(p_lact, 2), p_2m = round(p_2m, 3), muertes, letal_lact = round(letal_lact, 3),
                edad_med_m = edad_mediana_m, p_10mas = round(p_10mas, 2))])
print(t1)
print(irr[, .(comparacion, grupo_edad, c1, c0, irr = round(irr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(irr_lab[, .(comparacion, grupo_edad, c1, c0, irr = round(irr, 2), lo = round(lo, 2), hi = round(hi, 2))])
print(ror_tab)
print(head(dep, 12))
