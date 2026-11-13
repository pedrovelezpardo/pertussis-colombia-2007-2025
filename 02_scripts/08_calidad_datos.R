# =============================================================================
# 08_calidad_datos.R
# Control de calidad de los microdatos (evento 800) y covariables municipales:
#   - posibles duplicados (exactos y entre años consecutivos)
#   - datos faltantes por variable
#   - método de confirmación por año
#   - concordancia año de notificación vs año de inicio de síntomas
#   - covariables municipales de TerriData (cobertura pentavalente, control prenatal)
# Salidas: 03_resultados/calidad_*.csv y 01_datos/procesados/covariables_municipales.rds
# =============================================================================
suppressMessages({ library(data.table) })
cs <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
out <- list()

# --- 1. Duplicados ----------------------------------------------------------------
key_exact <- c("fecha_nto", "sexo", "mpio_res", "ini_sin", "fec_con")
dup_exact <- cs[!is.na(fecha_nto), .N, by = key_exact][N > 1]
# Mismo nacimiento, sexo y municipio con inicio de síntomas a <= 30 días (posible re-notificación)
setorder(cs, fecha_nto, sexo, mpio_res, f_inicio)
cs[, grp := .GRP, by = .(fecha_nto, sexo, mpio_res)]
cs[!is.na(fecha_nto), dif := as.numeric(f_inicio - shift(f_inicio)), by = grp]
cs[, posible_dup := !is.na(dif) & dif <= 30]
out$duplicados <- data.table(
  tipo = c("Duplicado exacto (nacimiento, sexo, municipio, inicio, consulta)",
           "Posible re-notificación (mismo nacimiento/sexo/municipio, inicio a <=30 días)"),
  registros = c(sum(dup_exact$N - 1), sum(cs$posible_dup)))
fwrite(out$duplicados, "03_resultados/calidad_duplicados.csv")
cs[, flag_posible_duplicado := posible_dup]
cs[, c("grp", "dif", "posible_dup") := NULL]
# Duplicado probable (criterio estricto, usado en la sensibilidad): mismo nacimiento, sexo y municipio
# y misma fecha de inicio o de consulta; se conserva un registro por grupo.
x <- cs[!is.na(fecha_nto)]
x[, `:=`(n_ini = .N), by = .(fecha_nto, sexo, mpio_res, ini_sin)]
x[, `:=`(n_con = .N), by = .(fecha_nto, sexo, mpio_res, fec_con)]
x <- x[n_ini > 1 | n_con > 1]
x[, dgrp := .GRP, by = .(fecha_nto, sexo, mpio_res)]
setorder(x, dgrp, anio, CONSECUTIVE)
x[, orden := seq_len(.N), by = dgrp]
cs[, flag_duplicado_probable := FALSE]
cs[x[orden > 1], on = .(CONSECUTIVE, anio), flag_duplicado_probable := TRUE]
fwrite(data.table(criterio = "Mismo nacimiento, sexo, municipio y misma fecha de inicio o de consulta (se conserva 1 por grupo)",
                  registros_excluidos_en_sensibilidad = sum(cs$flag_duplicado_probable)),
       "03_resultados/calidad_duplicados_probables.csv")
saveRDS(cs, "01_datos/procesados/tosferina_casos_2007_2025.rds")   # se agregan las marcas para sensibilidad
fwrite(cs, "01_datos/procesados/tosferina_casos_2007_2025.csv")

# --- 2. Datos faltantes -------------------------------------------------------------
vars <- c("edad_dias", "sexo", "fecha_nto", "ini_sin", "mpio_res", "area", "regimen", "etnia",
          "hospitalizado", "ret_consulta", "confirmacion")
falt <- rbindlist(lapply(vars, function(v) {
  x <- cs[[v]]
  miss <- if (v == "regimen") x == "Indeterminado" else if (is.character(x)) is.na(x) | x == "" else is.na(x)
  data.table(variable = v, faltantes = sum(miss), porcentaje = round(100 * mean(miss), 2))
}))
fwrite(falt, "03_resultados/calidad_faltantes.csv")

# --- 3. Confirmación por año ----------------------------------------------------------
conf <- dcast(cs[, .N, by = .(anio, confirmacion)], anio ~ confirmacion, value.var = "N", fill = 0)
conf[, total := rowSums(.SD), .SDcols = -1][, pct_laboratorio := round(100 * Laboratorio / total, 1)]
fwrite(conf, "03_resultados/calidad_confirmacion_por_anio.csv")

# --- 4. Año de notificación vs año de inicio de síntomas -------------------------------
cs[, anio_inicio := as.integer(format(f_inicio, "%Y"))]
conc <- cs[, .(n = .N, mismo_anio = sum(anio_inicio == anio, na.rm = TRUE),
               inicio_anio_previo = sum(anio_inicio == anio - 1, na.rm = TRUE)), by = anio][order(anio)]
conc[, pct_concordante := round(100 * mismo_anio / n, 1)]
fwrite(conc, "03_resultados/calidad_concordancia_anio.csv")

# --- 5. Covariables municipales de TerriData (DNP) --------------------------------------
x <- fread("01_datos/raw/terridata_salud/extraido/TerriData_Dim5.txt", encoding = "UTF-8", nThread = 4)
setnames(x, c("cod_dep", "dep", "cod_ent", "entidad", "dim", "subcat", "indicador", "valor", "cual",
              "anio", "mes", "fuente", "unidad"))
x <- x[cod_ent >= 1000]                                    # solo municipios
x[, v := as.numeric(sub(",", ".", gsub(".", "", valor, fixed = TRUE)))]
x[, MPIO := sprintf("%05d", cod_ent)]
ind <- c(penta = "Cobertura vacunación pentavalente en menores de 1 año",
         cpn4 = "Porcentaje de nacidos vivos con 4 o más consultas de control prenatal",
         parto_cal = "Porcentaje de partos atendidos por personal calificado",
         subsidiado = "Cobertura del régimen subsidiado",
         tmi = "Tasa de mortalidad infantil en menores de 1 año (x cada 1.000 nacidos vivos)")
td <- x[indicador %in% ind, .(MPIO, anio, ind = names(ind)[match(indicador, ind)], v)]
# Serie anual de cobertura pentavalente: mediana municipal vs WUENIC nacional (detecta el quiebre de 2021+)
w <- fread("01_datos/raw/wuenic/wuenic_colombia.csv")[INDICATOR == "IM_DTP3", .(anio = TIME_PERIOD, wuenic_dtp3 = OBS_VALUE)]
qc_penta <- merge(td[ind == "penta", .(mediana_municipal = median(v, na.rm = TRUE),
                                       p10 = quantile(v, .1, na.rm = TRUE), p90 = quantile(v, .9, na.rm = TRUE)), by = anio],
                  w, by = "anio", all.x = TRUE)[order(anio)]
fwrite(qc_penta, "03_resultados/calidad_cobertura_terridata_vs_wuenic.csv")
# Covariables: promedios en periodos con datos coherentes
covm <- dcast(td[, .(v = mean(pmin(v, 150), na.rm = TRUE)), by = .(MPIO, ind,
          per = fcase(ind == "penta" & anio %in% 2015:2019, "1519",
                      ind == "penta" & anio %in% 2008:2010, "0810",
                      ind != "penta" & anio %in% 2019:2024, "1924",
                      ind != "penta" & anio %in% 2008:2010, "0810", default = NA_character_))][!is.na(per)],
          MPIO ~ ind + per, value.var = "v")
setnames(covm, gsub("_(\\d{4})$", "_\\1", names(covm)))
# v4: control prenatal de cada periodo, para el modelo de muerte (2025 usa 2020-2024, último dato disponible)
cpn_per <- td[ind == "cpn4", .(v = mean(pmin(v, 150), na.rm = TRUE)),
              by = .(MPIO, per = fcase(anio %in% 2011:2013, "1113", anio %in% 2014:2019, "1419",
                                       anio %in% 2020:2024, "2024", default = NA_character_))][!is.na(per)]
covm <- merge(covm, dcast(cpn_per, MPIO ~ paste0("cpn4_", per), value.var = "v"), by = "MPIO", all = TRUE)
saveRDS(covm, "01_datos/procesados/covariables_municipales_terridata.rds")

options(width = 160)
print(out$duplicados); print(falt); print(conf); print(conc[, .(anio, n, pct_concordante, inicio_anio_previo)])
print(qc_penta); print(summary(covm))
