# =============================================================================
# 03_sivigila_limpieza.R
# Depuración de microdatos SIVIGILA, evento 800 (tos ferina), 2007-2025.
# Los microdatos públicos del INS contienen solo casos con clasificación final
# confirmada (laboratorio, clínica o nexo epidemiológico).
# Salida: 01_datos/procesados/tosferina_casos_2007_2025.rds (+ .csv)
# =============================================================================
suppressMessages({ library(readxl); library(data.table) })
fs <- sprintf("01_datos/raw/sivigila_800/Datos_%d_800.xlsx", 2007:2025)
d <- rbindlist(lapply(fs, function(f) as.data.table(read_excel(f, col_types = "text"))),
               use.names = TRUE, fill = TRUE)

# Fechas con formato "d/m/aaaa hh:mm:ss a. m."
pdate <- function(x) {
  m <- regmatches(x, regexec("^(\\d{1,2})/(\\d{1,2})/(\\d{4})", x))
  as.IDate(vapply(m, function(z) if (length(z) == 4) sprintf("%s-%02d-%02d", z[4], as.integer(z[3]), as.integer(z[2])) else NA_character_, ""))
}
for (v in c("FEC_NOT", "INI_SIN", "FEC_CON", "FEC_HOS", "FEC_DEF", "FECHA_NTO"))
  set(d, j = tolower(v), value = pdate(d[[v]]))

d[, anio := as.integer(ANO)]
d[, semana := as.integer(SEMANA)]

# --- Edad ----------------------------------------------------------------------
# UNI_MED: 1 años, 2 meses, 3 días, 4 horas, 5 minutos (0 = sin dato)
d[, edad_num := as.numeric(EDAD)]
d[, edad_dias_rep := fcase(UNI_MED == "1", edad_num * 365.25, UNI_MED == "2", edad_num * 30.4375,
                           UNI_MED == "3", edad_num, UNI_MED == "4", edad_num / 24,
                           UNI_MED == "5", edad_num / 1440, default = NA_real_)]
# Edad calculada con fecha de nacimiento y fecha de consulta (como la calcula SIVIGILA)
d[, edad_dias_fn := as.numeric(fec_con - fecha_nto)]
d[edad_dias_fn < 0 | edad_dias_fn > 110 * 365.25, edad_dias_fn := NA]
# Se prefiere la fecha de nacimiento si es coherente con la edad reportada
d[, edad_dias := fifelse(!is.na(edad_dias_fn) &
                           (is.na(edad_dias_rep) | abs(edad_dias_fn - edad_dias_rep) <= pmax(31, 0.10 * edad_dias_rep) |
                              (UNI_MED == "1" & floor(edad_dias_fn / 365.25) == edad_num)),
                         edad_dias_fn, edad_dias_rep)]
d[, edad_anios := floor(edad_dias / 365.25)]
d[, edad_meses := floor(edad_dias / 30.4375)]
# Intervalos semiabiertos [a, b) en días, coherentes con los modelos (<61 días = <2 meses)
d[, grupo_edad := cut(edad_dias,
  c(-Inf, 61, 122, 183, 365.25, 5 * 365.25, 10 * 365.25, 15 * 365.25, 20 * 365.25, 40 * 365.25, 60 * 365.25, Inf),
  right = FALSE,
  labels = c("<2 m", "2-3 m", "4-5 m", "6-11 m", "1-4 a", "5-9 a", "10-14 a", "15-19 a",
             "20-39 a", "40-59 a", "60+ a"))]
d[, grupo_pob := cut(edad_anios, c(-Inf, 0, 4, 9, 14, 19, 39, 59, Inf),
                     labels = c("0", "1-4", "5-9", "10-14", "15-19", "20-39", "40-59", "60+"))]
d[, lactante := edad_anios == 0]
d[, cohorte := as.integer(format(fifelse(!is.na(fecha_nto) & !is.na(edad_dias_fn) & edad_dias == edad_dias_fn,
                                          fecha_nto, as.IDate(fec_con - round(edad_dias))), "%Y"))]

# --- Fechas clínicas y retrasos ---------------------------------------------------
d[, f_inicio := fifelse(!is.na(ini_sin) & ini_sin <= fec_con & (fec_con - ini_sin) <= 120, ini_sin, fec_con)]
d[, ret_consulta := as.numeric(fec_con - ini_sin)]
d[ret_consulta < 0 | ret_consulta > 120, ret_consulta := NA]
d[, ret_notif := as.numeric(fec_not - fec_con)]
d[ret_notif < 0 | ret_notif > 180, ret_notif := NA]

# --- Desenlaces ------------------------------------------------------------------
d[, muerte := CON_FIN == "2"]
d[, hospitalizado := fifelse(anio %in% c(2008, 2009), NA, PAC_HOS == "1")]   # sin dato real en 2008-2009

# --- Variables sociales ------------------------------------------------------------
d[, sexo := fifelse(SEXO == "F", "Mujer", fifelse(SEXO == "M", "Hombre", NA_character_))]
d[, regimen := fcase(TIP_SS == "C", "Contributivo", TIP_SS == "S", "Subsidiado",
                     TIP_SS %in% c("P", "E"), "Especial/Excepción", TIP_SS == "N", "No asegurado",
                     default = "Indeterminado")]
d[, etnia := fcase(PER_ETN == "1", "Indígena", PER_ETN %in% c("3", "4", "5"), "Afrodescendiente",
                   PER_ETN == "2", "Rrom", default = "Otro/ninguno")]
d[, area := fcase(AREA == "1", "Cabecera", AREA %in% c("2", "3"), "Rural", default = NA_character_)]
d[, migrante := GP_MIGRANT == "1"]
d[, gestante := GP_GESTAN == "1"]
d[, confirmacion := fcase(Estado_final_de_caso == "3", "Laboratorio", Estado_final_de_caso == "4", "Clínica",
                          Estado_final_de_caso == "5", "Nexo epidemiológico")]

# --- Geografía de residencia ----------------------------------------------------------
d[, dp_res := COD_DPTO_R]
d[, mpio_res := paste0(COD_DPTO_R, COD_MUN_R)]
d[!grepl("^[0-9]{5}$", mpio_res) | dp_res %in% c("00", "01"), `:=`(mpio_res = NA, dp_res = NA)]
d[, dp_nom := Departamento_residencia]; d[, mpio_nom := Municipio_residencia]

# --- Periodos analíticos ---------------------------------------------------------------
d[, periodo := fcase(anio <= 2010, "2007-2010", anio <= 2013, "2011-2013 (epidemia)",
                     anio <= 2019, "2014-2019", anio <= 2024, "2020-2024 (pandemia/postpandemia)",
                     anio == 2025, "2025 (epidemia)")]

keep <- c("CONSECUTIVE", "anio", "semana", "fec_not", "ini_sin", "fec_con", "fec_hos", "fec_def",
          "fecha_nto", "f_inicio", "edad_dias", "edad_meses", "edad_anios", "grupo_edad", "grupo_pob",
          "lactante", "cohorte", "sexo", "regimen", "etnia", "area", "migrante", "gestante",
          "confirmacion", "muerte", "hospitalizado", "ret_consulta", "ret_notif", "dp_res", "mpio_res",
          "dp_nom", "mpio_nom", "periodo", "Nombre_upgd", "estrato")
casos <- d[, ..keep]
saveRDS(casos, "01_datos/procesados/tosferina_casos_2007_2025.rds")
fwrite(casos, "01_datos/procesados/tosferina_casos_2007_2025.csv")

# --- Control de calidad ----------------------------------------------------------------
cat("Casos:", nrow(casos), "| muertes:", sum(casos$muerte), "\n")
cat("Edad faltante:", sum(is.na(casos$edad_dias)), "| municipio faltante:", sum(is.na(casos$mpio_res)), "\n")
cat("Edad desde fecha de nacimiento:", d[, sum(!is.na(edad_dias_fn) & edad_dias == edad_dias_fn)], "\n")
print(casos[, .(casos = .N, lact = sum(lactante, na.rm = TRUE), m2 = sum(grupo_edad == "<2 m", na.rm = TRUE),
                muertes = sum(muerte), mediana_edad_m = median(edad_meses, na.rm = TRUE)), by = anio][order(anio)])
