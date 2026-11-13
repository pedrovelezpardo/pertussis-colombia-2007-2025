# =============================================================================
# 02_poblacion.R
# Lee los archivos oficiales del DANE (proyecciones/retroproyecciones CNPV 2018,
# versión post-COVID "_VP") y los convierte a formato largo:
#   pob_nac: año x sexo x edad simple (0-100+), área total
#   pob_dep: departamento x año x sexo x edad simple, área total + % rural
#   pob_mun: municipio x año x grupo de edad, área total + % rural
# Ejecutar desde la raíz del proyecto: Rscript 02_scripts/02_poblacion.R
# =============================================================================
suppressMessages({ library(readxl); library(data.table) })
raw <- "01_datos/raw/dane_proyecciones"
out <- "01_datos/procesados"
YEARS <- 2005:2026

# Lee una hoja DANE con encabezado de una o dos filas y devuelve tabla larga
read_dane <- function(f, sheet = NULL) {
  sh <- excel_sheets(f); if (is.null(sheet)) sheet <- length(sh)
  x <- as.data.table(read_excel(f, sheet = sheet, col_names = FALSE,
                                col_types = "text", .name_repair = "minimal"))
  setnames(x, paste0("V", seq_len(ncol(x))))
  hr <- which(apply(x[1:15], 1, function(r) any(r %in% c("AÑO"))))[1]
  h1 <- unlist(x[hr]); h2 <- unlist(x[hr + 1])
  two_rows <- any(grepl("^Hombres [0-9]", h2))
  nm <- if (two_rows) ifelse(is.na(h2), h1, h2) else h1
  nm[is.na(nm)] <- paste0("X", which(is.na(nm)))
  dat <- x[(hr + ifelse(two_rows, 2, 1)):.N]
  setnames(dat, make.unique(nm))
  id <- intersect(c("DP", "DPNOM", "DPMP", "MPIO", "AÑO", "ÁREA GEOGRÁFICA", "TERRITORIO"), names(dat))
  agecols <- grep("^(Hombres|Mujeres)[ _][0-9]", names(dat), value = TRUE)
  if (!length(agecols)) {               # archivo de totales (sin edad)
    pc <- grep("Población|Total", names(dat), value = TRUE)[1]
    d <- dat[, c(id, pc), with = FALSE]; setnames(d, pc, "pop")
    d[, pop := as.numeric(pop)]
    return(d[!is.na(pop)])
  }
  d <- melt(dat[, c(id, agecols), with = FALSE], id.vars = id,
            variable.name = "var", value.name = "pop", variable.factor = FALSE)
  d[, sexo := fifelse(grepl("^Hombres", var), "M", "F")]
  d[, edad := as.integer(sub("^(Hombres|Mujeres)[ _]([0-9]+).*$", "\\2", var))]
  d[, `:=`(var = NULL, pop = as.numeric(pop))]
  d[!is.na(pop)]
}

std <- function(d) {
  setnames(d, c("AÑO", "ÁREA GEOGRÁFICA"), c("anio", "area"), skip_absent = TRUE)
  d[, anio := as.integer(anio)]
  d[, area := fcase(grepl("^Cabecera", area), "cabecera",
                    grepl("^Centros", area), "rural",
                    grepl("^Total", area), "total", default = area)]
  d
}

# --- Nacional ------------------------------------------------------------------
pn <- rbind(std(read_dane(file.path(raw, "PPED-AreaSexoEdadNac-1950-2017.xlsx")))[, .(anio, area, sexo, edad, pop)],
            std(read_dane(file.path(raw, "PPED-AreaSexoEdadNac-2018-2070.xlsx")))[, .(anio, area, sexo, edad, pop)])
pn <- pn[anio %in% YEARS]
# Algunos archivos no traen fila "Total" por área: se reconstruye
pn_tot <- pn[area %in% c("cabecera", "rural"), .(pop = sum(pop)), by = .(anio, sexo, edad)]
pn_tot[, area := "total"]
pn <- rbind(pn[area != "total"], pn_tot)
saveRDS(pn, file.path(out, "pob_nac_edad_sexo.rds"))
cat("Nacional OK:", nrow(pn), "filas; población total 2025 =",
    format(pn[anio == 2025 & area == "total", sum(pop)], big.mark = ","), "\n")

# --- Departamental -------------------------------------------------------------
pdp <- rbind(std(read_dane(file.path(raw, "DCD-area-sexo-edad-proypoblacion-dep-2005-2017_VP.xlsx")))[, .(DP, anio, area, sexo, edad, pop)],
             std(read_dane(file.path(raw, "PPED-AreaSexoEdadDep-2018-2050_VP.xlsx")))[, .(DP, anio, area, sexo, edad, pop)])
pdp <- pdp[anio %in% YEARS & !is.na(DP)]
pdp_tot <- pdp[area %in% c("cabecera", "rural"), .(pop = sum(pop)), by = .(DP, anio, sexo, edad)]
pdp_tot[, area := "total"]
pdp <- rbind(pdp[area %in% c("cabecera", "rural")], pdp_tot)
saveRDS(pdp, file.path(out, "pob_dep_edad_sexo.rds"))
cat("Departamental OK:", uniqueN(pdp$DP), "departamentos\n")

# --- Municipal por edad (agregado a grupos) -------------------------------------
grp <- function(e) cut(e, c(-Inf, 0, 4, 9, 14, 19, 39, 59, Inf),
                       labels = c("0", "1-4", "5-9", "10-14", "15-19", "20-39", "40-59", "60+"))
pm_list <- lapply(c("DCD-area-sexo-edad-proypoblacion-Mun-2005-2017_VP.xlsx",
                    "PPED-AreaSexoEdadMun-2018-2042_VP.xlsx"), function(f) {
  d <- std(read_dane(file.path(raw, f)))
  d <- d[anio %in% YEARS]
  d[, grupo := grp(edad)]
  # En ambos archivos MPIO es el código DIVIPOLA de 5 dígitos (DPMP es el nombre)
  d[, .(pop = sum(pop)), by = .(DP, MPIO, anio, area, grupo)]
})
pm <- rbindlist(pm_list, use.names = TRUE)
pm[, MPIO := sprintf("%05d", as.integer(MPIO))]
pm_tot <- pm[area %in% c("cabecera", "rural"), .(pop = sum(pop)), by = .(DP, MPIO, anio, grupo)]
pm_tot[, area := "total"]
pm <- rbind(pm[area %in% c("cabecera", "rural")], pm_tot)
saveRDS(pm, file.path(out, "pob_mun_grupos.rds"))
cat("Municipal OK:", uniqueN(pm$MPIO), "municipios;", paste(range(pm$anio), collapse = "-"), "\n")
