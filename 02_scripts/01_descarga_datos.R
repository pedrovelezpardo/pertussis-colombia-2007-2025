# =============================================================================
# 01_descarga_datos.R
# Descarga las fuentes públicas crudas en 01_datos/raw/ desde las URL oficiales usadas
# el 3/11/2026. El repositorio incluye esa copia, salvo las proyecciones de población del
# DANE (~195 MB), así que por defecto solo baja lo que falta y descomprime TerriData.
#   Rscript 02_scripts/01_descarga_datos.R              solo archivos faltantes
#   Rscript 02_scripts/01_descarga_datos.R --todo       vuelve a descargar todo (el INS actualiza
#                                                       los microdatos: los conteos pueden cambiar)
#   Rscript 02_scripts/01_descarga_datos.R --verificar  solo comprueba que las URL respondan
# Al final compara cada archivo con 01_datos/raw/checksums_md5.csv (copia del 3/11/2026);
# si ese archivo no existe, lo crea.
# Ejecutar desde la raíz del proyecto.
# =============================================================================
args <- commandArgs(trailingOnly = TRUE)
TODO <- "--todo" %in% args
VERIFICAR <- "--verificar" %in% args
options(timeout = 3600)
raw <- "01_datos/raw"

dane <- "https://www.dane.gov.co/files/censo2018/proyecciones-de-poblacion/"
proy <- c("Nacional/PPED-AreaSexoEdadNac-1950-2017.xlsx", "Nacional/PPED-AreaSexoEdadNac-2018-2070.xlsx",
          "Departamental/DCD-area-sexo-edad-proypoblacion-dep-2005-2017_VP.xlsx",
          "Departamental/PPED-AreaSexoEdadDep-2018-2050_VP.xlsx",
          "Municipal/DCD-area-sexo-edad-proypoblacion-Mun-2005-2017_VP.xlsx",
          "Municipal/PPED-AreaSexoEdadMun-2018-2042_VP.xlsx",
          "Municipal/DCD-area-proypoblacion-Mun-2005-2017_VP.xlsx", "Municipal/PPED-AreaMun-2018-2042_VP.xlsx")
fuentes <- rbind(
  data.frame(archivo = sprintf("sivigila_800/Datos_%d_800.xlsx", 2007:2025),
             url = sprintf("https://portalsivigila.ins.gov.co/_api/Web/GetFileByServerRelativeUrl('/Microdatos/Datos_%d_800.xlsx')/$value", 2007:2025)),
  data.frame(archivo = paste0("dane_proyecciones/", basename(proy)), url = paste0(dane, proy)),
  data.frame(archivo = "dane_etnia/anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun.xlsx",
             url = paste0(dane, "Nacional/anex-DCD-Proypoblacion-PerteneniaEtnicoRacialmun.xlsx")),
  data.frame(archivo = "dane_ipm/anexo-censal-pobreza-municipal-2018.xlsx",
             url = "https://www.dane.gov.co/files/investigaciones/condiciones_vida/pobreza/2018/informacion-censal/anexo-censal-pobreza-municipal-2018.xlsx"),
  data.frame(archivo = "dane_mgn/mgn2018_mpio_simpl.geojson",   # MGN 2018 simplificado (tolerancia 0,002°)
             url = "https://portalgis.dane.gov.co/mparcgis/rest/services/MGN2018/Serv_CapaMunicipiosInt_2018/MapServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&maxAllowableOffset=0.002&geometryPrecision=5&outSR=4326&f=geojson"),
  data.frame(archivo = "terridata_salud/TerriData_Dim5.txt.zip",
             url = "https://terridata.dnp.gov.co/assets/docs/txt/dimensiones/TerriData_Dim5.txt.zip"),
  data.frame(archivo = "wuenic/wuenic_colombia.csv",
             url = "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,IMMUNISATION,1.0/COL.IM_DTP1+IM_DTP3+IM_MCV1+IM_HEPB3+IM_HIB3.?startPeriod=2000&format=csv"),
  data.frame(archivo = "minsalud_coberturas/coberturas_departamento_6i25-2hdt.csv",
             url = "https://www.datos.gov.co/resource/6i25-2hdt.csv?$limit=5000"))

# --- 1. Descarga -------------------------------------------------------------------
# Verificación: lee los primeros bytes (el portal del SIVIGILA rechaza las solicitudes HEAD)
responde <- function(u) {
  con <- tryCatch(url(u, "rb", method = "libcurl"), error = function(e) conditionMessage(e),
                  warning = function(w) conditionMessage(w))
  if (!inherits(con, "connection")) return(con)
  on.exit(close(con))
  if (length(readBin(con, "raw", 64)) > 0) "OK" else "sin contenido"
}
for (i in seq_len(nrow(fuentes))) {
  f <- file.path(raw, fuentes$archivo[i]); u <- fuentes$url[i]
  if (VERIFICAR) { cat(sprintf("%-72s %s\n", fuentes$archivo[i], responde(u))); next }
  if (file.exists(f) && !TODO) next
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(f, ".part")
  ok <- tryCatch(download.file(u, tmp, mode = "wb", quiet = TRUE) == 0, error = function(e) FALSE) &&
    isTRUE(file.size(tmp) > 0)
  if (ok) file.rename(tmp, f) else unlink(tmp)
  cat(if (ok) "Descargado:" else "ERROR al descargar:", fuentes$archivo[i], "\n")
}
if (VERIFICAR) quit(save = "no")

# --- 2. TerriData: el script 08 lee el texto descomprimido ------------------------------
z <- file.path(raw, "terridata_salud/TerriData_Dim5.txt.zip")
txt <- file.path(raw, "terridata_salud/extraido/TerriData_Dim5.txt")
if (file.exists(z) && (TODO || !file.exists(txt))) unzip(z, exdir = dirname(txt))

# --- 3. Comparación con la copia del 3/11/2026 --------------------------------------------
man <- file.path(raw, "checksums_md5.csv")
f <- file.path(raw, fuentes$archivo)
act <- data.frame(archivo = fuentes$archivo, bytes = file.size(f), md5 = unname(tools::md5sum(f)))
if (!file.exists(man)) {
  write.csv(act, man, row.names = FALSE)
  cat("Se creó", man, "con", nrow(act), "archivos\n")
} else {
  ref <- read.csv(man)
  cmp <- merge(ref, act, by = "archivo", suffixes = c("_ref", ""), all = TRUE)
  cmp$estado <- ifelse(is.na(cmp$md5), "falta", ifelse(cmp$md5 == cmp$md5_ref, "igual", "distinto"))
  print(table(cmp$estado))
  if (any(cmp$estado != "igual"))
    print(cmp[cmp$estado != "igual", c("archivo", "bytes_ref", "bytes", "estado")], row.names = FALSE)
}
