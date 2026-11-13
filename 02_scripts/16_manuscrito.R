# =============================================================================
# 16_manuscrito.R
# Convierte 05_manuscrito/manuscrito_fuente.txt en:
#   Manuscrito_Frontiers_tosferina_vN.docx   (texto, referencias, leyendas de figuras y tablas 1-4 al final)
#   Material_suplementario_tosferina_vN.docx (métodos suplementarios, tablas S1-S17, figuras S1-S6)
# v4 (12-11-2026): tablas 2 y 4 nuevas (dos líneas base; OR crudos y muertes por categoría), tabla 3 con
# Holm en la nota, 17 tablas y 6 figuras suplementarias en el orden en que el texto las cita.
# Formato de Frontiers in Public Health (guía oficial leída el 12-11-2026): interlineado sencillo,
# números de página y de línea, tablas al final del manuscrito, figuras subidas aparte (aquí solo
# sus leyendas) y material suplementario sin título ni autores.
# Estilos de Word de la plantilla del kit (referencia_kit.docx), sin formato directo. Las ecuaciones
# se escriben en LaTeX en la fuente y el conversor del kit las pasa a ecuaciones nativas de Word.
# Las citas [@clave] se numeran por orden de aparición (Frontiers Vancouver).
# Las tablas se construyen directamente desde 03_resultados/ (sin transcripción manual).
# Ejecutar desde la raíz: Rscript 02_scripts/16_manuscrito.R
# Si un Word de salida ya existe, el script se detiene (pudo editarse a mano); REGENERAR=1 lo
# reconstruye de todos modos.
# =============================================================================
suppressMessages({ library(officer); library(data.table); library(jsonlite); library(sf) })
suppressMessages(source(path.expand("~/herramientas/kit-investigacion/analisis/kit_analisis.R")))  # .tabla_xml(), .parrafo(), .REFERENCIA_DOCX
VERSION <- "v5"       # manuscrito (v5: armado sobre la plantilla de Word de Frontiers)
VERSION_SUP <- "v4"   # material suplementario (v4: sobre la plantilla de Frontiers para suplementos)
VERSION_REC <- "v3"   # lista RECORD (v3: sobre la plantilla de Frontiers para suplementos)
# Frontiers pide usar sus plantillas: «If working with Word please use our Word templates» (guía de autores,
# sección Templates, leída el 12-11-2026). Se trabaja sobre las copias vacías del kit, hechas con
# plantillas/revistas/preparar_plantilla.py: conservan estilos, página carta, numeración de líneas, títulos
# numerados, encabezados y pies de la plantilla oficial; solo se quita su texto de ejemplo.
PLANTILLA_MS <- path.expand("~/herramientas/kit-investigacion/plantillas/revistas/frontiers/Frontiers_Template_vacia_kit.docx")
PLANTILLA_SUP <- path.expand("~/herramientas/kit-investigacion/plantillas/revistas/frontiers/Frontiers_Supplementary_Material_vacia_kit.docx")
ANCHO_TEXTO <- 12240L - 1282L - 1181L        # dxa: página carta menos los márgenes de la plantilla
ANCHO_HORIZONTAL <- 15840L - 1282L - 1181L
CONVERTIR <- path.expand("~/herramientas/kit-investigacion/conversores/convertir")
R <- function(f) fread(file.path("03_resultados", f), encoding = "UTF-8")

TITLE <- "Pertussis in Colombia before and after the introduction of maternal pertussis vaccination, a nationwide analysis of surveillance microdata, 2007–2025"
AUTH <- list(list("Doris Cardona", "1", TRUE), list("Angela Segura Cardona", "2", FALSE),
             list("Alejandra Segura Cardona", "2", FALSE), list("Pedro Velez-Pardo", "2", TRUE))
AFF <- c("1" = "Independent Researcher, Medellín, Colombia", "2" = "Universidad CES, Medellín, Colombia")
CORR <- "Doris Cardona, doris.cardona@gmail.com; Pedro Velez-Pardo, pvelez@ces.edu.co"
CORRESP <- list(c("Doris Cardona", "doris.cardona@gmail.com"), c("Pedro Velez-Pardo", "pvelez@ces.edu.co"))
KEYW <- "pertussis, maternal immunization, Tdap, immunity gap, health inequities, Indigenous peoples, Bayesian spatial analysis, Colombia."   # como en la plantilla

out_main <- sprintf("Manuscrito_Frontiers_tosferina_%s.docx", VERSION)
out_sup <- sprintf("Material_suplementario_tosferina_%s.docx", VERSION_SUP)
out_rec <- sprintf("Lista_RECORD_tosferina_%s.docx", VERSION_REC)
for (f in c(out_main, out_sup, out_rec)) {
  if (file.exists(file.path(dirname(f), paste0("~$", basename(f))))) stop(f, " está abierto en Word: guárdelo y ciérrelo.")
  if (file.exists(f) && !nzchar(Sys.getenv("REGENERAR"))) stop(f, " ya existe y pudo editarse a mano; use REGENERAR=1 para reconstruirlo.")
}
build <- "05_manuscrito/build"; dir.create(build, showWarnings = FALSE)

# --------------------------------------------------------------------------------
# Texto fuente y citas
# --------------------------------------------------------------------------------
src <- readLines("05_manuscrito/manuscrito_fuente.txt", encoding = "UTF-8", warn = FALSE)
refs <- fromJSON("05_manuscrito/referencias.json")
all_txt <- paste(src, collapse = "\n")
cites <- regmatches(all_txt, gregexpr("\\[@[^]]+\\]", all_txt))[[1]]
keys_in_order <- unique(unlist(lapply(cites, function(m) sub("^@", "", trimws(strsplit(gsub("^\\[|\\]$", "", m), ";")[[1]])))))
missing <- setdiff(keys_in_order, names(refs))
if (length(missing)) stop("Claves sin referencia: ", paste(missing, collapse = ", "))
num <- setNames(seq_along(keys_in_order), keys_in_order)
compress <- function(v) {
  v <- sort(unique(v)); out <- character(); i <- 1
  while (i <= length(v)) { j <- i; while (j < length(v) && v[j + 1] == v[j] + 1) j <- j + 1
    out <- c(out, if (j - i >= 2) paste0(v[i], "–", v[j]) else as.character(v[i:j])); i <- j + 1 }
  paste0("(", paste(out, collapse = ", "), ")")
}
cite_repl <- function(line) {
  m <- gregexpr("\\[@[^]]+\\]", line)[[1]]
  if (m[1] == -1) return(line)
  ms <- regmatches(line, list(m))[[1]]
  for (x in ms) { k <- sub("^@", "", trimws(strsplit(gsub("^\\[|\\]$", "", x), ";")[[1]])); line <- sub(x, compress(num[k]), line, fixed = TRUE) }
  line
}
src <- vapply(src, cite_repl, "", USE.NAMES = FALSE)

# Recuento de palabras como Frontiers: texto principal sin resumen, títulos, ecuaciones en bloque,
# leyendas, tablas, declaraciones ni referencias
sec_start <- function(pat) which(grepl(pat, src))[1]
wc <- function(x) sum(lengths(strsplit(trimws(gsub("\\*", "", x[nzchar(trimws(x)) & !grepl("^#|^\\$\\$", x)])), "\\s+")))
abs_words <- wc(src[(sec_start("^# Abstract") + 1):(sec_start("^# 1 Introduction") - 1)])
main_words <- wc(src[sec_start("^# 1 Introduction"):(sec_start("^# Data availability") - 1)])

# --------------------------------------------------------------------------------
# Utilidades: todo el formato sale de los estilos de la plantilla; en el texto solo hay negrita,
# cursiva y superíndice
# --------------------------------------------------------------------------------
runs <- function(s) {                      # **negrita** y *cursiva*
  toks <- regmatches(s, gregexpr("\\*\\*[^*]+\\*\\*|\\*[^*]+\\*|[^*]+", s))[[1]]
  lapply(toks, function(t) {
    if (grepl("^\\*\\*.*\\*\\*$", t)) ftext(gsub("^\\*\\*|\\*\\*$", "", t), fp_text_lite(bold = TRUE))
    else if (grepl("^\\*.*\\*$", t)) ftext(gsub("^\\*|\\*$", "", t), fp_text_lite(italic = TRUE))
    else ftext(t, prop = NULL)
  })
}
P <- function(doc, s, style = "Normal") body_add_fpar(doc, do.call(fpar, runs(s)), style = style)
H <- function(doc, s, lvl) body_add_par(doc, s, style = paste("heading", lvl))
etiqueta <- function(doc, rotulo, texto, style) body_add_fpar(doc, do.call(fpar, c(list(ftext(rotulo, fp_text_lite(bold = TRUE))), runs(paste0(" ", texto)))), style = style)
en_num <- function(x) {                    # separador de miles en celdas de texto (inglés)
  vapply(x, function(s) { if (is.na(s)) return(s)
    m <- gregexpr("(?<![\\d.])\\d{4,}(?![\\d.])", s, perl = TRUE)
    regmatches(s, m) <- list(formatC(as.numeric(regmatches(s, m)[[1]]), format = "d", big.mark = ","))
    gsub("(\\d)-(\\d)", "\\1–\\2", s) }, "", USE.NAMES = FALSE)
}
ci <- function(e, l, h, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f–%.", d, "f)"), e, l, h)
# Tabla APA del kit (estilos «Kit», celdas de 9 pt). Los anchos se calculan aquí: ninguna palabra
# del encabezado se corta, las cifras no se parten y, si la tabla no cabe, se encogen primero las
# columnas con holgura (sobre todo la de etiquetas, que puede ocupar dos líneas). En dxa: 105 por
# carácter de Arial 9 y 240 de márgenes de celda.
anchos_tabla <- function(celdas, encabezados, ancho_max, partir_ic = FALSE, por_car = 105, margen = 240, max_primera = 3000, max_col = 4800) {
  palabra <- vapply(encabezados, function(h) max(nchar(strsplit(h, "[ /]")[[1]])), 1)
  minimo <- palabra * por_car + margen + 60   # letras anchas (W, O, M) en los encabezados
  largo <- function(v) max(nchar(if (partir_ic) unlist(strsplit(v, " (?=\\()", perl = TRUE)) else v), 1)
  pref <- pmax(vapply(celdas, largo, 1) * por_car + margen, minimo)
  pref[1] <- min(pref[1], max(max_primera, minimo[1]))
  pref <- pmin(pref, pmax(max_col, minimo))
  if (sum(pref) > ancho_max) {
    holgura <- pref - minimo; falta <- sum(pref) - ancho_max
    pref <- if (sum(holgura) >= falta) pref - holgura * falta / sum(holgura) else minimo * ancho_max / sum(minimo)
  }
  as.integer(floor(pref))
}
# partir_ic = TRUE pone el intervalo debajo del estimador («0.33» / «(0.12–0.88)») en tablas que no
# caben de otro modo en una página vertical
# izquierda = TRUE: tablas de texto (campos y códigos, lista RECORD), alineadas a la izquierda y arriba
TABLA <- function(doc, dt, rotulo, titulo, nota = NULL, ancho_max = ANCHO_TEXTO, partir_ic = FALSE, izquierda = FALSE) {
  doc <- etiqueta(doc, rotulo, titulo, "caption")
  celdas <- lapply(dt, function(v) ifelse(is.na(v), "—", as.character(v)))
  x <- .tabla_xml(as.data.frame(celdas, check.names = FALSE), names(dt))
  if (partir_ic) x <- gsub('(<w:t xml:space="preserve">[−-]?[0-9][0-9.,]*%?) \\(', '\\1</w:t><w:br/><w:t xml:space="preserve">(', x, perl = TRUE)
  if (izquierda) x <- gsub('<w:vAlign w:val="bottom"/>', '<w:vAlign w:val="top"/>', gsub('w:val="KitCelda"', 'w:val="KitCeldaPrimera"', gsub('w:val="KitEncabezado"', 'w:val="KitCeldaPrimera"', x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  w <- anchos_tabla(celdas, names(dt), ancho_max, partir_ic)
  m <- gregexpr('(?<=<w:gridCol w:w=")\\d+', x, perl = TRUE); regmatches(x, m) <- list(as.character(w))
  m <- gregexpr('(?<=<w:tcW w:w=")\\d+', x, perl = TRUE); regmatches(x, m) <- list(as.character(rep(w, times = nrow(dt) + 1)))
  doc <- body_add_xml(doc, x)
  if (!is.null(nota)) doc <- body_add_xml(doc, .parrafo(nota, "KitNota"))
  body_add_par(doc, "", style = "Normal")
}
# Ecuaciones: el conversor del kit reemplaza el LaTeX ($…$ y $$…$$) por ecuaciones nativas y
# comprueba que el resto del texto quede idéntico
ecuaciones_nativas <- function(entrada, salida) {
  if (file.exists(salida)) unlink(salida)
  r <- suppressWarnings(system2(CONVERTIR, c("insertar", shQuote(entrada), "--salida", shQuote(salida)), stdout = TRUE, stderr = TRUE))
  st <- attr(r, "status"); if (is.null(st)) st <- 0
  if (st != 0 || !file.exists(salida)) stop("El conversor de ecuaciones falló (", st, "):\n", paste(r, collapse = "\n"))
  cat(grep("ecuaci|Ecuaci", r, value = TRUE), sep = "\n")
}
# Ajustes finales en el .docx: celdas y notas de tabla a 9 pt (en sus estilos), pie de página fuera de
# la numeración de líneas y, en el manuscrito, numeración continua de líneas (requisito de Frontiers)
# Secciones en documentos hechos sobre una plantilla: las secciones que officer agrega (por ejemplo, la horizontal
# de las tablas) toman los encabezados, pies y márgenes de la sección de la plantilla; la portada de la plantilla
# (encabezado «first») queda solo en la primera página.
secciones_plantilla <- function(x) {
  m <- gregexpr("(?s)<w:sectPr\\b[^>]*>.*?</w:sectPr>", x, perl = TRUE, useBytes = TRUE)
  secs <- regmatches(x, m)[[1]]
  if (length(secs) < 2) return(x)
  final <- secs[length(secs)]
  refs <- regmatches(final, gregexpr("<w:(?:header|footer)Reference[^>]*/>", final, perl = TRUE, useBytes = TRUE))[[1]]
  pgmar <- regmatches(final, regexpr("<w:pgMar[^>]*/>", final, perl = TRUE, useBytes = TRUE))
  nuevos <- vapply(seq_along(secs), function(i) {
    b <- gsub("<w:(?:header|footer)Reference[^>]*/>", "", secs[i], perl = TRUE, useBytes = TRUE)
    b <- gsub("<w:titlePg/>", "", b, fixed = TRUE)
    b <- if (grepl("<w:pgMar", b, fixed = TRUE)) sub("<w:pgMar[^>]*/>", pgmar, b, perl = TRUE, useBytes = TRUE) else sub("</w:sectPr>", paste0(pgmar, "</w:sectPr>"), b, fixed = TRUE)
    r <- if (i == 1) refs else refs[!grepl('w:type="first"', refs, fixed = TRUE)]
    b <- sub("^(<w:sectPr\\b[^>]*>)", paste0("\\1", paste(r, collapse = "")), b, perl = TRUE, useBytes = TRUE)
    if (i == 1) b <- sub("</w:sectPr>", "<w:titlePg/></w:sectPr>", b, fixed = TRUE)
    b
  }, "", USE.NAMES = FALSE)
  regmatches(x, m) <- list(nuevos)
  x
}
ajustar_docx <- function(f, numerar_lineas, plantilla = FALSE) {
  td <- tempfile("docx_"); dir.create(td); unzip(f, exdir = td)
  leer <- function(p) paste(readLines(file.path(td, p), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  escribir <- function(x, p) writeLines(x, file.path(td, p), useBytes = TRUE)
  # officer deja un comments.xml vacío y sin relación cuando la plantilla no lo trae: se quita (y su tipo)
  cm <- file.path(td, "word", "comments.xml")
  if (file.exists(cm) && !grepl("comments.xml", leer("word/_rels/document.xml.rels"), fixed = TRUE) &&
      !grepl("<w:comment ", leer("word/comments.xml"), fixed = TRUE)) {
    unlink(cm)
    escribir(gsub('<Override[^>]*PartName="/word/comments.xml"[^>]*/>', "", leer("[Content_Types].xml"), perl = TRUE), "[Content_Types].xml")
  }
  st <- leer("word/styles.xml")
  for (id in c("KitCelda", "KitEncabezado", "KitCeldaPrimera", "KitCeldaSangria", "KitNota")) {
    m <- regexpr(sprintf('(?s)<w:style [^>]*w:styleId="%s".*?</w:style>', id), st, perl = TRUE, useBytes = TRUE)
    if (m > 0) regmatches(st, m) <- gsub('(?<=<w:sz w:val=")\\d+|(?<=<w:szCs w:val=")\\d+', "18", regmatches(st, m), perl = TRUE, useBytes = TRUE)
  }
  escribir(st, "word/styles.xml")
  # Pie de página: propiedades válidas según el esquema y fuera de la numeración de líneas (solo en la referencia
  # del kit; los pies de una plantilla de revista no se tocan)
  if (!plantilla) for (p in list.files(file.path(td, "word"), pattern = "^footer\\d*\\.xml$")) {
    x <- leer(file.path("word", p))
    x <- gsub("(?s)<w:pPr>.*?</w:pPr>", '<w:pPr><w:pStyle w:val="Normal"/><w:suppressLineNumbers/><w:jc w:val="center"/></w:pPr>', x, perl = TRUE)
    escribir(gsub('(<w:instrText[^>]*?) w:dirty="true"', "\\1", x, perl = TRUE), file.path("word", p))
  }
  x <- leer("word/document.xml")
  if (numerar_lineas && !grepl("lnNumType", x)) x <- sub("</w:sectPr>", '<w:lnNumType w:countBy="1" w:restart="continuous"/></w:sectPr>', x)
  if (plantilla) x <- secciones_plantilla(x)
  # Secciones: officer deja pgMar antes de pgSz y referencias al pie con espacios; se ordenan según el esquema
  orden <- c("headerReference", "footerReference", "footnotePr", "endnotePr", "type", "pgSz", "pgMar", "paperSrc", "pgBorders",
             "lnNumType", "pgNumType", "cols", "formProt", "vAlign", "noEndnote", "titlePg", "textDirection", "bidi", "rtlGutter",
             "docGrid", "printerSettings", "sectPrChange")
  x <- gsub("(<w:(?:header|footer)Reference[^>]*?)>\\s*</w:(?:header|footer)Reference>", "\\1/>", x, perl = TRUE)
  m <- gregexpr("(?s)<w:sectPr\\b[^>]*>.*?</w:sectPr>", x, perl = TRUE, useBytes = TRUE)   # posiciones en bytes: el XML tiene tildes
  regmatches(x, m) <- list(vapply(regmatches(x, m)[[1]], function(b) {
    ini <- sub("(?s)^(<w:sectPr\\b[^>]*>).*$", "\\1", b, perl = TRUE)
    resto <- sub("(?s)^<w:sectPr\\b[^>]*>", "", b, perl = TRUE, useBytes = TRUE)
    hijos <- regmatches(resto, gregexpr("(?s)<w:(\\w+)\\b[^>]*?(?:/>|>.*?</w:\\1>)", resto, perl = TRUE, useBytes = TRUE))[[1]]
    paste0(ini, paste(hijos[order(match(sub("^<w:(\\w+).*", "\\1", hijos), orden))], collapse = ""), "</w:sectPr>")
  }, "", USE.NAMES = FALSE))
  escribir(x, "word/document.xml")
  owd <- setwd(td); on.exit(setwd(owd))
  unlink(file.path(owd, f)); zip(file.path(owd, f), files = list.files(".", recursive = TRUE, all.files = TRUE), flags = "-r9Xq")
}

# --------------------------------------------------------------------------------
# Tablas del manuscrito
# --------------------------------------------------------------------------------
t1 <- R("tabla1_caracteristicas_periodo.csv")
lab1 <- c("Casos confirmados" = "Confirmed cases, n", "Edad mediana, meses (RIC)" = "Median age, months (IQR)",
          "<2 meses, n (%)" = "Age <2 months, n (%)", "<1 año, n (%)" = "Age <1 year, n (%)", "1-9 años, n (%)" = "Age 1–9 years, n (%)",
          ">=10 años, n (%)" = "Age ≥10 years, n (%)", "Mujeres, n (%)" = "Female, n (%)", "Régimen subsidiado, n (%)" = "Subsidized insurance, n (%)",
          "No asegurado, n (%)" = "Uninsured, n (%)", "Indígena, n (%)" = "Indigenous, n (%)", "Afrodescendiente, n (%)" = "Afro-descendant, n (%)",
          "Rural, n (%)" = "Rural residence, n (%)", "Confirmación por laboratorio, n (%)" = "Laboratory-confirmed, n (%)",
          "Hospitalizados, n (%)" = "Hospitalized, n (%)ᵃ", "Muertes, n" = "Deaths, n", "Letalidad en <1 año, %" = "Infant case fatality, %",
          "Letalidad en <2 meses, %" = "Case fatality, age <2 months, %", "Días inicio-consulta, mediana (RIC)" = "Days from onset to consultation, median (IQR)",
          "Incidencia anual media por 100.000" = "Mean annual incidence per 100,000", "Incidencia <1 año por 100.000" = "Incidence <1 year per 100,000",
          "Incidencia <2 meses por 100.000" = "Incidence <2 months per 100,000")
T1 <- copy(t1)[, Variable := lab1[Variable]]
for (j in names(T1)[-1]) set(T1, j = j, value = en_num(trimws(T1[[j]])))
setnames(T1, c("Characteristic", "2007–2010", "2011–2013 (epidemic)", "2014–2019", "2020–2024", "2025 (epidemic)"))

# Tabla 2: las dos líneas base lado a lado
bl <- c("All ages" = "All ages", "<1 year" = "<1 year", "1–9 years" = "1–9 years", "10+ years" = "≥10 years")
fmtn <- function(x) formatC(round(x), format = "d", big.mark = ",")   # sin relleno de espacios
pct <- function(x) paste0(gsub("^-", "−", formatC(round(100 * x), format = "d", big.mark = ",")), "%")
de_all <- R("series_deficit_exceso.csv")[periodo != "Balance 2020-2025"]
mk2 <- function(esp) de_all[especificacion == esp, .(banda, periodo, observados, oe = ci(razon_obs_esp, razon_lo, razon_hi),
  rec = fifelse(periodo == "Exceso 2025", sprintf("%s (%s to %s)", pct(recuperacion_mediana), pct(rec_lo), pct(rec_hi)), "—"))]
T2 <- merge(mk2("principal"), mk2("tendencia_2014_2019")[, .(banda, periodo, oe_t = oe, rec_t = rec)], by = c("banda", "periodo"))
T2 <- T2[, .(`Age group` = factor(bl[banda], levels = bl), Period = fifelse(periodo == "Exceso 2025", "2025", "2020–2024"),
             Observed = fmtn(observados), `O/E, stable baseline` = oe, `Deficit recovered, stable baseline` = rec,
             `O/E, trend baseline` = oe_t, `Deficit recovered, trend baseline` = rec_t)]
setorder(T2, `Age group`, Period); T2[, `Age group` := as.character(`Age group`)]
T2[Period == "2025", `Age group` := ""]

irr <- R("edad_irr_nb.csv"); ror <- R("edad_ror_nb_efectos_fijos_anio.csv")
ag <- c(`<6 wk` = "<6 weeks", `6–8 wk` = "6–8 weeks", `<2 mo` = "<2 months (combined)", `2–3 mo` = "2–3 months", `4–5 mo` = "4–5 months",
        `6–11 mo` = "6–11 months", `1–4 y` = "1–4 years (reference)", `5–9 y` = "5–9 years", `10–14 y` = "10–14 years",
        `15–19 y` = "15–19 years", `20–39 y` = "20–39 years", `40–59 y` = "40–59 years", `60+ y` = "≥60 years")
gi <- function(g, e) { x <- irr[grupo == g & epidemia == e]; if (nrow(x)) ci(x$irr, x$lo, x$hi) else "—" }
gr <- function(g, c) { x <- ror[grupo == g & contraste == c]; if (nrow(x)) ci(x$est, x$lo, x$hi) else if (g == "1–4 y") "1 (ref.)" else "—" }
T3 <- rbindlist(lapply(names(ag), function(g) data.table(`Age group` = ag[[g]], `IRR 2011–2013 vs baseline` = gi(g, "E1"),
  `IRR 2025 vs baseline` = gi(g, "E2"), `RoR 2011–2013` = gr(g, "RoR 2011-2013"), `RoR 2025` = gr(g, "RoR 2025"),
  `Ratio of ratios, 2025 vs 2011–2013` = gr(g, "RoRR 2025/2011-2013"))))
INF5 <- c(`<6 wk` = "<6 weeks", `6–8 wk` = "6–8 weeks", `2–3 mo` = "2–3 months", `4–5 mo` = "4–5 months", `6–11 mo` = "6–11 months")
pf <- function(p) ifelse(p >= 0.995, "1.00", ifelse(p < 0.1, sprintf("%.2g", p), sprintf("%.2f", p)))
holm <- ror[grupo %in% names(INF5) & contraste == "RoRR 2025/2011-2013"][match(names(INF5), grupo)]
p2m <- ror[grupo == "<2 mo" & contraste == "RoRR 2025/2011-2013", p]
tend <- R("edad_rorr_tendencia_lactantes.csv")

# Tabla 4: muertes por categoría, OR crudos, OR ajustados y modelo con control prenatal
ob <- R("letalidad_modelos_OR.csv"); crd <- R("letalidad_OR_crudos.csv"); cn <- R("letalidad_muertes_por_categoria.csv")
M_P <- "Principal (sin tipo de confirmación)"; M_A <- "Submuestra + control prenatal del periodo (municipal, DE)"
filas4 <- list(c("edad_g6-11 meses", "Age 6–11 months (reference)"), c("edad_g4-5 meses", "Age 4–5 months"), c("edad_g2-3 meses", "Age 2–3 months"),
  c("edad_g6-8 semanas", "Age 6–8 weeks"), c("edad_g<6 semanas", "Age <6 weeks"),
  c("periodo42011-2013", "Period 2011–2013 (reference)"), c("periodo42014-2019", "Period 2014–2019"), c("periodo42020-2024", "Period 2020–2024"),
  c("periodo42025", "Period 2025"), c("etnia3Otro/ninguno", "No ethnic self-identification (reference)"), c("etnia3Afrodescendiente", "Afro-descendant"),
  c("etnia3Indígena", "Indigenous"), c("regimen4Contributivo", "Contributory insurance (reference)"), c("regimen4Subsidiado", "Subsidized insurance"),
  c("regimen4No asegurado", "Uninsured"), c("regimen4Especial", "Special or exception scheme"), c("rural", "Rural residence"), c("mujer", "Female"),
  c("migrante", "Migrant"), c("z_cpn4_per", "Antenatal-care coverage in the period of the case, per SD"))
v_or <- function(m, k) { x <- ob[modelo == m & termino == k]; if (nrow(x)) ci(x$or, x$lo, x$hi) else NA_character_ }
T4 <- rbindlist(lapply(filas4, function(f) { k <- f[1]; es_ref <- grepl("reference", f[2]); x <- cn[termino == k]
  data.table(Variable = f[2],
             `Deaths/infants (%)` = if (nrow(x)) sprintf("%d/%s (%.1f)", x$muertes, format(x$lactantes, big.mark = ","), 100 * x$muertes / x$lactantes) else "—",
             `Crude OR (95% CI)` = if (es_ref) "1" else { y <- crd[termino == k]; if (nrow(y)) ci(y$or_crudo, y$lo, y$hi) else "—" },
             `Adjusted OR (95% CI)` = if (es_ref) "1" else { v <- v_or(M_P, k); if (is.na(v)) "—" else v },
             `Adjusted OR with antenatal care (95% CI)` = if (es_ref) "1" else { v <- v_or(M_A, k); if (is.na(v)) "—" else v }) }))
nB <- ob[modelo == M_P, n][1]; dB <- ob[modelo == M_P, muertes][1]
nA <- ob[modelo == M_A, n][1]; dA <- ob[modelo == M_A, muertes][1]
fl <- R("letalidad_flujo_exclusiones.csv")
# DE del control prenatal del periodo entre los lactantes del modelo (para leer el OR «por DE»)
covm <- readRDS("01_datos/procesados/covariables_municipales_terridata.rds")
inf_sd <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")[lactante == TRUE & anio >= 2011 & !is.na(grupo_edad) & !is.na(dp_res) & regimen != "Indeterminado"]
inf_sd <- merge(inf_sd[, .(MPIO = mpio_res, anio)], covm[, .(MPIO, cpn4_1113, cpn4_1419, cpn4_2024)], by = "MPIO")
inf_sd[, cpn := fcase(anio <= 2013, cpn4_1113, anio <= 2019, cpn4_1419, anio >= 2020, cpn4_2024)]
sd_cpn_inf <- sd(inf_sd$cpn, na.rm = TRUE)

# --------------------------------------------------------------------------------
# Documento principal
# --------------------------------------------------------------------------------
doc <- read_docx(PLANTILLA_MS)               # plantilla de Frontiers: su sección (página, líneas, pies) no se cambia
doc <- body_add_par(doc, TITLE, style = "Title")
au <- unlist(lapply(seq_along(AUTH), function(i) { a <- AUTH[[i]]
  list(ftext(a[[1]], prop = NULL), ftext(paste0(a[[2]], if (a[[3]]) "*" else ""), fp_text_lite(vertical.align = "superscript")),
       ftext(if (i < length(AUTH)) ", " else "", prop = NULL)) }), recursive = FALSE)
doc <- body_add_fpar(doc, do.call(fpar, au), style = "Author List")
for (k in names(AFF)) doc <- body_add_fpar(doc, fpar(ftext(k, fp_text_lite(vertical.align = "superscript")), ftext(AFF[[k]], prop = NULL)), style = "Normal")
corr <- list(ftext("* Correspondence: ", fp_text_lite(bold = TRUE)))   # como en la plantilla: rótulo en negrita y un renglón por dato
for (cc in CORRESP) corr <- c(corr, list(run_linebreak(), ftext(cc[1], prop = NULL), run_linebreak(), ftext(cc[2], prop = NULL)))
doc <- body_add_fpar(doc, do.call(fpar, corr), style = "Normal")
doc <- body_add_par(doc, paste("Keywords:", KEYW), style = "Author List")
doc <- etiqueta(doc, "Word count:", sprintf("abstract %d words; main text %s words; %d tables; %d figures; %d references.", abs_words, format(main_words, big.mark = ","), 4, 6, length(num)), "Normal")

body_lines <- src[sec_start("^# Abstract"):length(src)]
for (ln in body_lines) {
  if (!nzchar(trimws(ln))) next
  # Los títulos de la plantilla se numeran solos: el número de la fuente («2.4.1») se quita
  sin_num <- function(h) sub("^[0-9]+(\\.[0-9]+)*\\s+", "", h)
  if (grepl("^### ", ln)) { doc <- H(doc, sin_num(sub("^### ", "", ln)), 3); next }
  if (grepl("^## ", ln))  { doc <- H(doc, sin_num(sub("^## ", "", ln)), 2); next }
  if (grepl("^# ", ln))   { h <- sub("^# ", "", ln)
    if (h == "Abstract") { doc <- body_add_par(doc, "Abstract", style = "Author List"); next }   # como en la plantilla
    doc <- H(doc, sin_num(h), 1); next }
  doc <- P(doc, ln)
}
doc <- H(doc, "References", 1)
for (k in names(num)) doc <- body_add_par(doc, sprintf("%d. %s", num[[k]], refs[[k]]), style = "Normal")

leg <- c(
  "Figura1_dinamica_deficit" = "Pertussis dynamics in Colombia, 2007–2025. (A) Weekly confirmed cases by week of symptom onset. The shaded band marks 2013, the year maternal Tdap was introduced (month not documented); vertical lines mark the COVID-19 lockdown (March 2020) and Circular 0011 of 28 March 2025, which intensified vaccination and surveillance. (B) Observed and expected monthly cases by age group, 2018–2025; expected values (median and 95% prediction interval) from negative binomial models with seasonal terms fitted to 2015–2019. The shaded months (January 2020 to December 2024) are those used to compute the deficit. O/E, observed-to-expected ratio.",
  "Figura2_estructura_edad" = "Age structure of confirmed cases. (A) Age distribution of cases by year; numbers above bars are cases with known age (48 cases without recorded age are excluded). (B) Ratios of incidence rate ratios relative to children aged 1–4 years, for each epidemic versus the 2015–2019 baseline (negative binomial model with year fixed effects). (C) Ratio of ratios comparing the 2025 and 2011–2013 epidemics among infant age groups. Bars are 95% confidence intervals.",
  "Figura3_estudio_anio_por_anio" = "Incidence among infants younger than 2 months relative to two reference groups, by year, compared with the 2015–2019 baseline (shaded). Ratios of incidence rate ratios from a negative binomial event-study model with year fixed effects and a separate age-by-year interaction for each year outside the baseline; the dispersion parameter was estimated from the baseline years. (A) Reference group children aged 1–4 years. (B) Reference group infants aged 6–11 months. The dashed line marks 2013, when maternal Tdap was introduced. Years 2020–2024 (gray) had few cases. Bars are 95% confidence intervals.",
  "Figura4_mapas_riesgo_municipal" = "Municipal risk of confirmed pertussis. (A, B) Age-standardized relative risk (posterior median) from Bayesian BYM2 models in the 2011–2013 and 2025 epidemics; black outlines in (B) mark clusters detected by Kulldorff's spatial scan statistic (p < 0.05). (C) Posterior probability that the relative risk in 2025 exceeded 1; outlined units have a probability above 0.95. (D) Spatial component of the change in 2025 relative to each unit's own 2015–2019 baseline, after adjustment for municipal covariates, from a joint BYM2 model. The San Andrés archipelago is not shown.",
  "Figura5_control_prenatal_por_edad" = "Municipal antenatal-care coverage and confirmed incidence by age group in the 2011–2013 and 2025 epidemics. Relative risks per standard deviation of the percentage of live births with ≥4 antenatal visits (2008–2010 coverage for 2011–2013 and 2019–2024 coverage for 2025), with 95% credible intervals, from joint Bayesian models of counts by spatial unit and age group with age-group-specific slopes, adjusted for multidimensional poverty, rural, Indigenous and Afro-descendant shares, population density and pentavalent coverage. If coverage marked access to maternal Tdap, the association would be strongest in infants younger than 2 months in 2025 and absent in 2011–2013.",
  "Figura6_muerte_infantil_desigualdad" = "Infant death and inequalities. (A) Adjusted odds ratios for death among infants with confirmed pertussis, 2011–2025 (mixed-effects logistic regression with department random intercept). (B) Case fatality among Indigenous infants and infants without ethnic self-identification, by period (exact 95% confidence intervals). (C) Infant pertussis mortality per 100,000 infant-years by population-weighted quintile of municipal multidimensional poverty, with exact 95% confidence intervals; RII, relative index of inequality.")
doc <- H(doc, "Figure legends", 1)
for (i in seq_along(leg)) doc <- etiqueta(doc, sprintf("Figure %d.", i), leg[[i]], "Normal")

tablas <- list(
  list(T1, "Characteristics of confirmed pertussis cases by period, Colombia, 2007–2025.",
       "ᵃ Hospitalization was not recorded in 2008–2009. IQR, interquartile range. Incidence uses DANE population projections; infant subgroups use the population aged 0 years multiplied by the fraction of the year spanned."),
  list(T2, "Observed and expected confirmed pertussis cases by age group during the pandemic period (2020–2024) and the 2025 epidemic, under two baselines.",
       "Expected cases from negative binomial models with seasonal (Fourier) terms and population offset. Stable baseline, fitted to 2015–2019 without trend; trend baseline, fitted to 2014–2019 with a linear trend. Values are estimates with 95% prediction intervals from 5,000 simulations combining parameter uncertainty and negative binomial sampling. Deficit recovered = excess in 2025 / deficit in 2020–2024. Years refer to the date of symptom onset (1,189 cases in 2025). Expected counts are given in Supplementary Table S2. O/E, observed-to-expected ratio."),
  list(T3, "Age-specific incidence rate ratios and ratios of incidence rate ratios in the 2011–2013 and 2025 epidemics relative to the 2015–2019 baseline.",
       sprintf("IRR, incidence rate ratio from Model 1 (negative binomial, one parameter per age group and period). RoR, ratio of IRRs relative to children aged 1–4 years versus baseline, and ratio of ratios (2025 vs 2011–2013), both from Model 2 (negative binomial with year fixed effects). The <2 months row, the contrast outlined in the initial proposal, was estimated in a separate Model 2 fit with combined infant groups (p = %s). Holm-adjusted p values for the ratio of ratios across the five finer infant groups: %s. The ratio of ratios increased by a factor of %.2f (95%% CI %.2f–%.2f; p = %s) per month of age within the first year. Values are estimates (95%% CI).",
               pf(p2m), paste(sprintf("%s, %s", INF5, pf(holm$p_holm_lactantes)), collapse = "; "), tend$razon_por_mes, tend$lo, tend$hi, pf(tend$p))),
  list(T4, sprintf("Factors associated with death among infants with confirmed pertussis, Colombia, 2011–2025 (n = %s; %d deaths).", format(nB, big.mark = ","), dB),
       sprintf("Crude odds ratios (OR) from univariable logistic regression. Adjusted OR from mixed-effects logistic regression with a department random intercept, adjusted for all variables shown except antenatal-care coverage. The last column adds the standardized municipal percentage of live births with ≥4 antenatal visits in the period of each case (n = %s; %d deaths); one SD corresponds to %.1f percentage points. Excluded: %d infants without department of residence and %d with insurance recorded as indeterminate. Sensitivity analyses are in Supplementary Table S11.",
               format(nA, big.mark = ","), dA, sd_cpn_inf, fl[paso == "Sin departamento de residencia", n], fl[paso == "Régimen de salud indeterminado", n])))
doc <- H(doc, "Tables", 1)
for (i in seq_along(tablas)) doc <- TABLA(doc, tablas[[i]][[1]], sprintf("Table %d.", i), tablas[[i]][[2]], tablas[[i]][[3]], partir_ic = i %in% 2:4)

doc <- cursor_begin(doc) |> body_remove()   # el párrafo vacío con que empieza la copia de la plantilla
doc <- set_doc_properties(doc, title = TITLE, creator = paste(vapply(AUTH, `[[`, "", 1), collapse = "; "))
tmp_main <- file.path(build, paste0("latex_", out_main))
print(doc, target = tmp_main)
ecuaciones_nativas(tmp_main, out_main)
ajustar_docx(out_main, numerar_lineas = TRUE, plantilla = TRUE)

# --------------------------------------------------------------------------------
# Material suplementario: tablas en el orden en que el texto las cita
# --------------------------------------------------------------------------------
geo <- st_drop_geometry(st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE))
nm <- setNames(gsub(" (De|Del|La|Las|Los|El|Y) ", " \\L\\1 ", tools::toTitleCase(tolower(geo$MPIO_CNMBR)), perl = TRUE), geo$MPIO_CDPMP)

# S1: campos y códigos de los microdatos (RECORD 7.1)
S1 <- data.table(
  Variable = c("Event", "Final classification", "Death", "Hospitalization", "Ethnicity", "Insurance scheme", "Area of residence", "Migrant status",
               "Municipality of residence", "Age at consultation", "Date of symptom onset", "Pentavalent coverage", "Antenatal-care coverage"),
  `Source field` = c("Annual files of event 800 (Datos_YYYY_800)", "Estado_final_de_caso", "CON_FIN", "PAC_HOS", "PER_ETN", "TIP_SS", "AREA", "GP_MIGRANT",
                     "COD_DPTO_R and COD_MUN_R", "FECHA_NTO, FEC_CON, EDAD, UNI_MED", "INI_SIN, FEC_CON",
                     "TerriData: «Cobertura vacunación pentavalente en menores de 1 año»", "TerriData: «Porcentaje de nacidos vivos con 4 o más consultas de control prenatal»"),
  `Coding used` = c("All records, 2007–2025 (17,021); none excluded", "3 laboratory (culture or PCR); 4 clinical; 5 epidemiological link",
                    "2 dead; 1 alive; 0 in 3 records, analyzed as alive", "1 hospitalized; not recorded in 2008–2009",
                    "1 Indigenous; 3, 4 and 5 Afro-descendant (Raizal, Palenquero, Black or Afro-Colombian); 2 Rrom, grouped with no ethnic self-identification in the infant death models; 6 none",
                    "C contributory; S subsidized; N uninsured; P and E special or exception schemes; other values indeterminate, treated as missing in the infant death models",
                    "1 municipal head (urban); 2 populated center and 3 dispersed rural (rural)", "1 migrant; 2 not migrant",
                    "Five-digit DIVIPOLA code; codes ending in 000 identify only the department",
                    "From the date of birth when consistent with the reported age, otherwise from the reported age and unit",
                    "Replaced by the consultation date in time-series analyses when later than consultation or more than 120 days before it",
                    "Municipal mean, 2008–2010 or 2015–2019", "Municipal mean, 2008–2010, 2019–2024 or the period of each infant case"))

# S2: observados y esperados con las dos líneas base
S2 <- de_all[, .(Baseline = fifelse(especificacion == "principal", "Stable, 2015–2019", "Linear trend, 2014–2019"), `Age group` = bl[banda],
                 Period = fifelse(periodo == "Exceso 2025", "2025", "2020–2024"), Observed = fmtn(observados),
                 `Expected (95% PI)` = sprintf("%s (%s–%s)", fmtn(esperados), fmtn(esp_lo), fmtn(esp_hi)), `O/E (95% PI)` = ci(razon_obs_esp, razon_lo, razon_hi))]
S2[, ord := match(`Age group`, bl)]; setorder(S2, -Baseline, ord, Period); S2[, ord := NULL]

# S3: sensibilidad de la RoRR
se <- R("edad_sensibilidad.csv")
an_en <- c("Principal (confirmados; base 2015-2019; NB con efectos fijos de año)" = "Main analysis", "Solo confirmados por laboratorio" = "Laboratory-confirmed only",
           "Sin duplicados probables" = "Probable duplicates removed", "Sin posibles renotificaciones (marcador amplio)" = "Possible renotifications removed (broad flag)",
           "Año de inicio de síntomas" = "Year of symptom onset", "Línea base 2014-2019" = "Baseline 2014–2019", "Línea base 2017-2019" = "Baseline 2017–2019",
           "Sin Antioquia" = "Excluding Antioquia", "Cuasi-Poisson" = "Quasi-Poisson", "Referencia 6-11 meses" = "Reference group 6–11 months")
S3 <- se[, .(Analysis = an_en[analisis], `RoR <6 wk, 2011–13` = `RoR <6 sem 2011-13`, `RoR <6 wk, 2025` = `RoR <6 sem 2025`, `RoRR <6 wk` = `RoRR <6 sem`,
             `RoRR <2 mo` = `RoRR <2 m`, `RoRR 2–3 mo` = `RoRR 2-3 m`, `RoR 10–14 y, 2025` = `RoR 10-14 a 2025`)]
for (j in names(S3)[-1]) set(S3, j = j, value = gsub("(\\d)-(\\d)", "\\1–\\2", S3[[j]]))

# S4: estudio año por año
es <- R("edad_estudio_eventos.csv"); ec <- R("edad_estudio_eventos_conteos.csv")
ev <- function(a, rf, an) { x <- es[anio == a & referencia == rf & analisis == an]; if (nrow(x)) ci(x$est, x$lo, x$hi) else "Baseline" }
S4 <- ec[, .(Year = anio, `Cases <2 mo` = fmtn(`<2 mo`), `Cases 6–11 mo` = fmtn(`6–11 mo`), `Cases 1–4 y` = fmtn(`1–4 y`))]
S4[, `:=`(`vs 1–4 y` = vapply(Year, ev, "", rf = "1–4 y", an = "Todos los confirmados"), `vs 6–11 mo` = vapply(Year, ev, "", rf = "6–11 mo", an = "Todos los confirmados"),
          `vs 1–4 y, laboratory only` = vapply(Year, ev, "", rf = "1–4 y", an = "Solo laboratorio"), `vs 6–11 mo, laboratory only` = vapply(Year, ev, "", rf = "6–11 mo", an = "Solo laboratorio"),
          `vs 1–4 y, quasi-Poisson` = vapply(Year, ev, "", rf = "1–4 y", an = "Cuasi-Poisson"))]

# S5: RoRR de 2025 frente a otros periodos previos a la Tdap
pc <- R("edad_rorr_periodos_comparacion.csv")[grepl("^RoRR", contraste)]
S5 <- dcast(pc[, .(`Pre-Tdap comparison period` = gsub("-", "–", periodo_comparacion), Reference = fifelse(referencia == "1–4 y", "Children 1–4 years", "Infants 6–11 months"),
                   g = grupo, v = ci(est, lo, hi))], `Pre-Tdap comparison period` + Reference ~ g, value.var = "v")
S5 <- S5[, .(`Pre-Tdap comparison period`, Reference, `Infants <6 weeks` = `<6 wk`, `Infants <2 months` = `<2 mo`,
             `Infants 6–11 months` = fifelse(Reference == "Children 1–4 years", `6–11 mo`, "Reference"))]
S5[, ord := match(`Pre-Tdap comparison period`, c("2011–2013", "2011–2012", "2007–2012", "2007–2013", "2007–2010"))]; setorder(S5, ord, Reference); S5[, ord := NULL]

# S6: cohortes pandémicas
co <- R("cohortes_pandemicas_sensibilidad.csv")
S6 <- co[, .(Baseline = gsub("-", "–", base), Cohorts = gsub("-", "–", cohortes), `Spline df` = df, `IRR (95% CI)` = ci(irr, lo, hi), p = sprintf("%.3f", p))][order(Baseline, Cohorts, `Spline df`)]

# S7: contrastes identificables del modelo APC
ac <- R("apc_contraste_pandemia.csv"); appc <- R("apc_chequeo_predictivo.csv")
S7 <- ac[, .(Prior = sub("PC\\((.*), 0.01\\)", "P(σ > \\1) = 0.01", previa),
             Contrast = fifelse(grepl("tendencia", contraste), "2019–2022 cohorts vs linear trend of 2014–2018 cohorts", "Change in slope, [f(2022) − f(2019)] − [f(2018) − f(2015)]"),
             `Ratio (95% CrI)` = ci(est, lo, hi), `P(ratio > 1)` = sprintf("%.3f", p_pos))]

# S8: conglomerados de Kulldorff
k1 <- R("espacial_kulldorff_2025.csv"); k2 <- R("espacial_kulldorff_2025_ventana20.csv"); kc <- R("espacial_kulldorff_2025_composicion.csv")
er <- readRDS("01_datos/procesados/espacial_resultados.rds")
S8 <- rbind(k1[, .(Window = "10%", Cluster = cluster, Units = n_municipios, Observed = observados, Expected = sprintf("%.1f", esperados),
                   SMR = sprintf("%.2f", smr), p = sprintf("%.3f", p), `Indigenous share, %` = sprintf("%.1f", 100 * kc$p_indigena[match(cluster, kc$cluster)]),
                   `Units (first 12)` = vapply(er$kd_ids[cluster], function(v) paste(head(nm[v], 12), collapse = ", "), ""))],
            k2[, .(Window = "20%", Cluster = cluster, Units = n_municipios, Observed = observados, Expected = sprintf("%.1f", esperados),
                   SMR = sprintf("%.2f", smr), p = sprintf("%.3f", p), `Indigenous share, %` = "—",
                   `Units (first 12)` = vapply(strsplit(municipios, " "), function(v) paste(head(nm[v], 12), collapse = ", "), ""))])

# S9: covariables de los modelos BYM2 separados
fe <- R("espacial_bym2_efectos_fijos.csv"); fc <- R("espacial_cambio_efectos_fijos.csv"); cr_ <- R("espacial_covariables_resumen.csv")
cv <- c(z_ipm = "MPI", z_p_rural = "Rural share", z_p_ind = "Indigenous share", z_p_afro = "Afro-descendant share", z_log_dens = "Log population density",
        z_penta_1519 = "Pentavalent coverage", z_penta_0810 = "Pentavalent coverage", z_cpn4_1924 = "Antenatal care ≥4 visits", z_cpn4_0810 = "Antenatal care ≥4 visits")
mo <- c("2011-2013 <1 año" = "2011–13, <1 y", "2011-2013 todas las edades" = "2011–13, all ages", "2025 <1 año" = "2025, <1 y",
        "2025 todas las edades" = "2025, all ages", "2025 10+ años (control negativo)" = "2025, ≥10 y")
S9 <- dcast(fe[termino != "(Intercept)", .(Covariate = cv[termino], m = mo[modelo], v = ci(rr, lo, hi))], Covariate ~ m, value.var = "v")
chg <- fc[grepl("_x25$", termino), .(Covariate = cv[sub("_x25$", "", termino)], `Change 2025 vs own baseline` = ci(rr, lo, hi))]
S9 <- merge(S9, chg, by = "Covariate", all.x = TRUE)
setcolorder(S9, c("Covariate", mo[c(1, 2, 3, 5, 4)], "Change 2025 vs own baseline"))
for (j in names(S9)[-1]) set(S9, which(is.na(S9[[j]])), j, "—")
hy <- R("espacial_bym2_hiperparametros.csv")[grepl("Phi", param)]
sdv <- setNames(cr_$de, cr_$variable); nfal <- setNames(cr_$unidades_con_dato, cr_$variable)

# S10: modelo conjunto del control prenatal y sensibilidades
cj <- R("espacial_cpn_modelo_conjunto.csv"); cs2 <- R("espacial_cpn_conjunto_sensibilidad.csv")
filas10 <- c("CPN, <2 m" = "Slope, <2 months", "CPN, 2-5 m" = "Slope, 2–5 months", "CPN, 6-11 m" = "Slope, 6–11 months", "CPN, 1-9 a" = "Slope, 1–9 years",
             "CPN, 10+ a" = "Slope, ≥10 years", "Razón de pendientes, <2 m vs 6-11 m" = "Ratio, <2 months vs 6–11 months",
             "Razón de pendientes, <2 m vs 10+ a" = "Ratio, <2 months vs ≥10 years", "Razón de pendientes, 6-11 m vs 10+ a" = "Ratio, 6–11 months vs ≥10 years",
             "Razón de pendientes, 2-5 m vs 6-11 m" = "Ratio, 2–5 months vs 6–11 months", "Razón de pendientes, <2 m vs 1-9 a" = "Ratio, <2 months vs 1–9 years")
col10 <- list(`2025, joint model` = cj[modelo == "Conjunto 2025"], `2011–2013, joint model` = cj[modelo == "Conjunto 2011-2013"],
              `2025 vs 2011–2013 (difference in ratios)` = cj[grepl("diferencia", modelo)],
              `2025, covariates by age group` = cj[modelo == "Conjunto 2025, covariables por grupo"], `2025, excluding Antioquia` = cj[modelo == "Conjunto 2025, sin Antioquia"],
              `2025, NB with department fixed effects` = cs2[grepl("Binomial", analisis) & modelo == "Conjunto 2025"],
              `2011–2013, NB with department fixed effects` = cs2[grepl("Binomial", analisis) & modelo == "Conjunto 2011-2013"])
S10 <- data.table(Estimate = unname(filas10))
for (k in names(col10)) { x <- col10[[k]]; S10[, (k) := vapply(names(filas10), function(t) { y <- x[termino == t]; if (nrow(y)) ci(y$rr, y$lo, y$hi) else "—" }, "")] }
pri <- cs2[grepl("Previa", analisis) & grepl("^Razón de pendientes, <2 m vs 6-11 m", termino)]

# S11: sensibilidad del modelo de muerte
mods11 <- c("Principal (sin tipo de confirmación)" = "Main model", "Con tipo de confirmación (plan inicial)" = "With type of confirmation (initial plan)",
            "Solo confirmados por laboratorio" = "Laboratory-confirmed only", "Efectos aleatorios de departamento y municipio" = "Department and municipality random intercepts",
            "Firth (sin efecto aleatorio)" = "Firth regression", "Con interacción periodo x indígena" = "With period × Indigenous interaction")
lt11 <- c("edad_g<6 semanas" = "Age <6 weeks", "edad_g6-8 semanas" = "Age 6–8 weeks", "edad_g2-3 meses" = "Age 2–3 months", "edad_g4-5 meses" = "Age 4–5 months",
          "periodo42014-2019" = "Period 2014–2019", "periodo42020-2024" = "Period 2020–2024", "periodo42025" = "Period 2025", "etnia3Indígena" = "Indigenous",
          "etnia3Afrodescendiente" = "Afro-descendant", "regimen4Subsidiado" = "Subsidized insurance", "regimen4No asegurado" = "Uninsured",
          "regimen4Especial" = "Special or exception scheme", "rural" = "Rural residence", "mujer" = "Female", "migrante" = "Migrant",
          "lab" = "Laboratory-confirmed", "ind_1419" = "Indigenous × 2014–2019", "ind_2024" = "Indigenous × 2020–2024", "ind_25" = "Indigenous × 2025")
S11 <- data.table(Variable = unname(lt11))
for (m in names(mods11)) S11[, (mods11[[m]]) := vapply(names(lt11), function(t) { y <- ob[modelo == m & termino == t]; if (nrow(y)) ci(y$or, y$lo, y$hi) else "—" }, "")]
n11 <- vapply(names(mods11), function(m) sprintf("%s (n = %s; %d deaths)", mods11[[m]], format(ob[modelo == m, n][1], big.mark = ","), ob[modelo == m, muertes][1]), "")
lrt <- R("letalidad_interaccion_lrt.csv"); dgP <- R("letalidad_diagnostico.csv"); ti <- R("letalidad_tendencia_indigena.csv")

# S12-S14: letalidad, mortalidad por etnia e inequidades
lc <- R("letalidad_cruda_etnia_periodo.csv")
eth <- c("Otro/ninguno" = "No ethnic self-identification", "Afrodescendiente" = "Afro-descendant", "Indígena" = "Indigenous")
S12 <- lc[, .(Period = gsub("-", "–", periodo4), Ethnicity = eth[etnia3], Infants = lactantes, Deaths = muertes, `Case fatality (95% CI)` = gsub("(\\d)-(\\d)", "\\1–\\2", letalidad))]
me <- R("mortalidad_lactantes_etnia_aproximada.csv")
S13 <- me[, .(Period = gsub("-", "–", periodo), `Indigenous deaths` = m_ind, `Indigenous rate /100,000` = sprintf("%.1f", tasa_ind),
              `Non-Indigenous deaths` = m_noind, `Non-Indigenous rate /100,000` = sprintf("%.1f", tasa_noind), `Rate ratio (95% CI)` = ci(rr, rr_lo, rr_hi, 1))]
iq <- R("inequidades_ipm_quintiles.csv"); qc5 <- R("inequidades_ipm_cortes.csv")[!is.na(quintil)]
S14 <- iq[, .(Period = gsub("-", "–", periodo), Quintile = quintil, Cases = O, `SIR (95% CI)` = ci(sir, sir_lo, sir_hi),
              `Infant deaths` = muertes, `Infant mortality /100,000 (95% CI)` = ci(mort_inf_100k, mort_lo, mort_hi),
              `Infant case fatality, % (95% CI)` = ci(100 * letalidad, 100 * let_lo, 100 * let_hi, 1),
              `RII incidence` = gsub("(\\d)-(\\d)", "\\1–\\2", RII_incidencia), `RII infant mortality` = gsub("(\\d)-(\\d)", "\\1–\\2", RII_mortalidad_infantil),
              `SII infant mortality` = gsub("-", "−", gsub("(\\d)–(\\d)", "\\1 to \\2", gsub("(\\d)–-", "\\1 to -", SII_mortalidad_infantil))))]
for (j in c("RII incidence", "RII infant mortality", "SII infant mortality")) S14[Quintile != 1, (j) := ""]

# S15-S17: calidad de los datos y diagnósticos
qc <- R("calidad_confirmacion_por_anio.csv"); fal <- R("calidad_faltantes.csv")
S15 <- qc[, .(Year = anio, Laboratory = Laboratorio, `Epidemiological link` = `Nexo epidemiológico`, Clinical = Clínica, Total = total, `Laboratory, %` = sprintf("%.1f", pct_laboratorio))]
lab <- R("calidad_laboratorio_edad_periodo.csv")
S16 <- dcast(lab[, .(g = factor(g, levels = c("<2 mo", "2–5 mo", "6–11 mo", "1–4 y", "5–9 y", "≥10 y")), per = gsub("-", "–", per),
                     v = sprintf("%.1f (%s/%s)", pct_laboratorio, format(laboratorio, big.mark = ","), format(casos, big.mark = ",")))], g ~ per, value.var = "v")
setnames(S16, "g", "Age group"); S16[, `Age group` := c(`<2 mo` = "<2 months", `2–5 mo` = "2–5 months", `6–11 mo` = "6–11 months", `1–4 y` = "1–4 years", `5–9 y` = "5–9 years", `≥10 y` = "≥10 years")[as.character(`Age group`)]]
dA_ <- R("edad_diagnosticos.csv"); dB_ <- R("espacial_bym2_diagnostico.csv"); pB <- R("espacial_bym2_chequeo_predictivo.csv")
mdl <- c("Modelo 1 (grupos finos)" = "Model 1, age groups", "Modelo 2 (grupos finos)" = "Model 2, finer infant groups", "Modelo 2 (<2 meses combinado)" = "Model 2, infants <2 months combined",
         "Estudio de eventos: años de la línea base" = "Event-study model, baseline years")
S17 <- rbind(
  dA_[, .(Model = mdl[modelo], Check = "Negative binomial θ; Pearson χ²/df", Result = sprintf("%.1f; %.2f", theta, pearson_gl))],
  dA_[, .(Model = mdl[modelo], Check = "Simulated residuals: p for uniformity, dispersion, zeros", Result = sprintf("%.2f; %.2f; %.2f", p_uniformidad_KS, p_dispersion, p_ceros))],
  pB[, .(Model = paste("BYM2,", mo[modelo]), Check = c(ceros = "Units without cases, observed vs predicted (95% interval); p", maximo = "Maximum count, observed vs predicted (95% interval); p",
                                                      pearson = "Pearson discrepancy, observed vs replicated (median); p")[estadistico],
         Result = sprintf("%s vs %s (%s–%s); %.2f", fmtn(observado), fmtn(replicado_mediana), fmtn(replicado_p2.5), fmtn(replicado_p97.5), p_predictivo))],
  appc[, .(Model = "Age–period–cohort", Check = c(ceros = "Cells without cases, observed vs predicted; p", maximo = "Maximum count, observed vs predicted; p",
                                                  pearson = "Pearson discrepancy, observed vs replicated (median); p")[estadistico],
           Result = sprintf("%s vs %s; %.2f", fmtn(observado), fmtn(replicado_mediana), p_predictivo))],
  dgP[, .(Model = "Infant death, main model", Check = c("Department intraclass correlation; area under the ROC curve", "Simulated residuals: p for uniformity, dispersion"),
          Result = c(sprintf("%.3f; %.2f", icc_departamento, auc), sprintf("%.2f; %.2f", p_uniformidad_KS, p_dispersion)))])

sd <- read_docx(PLANTILLA_SUP)                # plantilla de Frontiers para el suplemento
sd <- body_add_par(sd, "Supplementary Material", style = "Supplementary Material")
sd <- H(sd, "Supplementary Methods", 1)
pr <- R("modelo_C_bym2_efectos_fijos.csv")[termino == "z_penta3"]
sm <- c(
  "**Data preparation.** Annual microdata files for event 800 (2007–2025) were merged and dates were parsed from the source format; no record was removed. Age at consultation was computed as the difference between the consultation date and the date of birth when both were available and consistent with the reported age (difference ≤31 days or ≤10% of the reported age, or same completed years), and from the reported age and unit otherwise. Symptom onset dates later than consultation or more than 120 days before consultation were replaced by the consultation date in time-series analyses. Three records with a final condition coded 0 were analyzed as survivors. Probable duplicates were defined as records sharing date of birth, sex and municipality of residence plus the same symptom-onset or consultation date; one record per group was retained in the sensitivity analysis (133 records excluded). A broader flag marked records with the same date of birth, sex and municipality and symptom onset within 30 days of a previous record (721 records).",
  "**Area-level data.** No person-level or institutional linkage was performed. Population denominators and municipal covariates were joined to cases by the five-digit DIVIPOLA code of the municipality of residence, and 137 cases (0.8%) could not be assigned to a municipality (88 without a valid code and 49 with a code identifying only the department). The non-municipalized area of Mapiripana (code 94663), which became part of the municipality of Barrancominas (94343) in 2019, was joined to Barrancominas, and Nuevo Belén de Bajirá (27493), which appears in the population projections from 2024 with population separated from Riosucio (27615), was joined to Riosucio. The corresponding polygons of the 2018 National Geostatistical Framework were dissolved, which left 1,121 spatial units. Covariates of joined units were averaged.",
  sprintf("**Municipal covariates.** Between-unit standard deviations were %.1f percentage points for the MPI, %.1f for the rural share, %.1f for the Indigenous share, %.1f for the Afro-descendant share and %.2f for log population density; %.1f and %.1f percentage points for pentavalent coverage in 2015–2019 and 2008–2010; and %.1f and %.1f for antenatal-care coverage in 2019–2024 and 2008–2010. Coverage data were missing for %d to %d of 1,121 units, which were assigned the mean (standardized value 0).",
          sdv[["ipm"]], 100 * sdv[["p_rural"]], 100 * sdv[["p_ind"]], 100 * sdv[["p_afro"]], sdv[["log_dens"]], sdv[["penta_1519"]], sdv[["penta_0810"]], sdv[["cpn4_1924"]], sdv[["cpn4_0810"]],
          1121L - max(nfal[c("penta_1519", "penta_0810", "cpn4_1924", "cpn4_0810")]), 1121L - min(nfal[c("penta_1519", "penta_0810", "cpn4_1924", "cpn4_0810")])),
  "**Time-series models.** For each age group, monthly counts $y_t$ were modeled as negative binomial with",
  "$$\\log E(y_t) = \\beta_0 + \\sum_{k=1}^{2} \\left[ \\beta_k \\sin\\left(\\frac{2\\pi k m_t}{12}\\right) + \\gamma_k \\cos\\left(\\frac{2\\pi k m_t}{12}\\right) \\right] + \\log\\left(\\frac{N_t}{12}\\right),$$",
  "where $m_t$ is the calendar month and $N_t$ the annual population, fitted to January 2015–December 2019 (stable baseline) or, with an added linear term in time, to January 2014–December 2019 (trend baseline). For each of 5,000 draws, coefficients were sampled from their asymptotic multivariate normal distribution and counts from the negative binomial distribution with the estimated dispersion. The recovered fraction was computed jointly within draws as",
  "$$\\frac{O_{2025} - E_{2025}}{E_{2020–2024} - O_{2020–2024}},$$",
  "where $O$ and $E$ are the observed and expected counts.",
  "**Age-structure models.** Counts were tabulated by 12 age groups and year for 2011–2013, 2015–2019 and 2025. Model 1 used one parameter per age group and period with a common negative binomial dispersion. Model 2 used year fixed effects, age-group main effects and age-by-epidemic interaction terms (11 non-reference age groups × 2 epidemics). Linear contrasts and their variances were obtained from the estimated coefficient covariance matrix. The trend of the log ratio of ratios with infant age was estimated by generalized least squares on the five infant estimates, with their covariance matrix and the midpoint of each age group in months. For the hypothetical scenario, the interaction coefficients were sampled 10,000 times from their asymptotic normal distribution, and case-fatality ratios were sampled from $\\mathrm{Beta}(d + 0.5,\\ n - d + 0.5)$, where $d$ is the number of deaths and $n$ the number of cases.",
  sprintf("**Event-study model.** Counts for 2007–2025 were tabulated by seven age groups (<2 months, 2–5 months, 6–11 months, 1–4, 5–9, 10–19 and ≥20 years). The model had year fixed effects, age-group effects and an age-by-year interaction for each age group other than children aged 1–4 years and each year outside 2015–2019. Each non-baseline year is then saturated, and estimating the negative binomial dispersion in the same fit drives it to infinity. The dispersion was therefore estimated in a model of the baseline years alone (θ = %.1f) and held fixed. Ratios relative to infants aged 6–11 months are differences between the <2 months and 6–11 months interactions of the same year. A quasi-Poisson version and a version restricted to laboratory-confirmed cases are shown in Supplementary Table S4.", dA_[grepl("eventos", modelo), theta]),
  "**Bayesian age–period–cohort model.** Counts $y_{a,p}$ by single year of age $a$ (0–19), calendar year $p$ (2007–2025) and cohort $c = p - a$ were modeled as Poisson with population offset,",
  "$$\\log E(y_{a,p}) = \\mu + f_A(a) + f_P(p) + f_C(c) + \\varepsilon_{a,p} + \\log N_{a,p},$$",
  "with scaled second-order random walks for $f_A$, $f_P$ and $f_C$, an independent overdispersion term $\\varepsilon_{a,p}$, and penalized-complexity priors on the precisions, $P(\\sigma > 1) = 0.01$, with $P(\\sigma > 0.5) = 0.01$ and $P(\\sigma > 2) = 0.01$ as alternatives. From 4,000 posterior samples (drawn with a fixed seed) we computed two contrasts whose weights $w_c$ satisfy $\\sum_{c} w_{c} = 0$ and $\\sum_{c} c w_{c} = 0$, so that they do not depend on the unidentifiable linear trend. The first is the mean of $f_C(c)$ for $c$ = 2019–2022 minus the least-squares line through $f_C(c)$ for $c$ = 2014–2018 evaluated at 2020.5; the second is $[f_C(2022) - f_C(2019)] - [f_C(2018) - f_C(2015)]$. For Supplementary Figure S4, the least-squares linear trend across all cohorts was removed within each sample.",
  sprintf("**Spatial models.** BYM2 models used the scaled intrinsic conditional autoregressive structure on a queen-contiguity graph (islands linked to their nearest neighbor), with penalized-complexity priors $P(\\sigma > 1) = 0.01$ for the total standard deviation and $P(\\phi < 0.5) = 2/3$ for the mixing parameter; alternative priors were $P(\\sigma > 0.5) = 0.01$, $P(\\sigma > 2) = 0.01$ and $P(\\phi < 0.5) = 0.5$. Expected counts were obtained by indirect standardization with national rates by eight age groups. In the joint antenatal-care model, the expected count for unit $i$ and age group $g$ used national rates by finer age groups within $g$, and the linear predictor was $\\alpha_g + \\beta_g z_i + \\mathbf{x}_i^\\top \\boldsymbol{\\gamma} + u_i + v_{ig}$, where $z_i$ is standardized antenatal-care coverage, $\\mathbf{x}_i$ the other covariates, $u_i$ a BYM2 component shared by all age groups and $v_{ig}$ an unstructured term; ratios of slopes are $\\exp(\\beta_g - \\beta_{g'})$. The 2025 versus 2011–2013 comparison of slope ratios assumes independent posteriors (normal approximation). The change model used two BYM2 components (baseline and 2025-specific), a 2025 intercept and covariate-by-2025 interactions. Posterior predictive checks used 500 posterior samples of the linear predictor and Poisson replicates. Kulldorff's spatial scan statistic used unit centroids in the national planar projection (MAGNA-SIRGAS Origen Nacional), the Poisson model with expected counts and 999 Monte Carlo replicates. Estimated BYM2 mixing parameter $\\phi$ (posterior mean): %s.",
          paste(sprintf("%s %.2f (%.2f–%.2f)", mo[hy$modelo], hy$mean, hy$`0.025quant`, hy$`0.975quant`), collapse = "; ")),
  sprintf("**Preliminary model.** Before the main analyses, a BYM2 model of municipal counts with the multidimensional poverty index, the rural, Indigenous and Afro-descendant shares, population density and departmental coverage with the third pentavalent dose averaged over 2019–2022 (Ministry of Health open data, datos.gov.co) estimated a relative risk per SD of departmental coverage of %s in 2025 and %s in 2011–2013 (posterior mean and 95%% credible interval). Departmental coverage assigned to municipalities is a coarse measure, and municipal coverage was not usable after 2020 (Supplementary Figure S6B). The model is reproduced by script 05 in the repository.",
          pr[epidemia == "2025", ci(rr, lo, hi)], pr[epidemia == "2011-2013", ci(rr, lo, hi)]),
  sprintf("**Infant death models.** Infants notified in 2011–2025 entered the models when department of residence and insurance scheme were known (%s of %s infants). Antenatal-care coverage in the period of each case was the municipal mean of 2011–2013, 2014–2019 or 2020–2024 (the last also for 2025, the latest year with data). The period-by-Indigenous interaction was parameterized with 2011–2013 as reference and tested with a likelihood-ratio test of three degrees of freedom (χ² = %.2f, p = %.3f). The trend in case fatality among Indigenous infants was estimated by logistic regression on the period coded 1–4 (odds ratio per period %.2f, p = %.2f).",
          format(fl[paso == "Analizados (casos completos)", n], big.mark = ","), format(fl[paso == "Lactantes notificados 2011-2025", n], big.mark = ","), lrt$chi2, lrt$p, ti$or_por_periodo, ti$p),
  "**Software.** R 4.6.1 with data.table, MASS (negative binomial), lme4 (mixed models), logistf (Firth), DHARMa 0.5.1 (simulated residuals), splines, INLA 26.8.7 (BYM2 and age–period–cohort models), spdep (graphs and Moran's I), SpatialEpi (Kulldorff scan), sf, ggplot2 and patchwork. Analyses ran with up to four threads. The scripts in the repository reproduce all results from the public source files.")
for (s in sm) sd <- P(sd, s)
sd <- body_end_section_portrait(sd, w = 8.5, h = 11)
sd <- H(sd, "Supplementary Figures and Tables", 1)   # estructura de la plantilla
sd <- H(sd, "Supplementary Tables", 2)
stab <- list(
  list(S1, "Fields and codes of the SIVIGILA microdata and municipal indicators used in the analyses.",
       "DIVIPOLA, national political-administrative division; PCR, polymerase chain reaction. All records had a final classification of confirmed (laboratory 9,730; clinical 5,729; epidemiological link 1,562)."),
  list(S2, "Observed and expected cases by age group under the stable (2015–2019) and trend (2014–2019) baselines.",
       "Expected counts and 95% prediction intervals (PI) from 5,000 simulations. Extrapolating the declining 2014–2019 trend gives lower expected counts and therefore larger O/E ratios. O/E, observed-to-expected ratio."),
  list(S3, "Sensitivity analyses for the ratios of incidence rate ratios of young infants (Model 2).",
       "RoR, ratio of incidence rate ratios relative to children aged 1–4 years (or 6–11 months in the last row) versus baseline; RoRR, ratio of RoRs, 2025 vs 2011–2013. Each column comes from the same fit as Table 3: finer infant groups, except the <2 months column (combined infant groups). The broad renotification flag marks records with the same date of birth, sex and municipality and symptom onset within 30 days of a previous record. Values are estimates (95% CI)."),
  list(S4, "Number of cases and ratio of incidence rate ratios of infants younger than 2 months relative to children aged 1–4 years and to infants aged 6–11 months, by year, compared with the 2015–2019 baseline (event-study model).",
       "Values are estimates (95% CI) from the negative binomial event-study model with the dispersion estimated in the baseline years; quasi-Poisson and laboratory-only versions are shown for comparison. Baseline years (2015–2019) define the reference."),
  list(S5, "Ratio of ratios of incidence comparing 2025 with different pre-Tdap periods, with two reference groups (Model 2, 2015–2019 baseline).",
       "Values are estimates (95% CI). The 2011–2013 row reproduces the main analysis; 2013, the year of introduction of maternal Tdap, is excluded from the 2011–2012 and 2007–2012 comparisons."),
  list(S6, "Excess risk among pandemic-era birth cohorts in 2025: specifications of the smooth-age negative binomial model.",
       "IRR for the pandemic-cohort indicator relative to a natural-spline age curve, with offset equal to the population and the baseline age-specific rate."),
  list(S7, "Identifiable cohort contrasts from the Bayesian age–period–cohort model under three priors for the precisions.",
       sprintf("Ratios are exponentiated contrasts of cohort effects (posterior median and 95%% credible interval, CrI); weights sum to zero and are orthogonal to a linear trend in cohort. Posterior predictive p values for the main prior: cells without cases %.2f, maximum count %.2f, Pearson discrepancy %.2f.",
               appc[estadistico == "ceros", p_predictivo], appc[estadistico == "maximo", p_predictivo], appc[estadistico == "pearson", p_predictivo])),
  list(S8, "Spatial clusters of confirmed pertussis in 2025 (Kulldorff's spatial scan statistic).",
       "Maximum window as percentage of the national population. SMR, standardized morbidity ratio (observed/expected). Indigenous share of the cluster population from DANE 2025 projections. Yavaraté (Vaupés) is a non-municipalized area."),
  list(S9, "Covariates of municipal pertussis risk in separate Bayesian BYM2 models (relative risk per 1 SD, 95% credible interval).",
       sprintf("Pentavalent and antenatal-care coverage refer to 2008–2010 in the 2011–2013 models and to 2015–2019 and 2019–2024, respectively, in the 2025 and change models. One SD corresponds to %.1f percentage points of antenatal-care coverage and %.1f of pentavalent coverage in the 2025 models, and to %.1f and %.1f in the 2011–2013 models. MPI, multidimensional poverty index.",
               sdv[["cpn4_1924"]], sdv[["penta_1519"]], sdv[["cpn4_0810"]], sdv[["penta_0810"]])),
  list(S10, "Antenatal-care coverage and incidence by age group: joint models and sensitivity analyses.",
       sprintf("Slopes are relative risks per 1 SD of municipal antenatal-care coverage; ratios compare slopes between age groups. Bayesian joint models give posterior medians and 95%% credible intervals; negative binomial (NB) models with department fixed effects give estimates and 95%% CI. The difference in ratios between 2025 and 2011–2013 assumes independent posteriors. %s",
               if (length(unique(pri[, ci(rr, lo, hi)])) == 1) sprintf("The ratio of slopes for <2 months vs 6–11 months in 2025 was %s under each of the three alternative priors.", pri[1, ci(rr, lo, hi)])
               else sprintf("With alternative priors, the ratio of slopes for <2 months vs 6–11 months in 2025 ranged from %s to %s.", pri[which.min(rr), ci(rr, lo, hi)], pri[which.max(rr), ci(rr, lo, hi)]))),
  list(S11, "Sensitivity analyses for factors associated with death among infants with confirmed pertussis, 2011–2025 (adjusted odds ratios, 95% CI).",
       sprintf("Models: %s. All models include the variables shown in Table 4; the interaction model adds Indigenous-by-period terms with 2011–2013 as reference (likelihood-ratio test p = %.3f), and its Indigenous row is the odds ratio in 2011–2013. The estimate for Indigenous × 2020–2024 is unstable (one death among non-Indigenous infants in that period).",
               paste(n11, collapse = "; "), lrt$p)),
  list(S12, "Crude case fatality among infants with confirmed pertussis, by ethnicity and period.", "Exact (Clopper–Pearson) 95% CI."),
  list(S13, "Approximate infant pertussis mortality by ethnicity.",
       "The Indigenous infant population was approximated as the sum over municipalities of the population aged 0 years multiplied by the municipal Indigenous share (DANE projections). Rate ratios with exact Poisson 95% CI. Estimates for 2020–2024 are unstable (one non-Indigenous death)."),
  list(S14, "Confirmed incidence, infant mortality and infant case fatality by population-weighted quintile of municipal multidimensional poverty (Q1 least poor, Q5 poorest).",
       sprintf("SIR, standardized incidence ratio; RII, relative index of inequality (values >1 indicate concentration in poorer municipalities); SII, slope index of inequality of infant mortality (deaths per 100,000 infant-years between the poorest and least poor ends of the ridit scale; 95%% interval from 10,000 simulations). Infant mortality per 100,000 infant-years. Exact 95%% CI. MPI quintile limits (percentage of people in multidimensional poverty, 2018 census): %s; units per quintile: %s.",
               paste(sprintf("Q%d %.1f–%.1f", qc5$quintil, qc5$ipm_min, qc5$ipm_max), collapse = ", "), paste(qc5$unidades, collapse = ", "))),
  list(S15, "Type of confirmation of pertussis cases by year, Colombia, 2007–2025.",
       sprintf("Missing data: date of birth %.1f%%, municipality of residence %.1f%%, age %.1f%%, symptom onset date %.1f%%, insurance scheme %.1f%%.",
               fal[variable == "fecha_nto", porcentaje], fal[variable == "mpio_res", porcentaje], fal[variable == "edad_dias", porcentaje], fal[variable == "ini_sin", porcentaje], fal[variable == "regimen", porcentaje])),
  list(S16, "Percentage of cases confirmed by laboratory, by age group and period.", "Values are percentage (laboratory-confirmed/all confirmed cases)."),
  list(S17, "Model diagnostics.",
       "Simulated quantile residuals from DHARMa (1,000 simulations); posterior predictive checks from 500 posterior samples. The event-study model is saturated outside the baseline years, so its diagnostics refer to the baseline years."))
for (i in seq_along(stab)) sd <- TABLA(sd, stab[[i]][[1]], sprintf("Supplementary Table S%d.", i), stab[[i]][[2]], stab[[i]][[3]], ancho_max = ANCHO_HORIZONTAL, partir_ic = i == 10, izquierda = i == 1)
sd <- body_end_section_landscape(sd, w = 8.5, h = 11)
sd <- H(sd, "Supplementary Figures", 2)
sleg <- c("FiguraS1_flujo_registros" = "Cases included in each analysis. Records without age were excluded from age-specific analyses, and records without a valid municipality code or with a code identifying only the department were excluded from municipal analyses.",
          "FiguraS2_sensibilidad_RoRR" = "Sensitivity analyses for the ratio of ratios (2025 vs 2011–2013) among infants younger than 6 weeks and 2 months. NB, negative binomial.",
          "FiguraS3_superficie_Lexis" = "Lexis surface of confirmed pertussis incidence per 100,000 by single year of age (0–19 years) and calendar year, Colombia, 2007–2025. Gray cells indicate no cases. Diagonal lines bound the 2019–2021 birth cohorts.",
          "FiguraS4_APC_cohortes" = "Cohort effects from the Bayesian age–period–cohort model (ages 0–19 years, 2007–2025) after removing a linear trend, which is not identifiable (posterior median and 95% credible interval). The shaded band marks the pandemic-era birth cohorts (2019–2022). Identifiable contrasts are given in Supplementary Table S7.",
          "FiguraS5_cronologia_2025" = "Median date of symptom onset by age group among cases with onset in 2025 (95% bootstrap confidence intervals).",
          "FiguraS6_calidad_datos" = "Data quality. (A) Share of confirmed cases confirmed by laboratory, by year. (B) Median municipal pentavalent coverage reported in TerriData versus national WHO/UNICEF estimates of coverage with the third dose of diphtheria–tetanus–pertussis vaccine (DTP3), showing the series break from 2021. WUENIC, WHO/UNICEF estimates of national immunization coverage.")
for (i in seq_along(sleg)) {
  png <- file.path("04_figuras", paste0(names(sleg)[i], "_preview.png")); dm <- dim(png::readPNG(png)); w <- min(6.5, 6.5 * dm[2] / 2126)
  if (i > 1) sd <- body_add_break(sd)
  sd <- body_add_img(sd, png, width = w, height = w * dm[1] / dm[2], style = "Kit figura")
  sd <- etiqueta(sd, sprintf("Supplementary Figure S%d.", i), sleg[[i]], "Normal")   # leyenda en un párrafo, rótulo en negrita (plantilla)
}
sd <- cursor_begin(sd) |> body_remove()
sd <- set_doc_properties(sd, title = "Supplementary Material", creator = "")
tmp_sup <- file.path(build, paste0("latex_", out_sup))
print(sd, target = tmp_sup)
ecuaciones_nativas(tmp_sup, out_sup)
ajustar_docx(out_sup, numerar_lineas = FALSE, plantilla = TRUE)

# --------------------------------------------------------------------------------
# Lista RECORD: segundo archivo de material suplementario, sobre la misma plantilla
# --------------------------------------------------------------------------------
rec_md <- readLines("05_manuscrito/RECORD_checklist_v2.md", encoding = "UTF-8")
filas_rec <- grep("^\\| ", rec_md, value = TRUE)
filas_rec <- filas_rec[!grepl("^\\| Item \\||^\\|-", filas_rec)]
partes <- lapply(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", filas_rec)), "(?<!\\\\)\\|", perl = TRUE),
                 function(v) trimws(gsub("\\\\\\|", "|", gsub("\\*", "", v))))
REC <- rbindlist(lapply(partes, function(v) { v <- c(v, rep("", 3 - length(v)))
  if (!nzchar(trimws(v[1]))) data.table(Item = trimws(v[2]), Recommendation = "", `Location in the manuscript` = "")
  else data.table(Item = v[1], Recommendation = v[2], `Location in the manuscript` = v[3]) }))
REC <- REC[!(Recommendation == "" & Item == shift(Item, fill = ""))]          # sección y tema con el mismo nombre
nota_rec <- paste(grep("^Locations give|^Checklist items reproduced", rec_md, value = TRUE), collapse = " ")
rc <- read_docx(PLANTILLA_SUP)
rc <- body_add_par(rc, "Supplementary Material", style = "Supplementary Material")
rc <- H(rc, "RECORD checklist", 1)
rc <- P(rc, "Items of the RECORD statement (REporting of studies Conducted using Observational Routinely-collected health Data) and of the STROBE statement on which it builds, with their location in the manuscript and its Supplementary Material. NA, not applicable.")
rc <- TABLA(rc, REC, "RECORD checklist.", "Reporting items and their location.", nota = nota_rec, ancho_max = ANCHO_TEXTO, izquierda = TRUE)
rc <- cursor_begin(rc) |> body_remove()
rc <- set_doc_properties(rc, title = "Supplementary Material: RECORD checklist", creator = "")
print(rc, target = out_rec)
ajustar_docx(out_rec, numerar_lineas = FALSE, plantilla = TRUE)
cat(sprintf("Manuscrito: resumen %d palabras; texto %d palabras; %d referencias citadas.\n", abs_words, main_words, length(num)))
