# =============================================================================
# 16_manuscrito.R
# Convierte 05_manuscrito/manuscrito_fuente.txt en:
#   Manuscrito_Frontiers_tosferina_vN.docx   (texto, tablas 1-4, referencias, leyendas, figuras)
#   Material_suplementario_tosferina_vN.docx (métodos suplementarios, tablas S1-S9, figuras S1-S4)
# Las citas [@clave] se numeran por orden de aparición (estilo Vancouver de Frontiers).
# Las tablas se construyen directamente desde 03_resultados/ (sin transcripción manual).
# Ejecutar desde la raíz: Rscript 02_scripts/16_manuscrito.R
# =============================================================================
suppressMessages({ library(officer); library(flextable); library(data.table); library(jsonlite); library(sf) })
VERSION <- "v2"       # manuscrito
VERSION_SUP <- "v1"   # material suplementario (sin cambios desde la v1)
FONT <- "Arial"; FS <- 11
R <- function(f) fread(file.path("03_resultados", f), encoding = "UTF-8")

TITLE <- "The 2025 pertussis resurgence in Colombia: an older age profile, relative protection of young infants after maternal Tdap, and widening Indigenous inequities. A nationwide analysis of surveillance microdata, 2007–2025"
RUNNING <- "Pertussis resurgence in Colombia, 2025"
AUTH <- list(list("Doris Cardona", "1", TRUE), list("Angela Segura Cardona", "2", FALSE),
             list("Alejandra Segura Cardona", "2", FALSE), list("Pedro Velez-Pardo", "2", TRUE))
AFF <- c("1" = "Independent Researcher, Medellín, Colombia", "2" = "Universidad CES, Medellín, Colombia")
CORR <- "Doris Cardona, doris.cardona@gmail.com; Pedro Velez-Pardo, pvelez@ces.edu.co"
KEYW <- "pertussis; maternal immunization; Tdap; immunity gap; health inequities; Indigenous peoples; Bayesian spatial analysis; Colombia"

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

# Recuento de palabras
sec_start <- function(pat) which(grepl(pat, src))[1]
wc <- function(x) sum(lengths(strsplit(trimws(gsub("\\*", "", x[nzchar(trimws(x)) & !grepl("^#|^\\{\\{", x)])), "\\s+")))
abs_words <- wc(src[(sec_start("^# Abstract") + 1):(sec_start("^# 1 Introduction") - 1)])
main_words <- wc(src[sec_start("^# 1 Introduction"):(sec_start("^# Data availability") - 1)])

# --------------------------------------------------------------------------------
# Utilidades de formato
# --------------------------------------------------------------------------------
fpt <- function(size = FS, bold = FALSE, italic = FALSE, color = "#000000", va = "baseline")
  fp_text(font.family = FONT, font.size = size, bold = bold, italic = italic, color = color, vertical.align = va)
runs <- function(s, size = FS) {          # **negrita** y *cursiva*
  toks <- regmatches(s, gregexpr("\\*\\*[^*]+\\*\\*|\\*[^*]+\\*|[^*]+", s))[[1]]
  lapply(toks, function(t) {
    if (grepl("^\\*\\*.*\\*\\*$", t)) ftext(gsub("^\\*\\*|\\*\\*$", "", t), fpt(size, bold = TRUE))
    else if (grepl("^\\*.*\\*$", t)) ftext(gsub("^\\*|\\*$", "", t), fpt(size, italic = TRUE))
    else ftext(t, fpt(size))
  })
}
P <- function(doc, s, align = "justify", size = FS, sp = 1.5, after = 6)
  body_add_fpar(doc, do.call(fpar, c(runs(s, size), list(fp_p = fp_par(text.align = align, line_spacing = sp, padding.bottom = after)))))
H <- function(doc, s, lvl) {
  sz <- c(14, 12, 11)[lvl]
  body_add_fpar(doc, fpar(ftext(s, fpt(sz, bold = TRUE, italic = lvl == 3)),
                          fp_p = fp_par(padding.top = c(14, 10, 8)[lvl], padding.bottom = 4, keep_with_next = TRUE, line_spacing = 1.2)))
}
en_num <- function(x) {                    # separador de miles en celdas de texto (inglés)
  vapply(x, function(s) { if (is.na(s)) return(s)
    m <- gregexpr("(?<![\\d.])\\d{4,}(?![\\d.])", s, perl = TRUE)
    regmatches(s, m) <- list(formatC(as.numeric(regmatches(s, m)[[1]]), format = "d", big.mark = ","))
    gsub("(\\d)-(\\d)", "\\1–\\2", s) }, "", USE.NAMES = FALSE)
}
TAB <- function(doc, dt, caption, note = NULL, widths = NULL, size = 8) {
  doc <- body_add_fpar(doc, fpar(ftext(caption, fpt(9.5, bold = TRUE)), fp_p = fp_par(padding.top = 8, padding.bottom = 3, keep_with_next = TRUE)))
  ft <- flextable(dt) |> font(fontname = FONT, part = "all") |> fontsize(size = size, part = "all") |>
    bold(part = "header") |> border_remove() |>
    hline_top(border = fp_border(color = "#000000", width = 1), part = "header") |>
    hline_bottom(border = fp_border(color = "#000000", width = 1), part = "body") |>
    hline_bottom(border = fp_border(color = "#000000", width = 0.5), part = "header") |>
    padding(padding.top = 1.5, padding.bottom = 1.5, part = "all") |> valign(valign = "top", part = "all")
  if (!is.null(widths)) for (j in seq_along(widths)) ft <- width(ft, j = j, width = widths[j])
  doc <- body_add_flextable(doc, ft)
  if (!is.null(note)) doc <- body_add_fpar(doc, fpar(ftext(note, fpt(8, color = "#333333")), fp_p = fp_par(padding.bottom = 10, line_spacing = 1.1)))
  doc
}
ci <- function(e, l, h, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f–%.", d, "f)"), e, l, h)

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

de <- R("series_deficit_exceso.csv")[especificacion == "principal" & periodo != "Balance 2020-2025"]
bl <- c("All ages" = "All ages", "<1 year" = "<1 year", "1–9 years" = "1–9 years", "10+ years" = "≥10 years")
T2 <- de[, .(`Age group` = factor(bl[banda], levels = bl), Period = fifelse(periodo == "Exceso 2025", "2025", "2020–2024"),
             Observed = format(observados, big.mark = ","), `Expected (95% PI)` = sprintf("%s (%s–%s)", format(round(esperados), big.mark = ","),
             format(round(esp_lo), big.mark = ","), format(round(esp_hi), big.mark = ",")), `O/E (95% PI)` = ci(razon_obs_esp, razon_lo, razon_hi),
             `Deficit recovered in 2025 (95% PI)` = fifelse(periodo == "Exceso 2025", sprintf("%.0f%% (%.0f%% to %.0f%%)", 100 * recuperacion_mediana, 100 * rec_lo, 100 * rec_hi), "—"))]
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

ob <- R("letalidad_modelos_OR.csv")
lt <- c("edad_g<6 semanas" = "Age <6 weeks", "edad_g6-8 semanas" = "Age 6–8 weeks", "edad_g2-3 meses" = "Age 2–3 months", "edad_g4-5 meses" = "Age 4–5 months",
        "periodo42014-2019" = "Period 2014–2019", "periodo42020-2024" = "Period 2020–2024", "periodo42025" = "Period 2025",
        "etnia3Indígena" = "Indigenous", "etnia3Afrodescendiente" = "Afro-descendant", "regimen4Subsidiado" = "Subsidized insurance",
        "regimen4No asegurado" = "Uninsured", "regimen4Otro" = "Other insurance schemes", "rural" = "Rural residence", "mujer" = "Female",
        "migrante" = "Migrant", "lab" = "Laboratory-confirmed", "z_cpn4" = "Municipal antenatal-care coverage (per SD)")
refrow <- c("Age <6 weeks" = "Age 6–11 months (reference)", "Period 2014–2019" = "Period 2011–2013 (reference)",
            "Indigenous" = "No ethnic self-identification (reference)", "Subsidized insurance" = "Contributory insurance (reference)")
mk <- function(m) ob[modelo == m, .(termino, v = ci(or, lo, hi))]
T4 <- Reduce(function(x, y) merge(x, y, by = "termino", all = TRUE),
             list(setnames(mk("Mixto principal"), "v", "Mixed model"), setnames(mk("Firth (sin efecto aleatorio)"), "v", "Firth regression"),
                  setnames(mk("Mixto + % nacidos con >=4 controles prenatales (municipal, DE)"), "v", "Mixed model + antenatal care")))
T4 <- T4[match(names(lt), termino)][!is.na(termino)][, Variable := lt[termino]][, termino := NULL]
setcolorder(T4, "Variable")
T4 <- rbindlist(lapply(seq_len(nrow(T4)), function(i) { r <- T4[i]
  if (r$Variable %in% names(refrow)) rbind(data.table(Variable = refrow[[r$Variable]], `Mixed model` = "1", `Firth regression` = "1", `Mixed model + antenatal care` = "1"), r) else r }))
for (j in 2:4) set(T4, which(is.na(T4[[j]])), j, "—")
nB <- ob[modelo == "Mixto principal", n][1]; dB <- ob[modelo == "Mixto principal", muertes][1]
nA <- ob[grepl("prenatales", modelo), n][1]; dA <- ob[grepl("prenatales", modelo), muertes][1]

# --------------------------------------------------------------------------------
# Documento principal
# --------------------------------------------------------------------------------
sect <- prop_section(page_size = page_size(orient = "portrait", width = 8.27, height = 11.69),
                     page_margins = page_mar(top = 1, bottom = 1, left = 1, right = 1, header = 0.4, footer = 0.4),
                     footer_default = block_list(fpar(run_word_field("PAGE", prop = fpt(9)), fp_p = fp_par(text.align = "center"))))
doc <- read_docx() |> body_set_default_section(sect)
doc <- body_add_fpar(doc, fpar(ftext(TITLE, fpt(15, bold = TRUE)), fp_p = fp_par(line_spacing = 1.2, padding.bottom = 10)))
au <- unlist(lapply(seq_along(AUTH), function(i) { a <- AUTH[[i]]
  list(ftext(a[[1]], fpt(FS)), ftext(paste0(a[[2]], if (a[[3]]) "*" else ""), fpt(FS, va = "superscript")),
       ftext(if (i < length(AUTH)) ", " else "", fpt(FS))) }), recursive = FALSE)
doc <- body_add_fpar(doc, do.call(fpar, c(au, list(fp_p = fp_par(padding.bottom = 8)))))
for (k in names(AFF)) doc <- body_add_fpar(doc, fpar(ftext(k, fpt(9.5, va = "superscript")), ftext(paste0(" ", AFF[[k]]), fpt(9.5)), fp_p = fp_par(padding.bottom = 2)))
doc <- body_add_fpar(doc, fpar(ftext("*Correspondence: ", fpt(9.5, bold = TRUE)), ftext(CORR, fpt(9.5)), fp_p = fp_par(padding.top = 6, padding.bottom = 6)))
doc <- body_add_fpar(doc, fpar(ftext("Keywords: ", fpt(9.5, bold = TRUE)), ftext(KEYW, fpt(9.5)), fp_p = fp_par(padding.bottom = 2)))
doc <- body_add_fpar(doc, fpar(ftext("Running title: ", fpt(9.5, bold = TRUE)), ftext(RUNNING, fpt(9.5)), fp_p = fp_par(padding.bottom = 2)))
doc <- body_add_fpar(doc, fpar(ftext("Article type: ", fpt(9.5, bold = TRUE)), ftext("Original Research; Research Topic “Bordetella Pertussis in the 21st Century: Surveillance, Immunity, and Control of a Resurgent Pathogen”", fpt(9.5)), fp_p = fp_par(padding.bottom = 2)))
doc <- body_add_fpar(doc, fpar(ftext("Word count: ", fpt(9.5, bold = TRUE)), ftext(sprintf("abstract %d; main text %s; %d tables; %d figures; %d references", abs_words, format(main_words, big.mark = ","), 4, 6, length(num)), fpt(9.5)), fp_p = fp_par(padding.bottom = 2)))
doc <- body_add_break(doc)

tables <- list(
  "1" = list(T1, "Table 1. Characteristics of confirmed pertussis cases by period, Colombia, 2007–2025.",
             "ᵃ Hospitalization was not recorded in 2008–2009. IQR, interquartile range. Incidence uses DANE population projections; infant subgroups use the population aged 0 years multiplied by the fraction of the year spanned.",
             c(2.15, 0.95, 1.05, 0.95, 0.95, 1.05)),
  "2" = list(T2, "Table 2. Observed and expected confirmed pertussis cases by age group during the pandemic period (2020–2024) and the 2025 epidemic.",
             "Expected cases from negative binomial models with seasonal (Fourier) terms and population offset fitted to 2015–2019; 95% prediction intervals (PI) from 5,000 simulations combining parameter uncertainty and negative binomial sampling. Deficit recovered = excess in 2025 / deficit in 2020–2024. O/E, observed-to-expected ratio.",
             c(0.95, 0.75, 0.75, 1.35, 1.25, 2.05)),
  "3" = list(T3, "Table 3. Age-specific incidence rate ratios and ratios of incidence rate ratios in the 2011–2013 and 2025 epidemics relative to the 2015–2019 baseline.",
             "IRR: Model 1 (negative binomial, one parameter per age group and period). RoR and ratio of ratios: Model 2 (negative binomial with year fixed effects; reference group children aged 1–4 years). The <2 months row was estimated in a separate Model 2 fit with combined infant groups. Values are estimates (95% CI).",
             c(1.4, 1.05, 1.05, 1.0, 1.0, 1.4)),
  "4" = list(T4, sprintf("Table 4. Factors associated with death among infants with confirmed pertussis, Colombia, 2011–2025 (n = %s; %d deaths).", format(nB, big.mark = ","), dB),
             sprintf("Adjusted odds ratios (95%% CI). Mixed model: logistic regression with department random intercept. Firth regression: penalized likelihood without random effects. The antenatal-care model includes the municipal percentage of live births with ≥4 antenatal visits (2019–2024, standardized; n = %s; %d deaths). All models adjust for all variables shown.", format(nA, big.mark = ","), dA),
             c(2.4, 1.3, 1.3, 1.9)))

body_lines <- src[sec_start("^# Abstract"):length(src)]
for (ln in body_lines) {
  if (!nzchar(trimws(ln))) next
  if (grepl("^\\{\\{TABLE:", ln)) { k <- sub("^\\{\\{TABLE:(\\d+)\\}\\}.*", "\\1", ln); tb <- tables[[k]]
    doc <- TAB(doc, tb[[1]], tb[[2]], tb[[3]], tb[[4]]); next }
  if (grepl("^### ", ln)) { doc <- H(doc, sub("^### ", "", ln), 3); next }
  if (grepl("^## ", ln))  { doc <- H(doc, sub("^## ", "", ln), 2); next }
  if (grepl("^# ", ln))   { h <- sub("^# ", "", ln)
    if (h == "1 Introduction") doc <- body_add_break(doc)
    doc <- H(doc, h, 1); next }
  doc <- P(doc, ln)
}
# Referencias
doc <- H(doc, "References", 1)
for (k in names(num)) doc <- body_add_fpar(doc, fpar(ftext(sprintf("%d. %s", num[[k]], refs[[k]]), fpt(9.5)),
                                                    fp_p = fp_par(padding.bottom = 3, line_spacing = 1.15, padding.left = 0)))
# Leyendas y figuras
leg <- c(
  "Figura1_dinamica_deuda" = "Figure 1. Pertussis dynamics in Colombia, 2007–2025. (A) Weekly confirmed cases by week of symptom onset; vertical lines mark the introduction of maternal Tdap (2013), the COVID-19 lockdown (March 2020) and the 2025 response (accelerated infant schedule and earlier maternal Tdap). (B) Observed and expected monthly cases by age group, 2018–2025; expected values (median and 95% prediction interval) from negative binomial models with seasonal terms fitted to 2015–2019. O/E, observed-to-expected ratio.",
  "Figura2_edad_firma_Tdap" = "Figure 2. Age structure of confirmed cases and the population-level signature of maternal Tdap. (A) Age distribution of cases by year (annual totals above bars). (B) Ratios of incidence rate ratios relative to children aged 1–4 years, for each epidemic versus the 2015–2019 baseline (negative binomial model with year fixed effects). (C) Ratio of ratios comparing the 2025 and 2011–2013 epidemics among infant age groups, and counterfactual additional cases under the 2011–2013 age pattern. Bars are 95% confidence intervals; UI, uncertainty interval.",
  "Figura3_superficie_Lexis" = "Figure 3. Lexis surface of confirmed pertussis incidence per 100,000 by single year of age (0–19 years) and calendar year, Colombia, 2007–2025. Grey cells indicate no cases. Diagonal lines bound the 2019–2021 birth cohorts.",
  "Figura4_mapas_BYM2_cambio_conglomerados" = "Figure 4. Municipal risk of confirmed pertussis. (A, B) Age-standardized relative risk (posterior median) from Bayesian BYM2 models in the 2011–2013 and 2025 epidemics. (C) Change in 2025 relative to each municipality's own 2015–2019 baseline, relative to the national change. Black outlines in (B) mark clusters detected by Kulldorff's spatial scan statistic (p < 0.05). The San Andrés archipelago is not shown.",
  "Figura5_control_prenatal_control_negativo" = "Figure 5. Antenatal-care and pentavalent coverage as municipal predictors of pertussis risk. Relative risks per standard deviation (95% credible intervals) from BYM2 models adjusted for multidimensional poverty, rural, Indigenous and Afro-descendant shares and population density. The 2011–2013 models (before maternal Tdap) and the model for people aged ≥10 years in 2025 serve as negative controls for the antenatal-care association.",
  "Figura6_letalidad_inequidad" = "Figure 6. Infant death and inequalities. (A) Adjusted odds ratios for death among infants with confirmed pertussis, 2011–2025 (mixed-effects logistic regression with department random intercept). (B) Infant case fatality by ethnicity and period (95% exact confidence intervals). (C) Infant pertussis mortality per 100,000 infants by population-weighted quintile of municipal multidimensional poverty; RII, relative index of inequality.")
doc <- body_add_break(doc); doc <- H(doc, "Figure legends", 1)
for (k in names(leg)) doc <- P(doc, leg[[k]], size = 10, sp = 1.3)
for (k in names(leg)) {
  png <- file.path("04_figuras", paste0(k, "_preview.png")); dm <- dim(png::readPNG(png)); w <- 6.27
  doc <- body_add_break(doc)
  doc <- body_add_img(doc, png, width = w, height = min(9, w * dm[1] / dm[2]))
  doc <- body_add_fpar(doc, fpar(ftext(sub("\\..*$", "", leg[[k]]), fpt(9, bold = TRUE))))
}
out_main <- sprintf("Manuscrito_Frontiers_tosferina_%s.docx", VERSION)
print(doc, target = out_main)

# Numeración continua de líneas (requisito de revisión de Frontiers)
add_line_numbers <- function(f) {
  td <- tempfile("docx_"); dir.create(td); unzip(f, exdir = td)
  x <- readLines(file.path(td, "word/document.xml"), encoding = "UTF-8", warn = FALSE); x <- paste(x, collapse = "\n")
  ln <- '<w:lnNumType w:countBy="1" w:restart="continuous"/>'
  if (!grepl("lnNumType", x)) x <- if (grepl("<w:cols", x)) sub("(<w:cols)", paste0(ln, "\\1"), x) else sub("</w:sectPr>", paste0(ln, "</w:sectPr>"), x)
  writeLines(x, file.path(td, "word/document.xml"), useBytes = TRUE)
  owd <- setwd(td); on.exit(setwd(owd))
  unlink(file.path(owd, f)); zip(file.path(owd, f), files = list.files(".", recursive = TRUE, all.files = TRUE), flags = "-r9Xq")
}
add_line_numbers(out_main)

# --------------------------------------------------------------------------------
# Material suplementario
# --------------------------------------------------------------------------------
geo <- st_drop_geometry(st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE))
nm <- setNames(tools::toTitleCase(tolower(geo$MPIO_CNMBR)), geo$MPIO_CDPMP)
qc <- R("calidad_confirmacion_por_anio.csv"); fal <- R("calidad_faltantes.csv")
S1 <- qc[, .(Year = anio, Laboratory = Laboratorio, `Epidemiological link` = `Nexo epidemiológico`, Clinical = Clínica, Total = total, `Laboratory, %` = sprintf("%.1f", pct_laboratorio))]
dt <- R("series_deficit_exceso.csv")[especificacion != "principal" & periodo != "Balance 2020-2025"]
S2 <- dt[, .(`Age group` = bl[banda], Period = fifelse(periodo == "Exceso 2025", "2025", "2020–2024"), Observed = observados,
             `Expected (95% PI)` = sprintf("%.0f (%.0f–%.0f)", esperados, esp_lo, esp_hi), `O/E (95% PI)` = ci(razon_obs_esp, razon_lo, razon_hi))]
se <- R("edad_sensibilidad.csv")
an_en <- c("Main analysis", "Laboratory-confirmed only", "Probable duplicates removed", "Year of symptom onset", "Baseline 2014–2019",
           "Baseline 2017–2019", "Excluding Antioquia", "Quasi-Poisson", "Reference group 6–11 months")
S3 <- se[, .(Analysis = an_en, `RoR <6 wk, 2011–13` = `RoR <6 sem 2011-13`, `RoR <6 wk, 2025` = `RoR <6 sem 2025`, `RoRR <6 wk` = `RoRR <6 sem`,
             `RoRR <2 mo` = `RoRR <2 m`, `RoRR 2–3 mo` = `RoRR 2-3 m`, `RoR 10–14 y, 2025` = `RoR 10-14 a 2025`)]
co <- R("cohortes_pandemicas_sensibilidad.csv")
S4 <- co[, .(Baseline = base, Cohorts = cohortes, `Spline df` = df, `IRR (95% CI)` = ci(irr, lo, hi), p = sprintf("%.3f", p))][order(Baseline, Cohorts, `Spline df`)]
lc <- R("letalidad_cruda_etnia_periodo.csv")
eth <- c("Otro/ninguno" = "No ethnic self-identification", "Afrodescendiente" = "Afro-descendant", "Indígena" = "Indigenous")
S5 <- lc[, .(Period = gsub("-", "–", periodo4), Ethnicity = eth[etnia3], Infants = lactantes, Deaths = muertes, `Case fatality (95% CI)` = gsub("(\\d)-(\\d)", "\\1–\\2", letalidad))]
me <- R("mortalidad_lactantes_etnia_aproximada.csv")
S6 <- me[, .(Period = gsub("-", "–", periodo), `Indigenous deaths` = m_ind, `Indigenous rate /100,000` = sprintf("%.1f", tasa_ind),
             `Non-Indigenous deaths` = m_noind, `Non-Indigenous rate /100,000` = sprintf("%.1f", tasa_noind), `Rate ratio (95% CI)` = ci(rr, rr_lo, rr_hi, 1))]
iq <- R("inequidades_ipm_quintiles.csv")
S7 <- iq[, .(Period = gsub("-", "–", periodo), Quintile = quintil, Cases = O, SIR = sprintf("%.2f", sir), `Infant mortality /100,000` = sprintf("%.2f", mort_inf_100k),
             `Infant case fatality, %` = sprintf("%.1f", 100 * letalidad), `RII incidence` = gsub("(\\d)-(\\d)", "\\1–\\2", RII_incidencia),
             `RII infant mortality` = gsub("(\\d)-(\\d)", "\\1–\\2", RII_mortalidad_infantil))]
k1 <- R("espacial_kulldorff_2025.csv"); k2 <- R("espacial_kulldorff_2025_ventana20.csv")
er <- readRDS("01_datos/procesados/espacial_resultados.rds")
S8 <- rbind(k1[, .(Window = "10%", Cluster = cluster, Municipalities = n_municipios, Observed = observados, Expected = sprintf("%.1f", esperados),
                   SMR = sprintf("%.2f", smr), p = sprintf("%.3f", p),
                   `Municipalities (first 12)` = vapply(er$kd_ids[cluster], function(v) paste(head(nm[v], 12), collapse = ", "), ""))],
            k2[, .(Window = "20%", Cluster = cluster, Municipalities = n_municipios, Observed = observados, Expected = sprintf("%.1f", esperados),
                   SMR = sprintf("%.2f", smr), p = sprintf("%.3f", p),
                   `Municipalities (first 12)` = vapply(strsplit(municipios, " "), function(v) paste(head(nm[v], 12), collapse = ", "), ""))])
fe <- R("espacial_bym2_efectos_fijos.csv"); fc <- R("espacial_cambio_efectos_fijos.csv")
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

sd <- read_docx() |> body_set_default_section(sect)
sd <- body_add_fpar(sd, fpar(ftext("Supplementary Material", fpt(15, bold = TRUE)), fp_p = fp_par(padding.bottom = 4)))
sd <- body_add_fpar(sd, fpar(ftext(TITLE, fpt(10.5, italic = TRUE)), fp_p = fp_par(padding.bottom = 10, line_spacing = 1.2)))
sd <- H(sd, "Supplementary Methods", 1)
sm <- c(
  "**Data preparation.** Annual microdata files for event 800 (2007–2025) were merged and dates were parsed from the source format. Age at onset was computed as the difference between the consultation date and the date of birth when both were available and consistent with the reported age (difference ≤31 days or ≤10% of the reported age, or same completed years), and from the reported age and unit otherwise. Symptom onset dates later than consultation or more than 120 days before consultation were replaced by the consultation date in time-series analyses. Municipality of residence was coded with the five-digit DIVIPOLA code; Barrancominas and Belén de Bajirá, created after 2018, were merged with Mapiripán and Mutatá, respectively. Probable duplicates were defined as records sharing date of birth, sex and municipality of residence plus the same symptom-onset or consultation date; one record per group was retained in the sensitivity analysis (133 records excluded).",
  "**Time-series models.** For each age group, monthly counts y[t] were modelled as negative binomial with log E(y[t]) = β0 + Σk=1..2 [βk sin(2πkm/12) + γk cos(2πkm/12)] + log(N[t]/12), fitted to January 2015–December 2019. For each of 5,000 draws, coefficients were sampled from their asymptotic multivariate normal distribution and counts from the negative binomial distribution with the estimated dispersion. The recovered fraction was computed jointly within draws as (observed 2025 − expected 2025)/(expected 2020–2024 − observed 2020–2024).",
  "**Age-structure models.** Counts were tabulated by 12 age groups and year for 2011–2013, 2015–2019 and 2025. Model 1 used one parameter per age group and period with a common negative binomial dispersion. Model 2 used year fixed effects, age-group main effects and age-by-epidemic interaction terms (11 non-reference age groups × 2 epidemics). Linear contrasts and their variances were obtained from the estimated coefficient covariance matrix. For the counterfactual, the interaction coefficients were sampled 10,000 times from their asymptotic normal distribution; case-fatality ratios were sampled from Beta(d + 0.5, n − d + 0.5).",
  "**Bayesian age–period–cohort model.** Counts by single year of age (0–19), calendar year (2007–2025) and cohort (year − age) were modelled as Poisson with population offset, log-linear predictor μ + f(age) + f(period) + f(cohort) + ε, second-order random walks (scaled) for age, period and cohort, and an independent overdispersion term ε, all with penalized-complexity priors on the precision (P(σ > 1) = 0.01). Detrended cohort effects were obtained by removing, within each of 2,000 posterior samples, the least-squares linear trend across cohorts.",
  "**Spatial models.** BYM2 models used the scaled intrinsic conditional autoregressive structure on a queen-contiguity graph (islands linked to their nearest neighbour), with penalized-complexity priors P(σ > 1) = 0.01 for the total standard deviation and P(φ < 0.5) = 2/3 for the mixing parameter. The change model used two BYM2 components (baseline and 2025-specific), a 2025 intercept and covariate-by-2025 interactions. Expected counts were obtained by indirect standardization with national rates by eight age groups. Kulldorff's spatial scan statistic used municipal centroids in the national planar projection (MAGNA-SIRGAS Origen Nacional), the Poisson model with expected counts and 999 Monte Carlo replicates.",
  "**Software.** R 4.6.1 with data.table, MASS (negative binomial), lme4 (mixed models), logistf (Firth), splines, INLA 26.8.7 (BYM2, APC), spdep (graphs and Moran's I), SpatialEpi (Kulldorff scan), sf, ggplot2 and patchwork. Analyses ran with four threads. The scripts (01–16) reproduce all results from the raw public files.")
for (s in sm) sd <- P(sd, s, size = 10, sp = 1.3)
sd <- body_add_fpar(sd, fpar(ftext(sprintf("Estimated BYM2 mixing parameter φ (proportion of spatially structured variance): %s.",
                                           paste(sprintf("%s %.2f (%.2f–%.2f)", mo[hy$modelo], hy$mean, hy$`0.025quant`, hy$`0.975quant`), collapse = "; ")), fpt(9.5)),
                               fp_p = fp_par(padding.bottom = 8)))
sd <- body_add_break(sd); sd <- H(sd, "Supplementary Tables", 1)
sd <- TAB(sd, S1, "Supplementary Table S1. Type of confirmation of pertussis cases by year, Colombia, 2007–2025.",
          sprintf("Missing data: date of birth %.1f%%, municipality of residence %.1f%%, age %.1f%%, symptom onset date %.1f%%.",
                  fal[variable == "fecha_nto", porcentaje], fal[variable == "mpio_res", porcentaje], fal[variable == "edad_dias", porcentaje], fal[variable == "ini_sin", porcentaje]),
          c(0.8, 1.1, 1.4, 1.0, 0.9, 1.1))
sd <- TAB(sd, S2, "Supplementary Table S2. Observed and expected cases using a 2014–2019 baseline with linear trend (sensitivity analysis).",
          "Extrapolating the declining 2014–2019 trend yields lower expected counts and therefore larger O/E ratios than the main (no-trend) baseline.", c(1.1, 0.9, 0.9, 1.6, 1.7))
sd <- TAB(sd, S3, "Supplementary Table S3. Sensitivity analyses for the ratios of incidence rate ratios of young infants (Model 2).",
          "RoR, ratio of incidence rate ratios relative to children aged 1–4 years (or 6–11 months in the last row) versus baseline; RoRR, ratio of RoRs, 2025 vs 2011–2013. Values are estimates (95% CI).",
          c(1.45, 0.82, 0.82, 0.82, 0.82, 0.82, 0.72), size = 7)
sd <- TAB(sd, S4, "Supplementary Table S4. Excess risk among pandemic-era birth cohorts in 2025: specifications of the smooth-age negative binomial model.",
          "IRR for the pandemic-cohort indicator relative to a natural-spline age curve, with offset equal to the population and the baseline age-specific rate.", c(1.2, 1.2, 1.0, 1.7, 0.8))
sd <- TAB(sd, S5, "Supplementary Table S5. Crude case fatality among infants with confirmed pertussis, by ethnicity and period.", "Exact (Clopper–Pearson) 95% CI.", c(1.0, 2.2, 0.9, 0.9, 1.6))
sd <- TAB(sd, S6, "Supplementary Table S6. Approximate infant pertussis mortality by ethnicity.",
          "The Indigenous infant population was approximated as the sum over municipalities of the population aged 0 years multiplied by the municipal Indigenous share (DANE projections). Rate ratios with exact Poisson 95% CI. Estimates for 2020–2024 are unstable (one non-Indigenous death).",
          c(0.9, 0.9, 1.05, 1.0, 1.15, 1.2))
sd <- TAB(sd, S7, "Supplementary Table S7. Confirmed incidence, infant mortality and infant case fatality by population-weighted quintile of municipal multidimensional poverty (Q1 least poor, Q5 poorest).",
          "SIR, standardized incidence ratio; RII, relative index of inequality (values >1 indicate concentration in poorer municipalities).", c(0.75, 0.6, 0.6, 0.55, 0.95, 0.85, 0.95, 0.95), size = 7.5)
sd <- TAB(sd, S8, "Supplementary Table S8. Spatial clusters of confirmed pertussis in 2025 (Kulldorff's spatial scan statistic).",
          "Maximum window as percentage of the national population. SMR, standardized morbidity ratio (observed/expected).", c(0.55, 0.55, 0.75, 0.65, 0.65, 0.5, 0.45, 2.2), size = 7)
sd <- TAB(sd, S9, "Supplementary Table S9. Covariates of municipal pertussis risk in Bayesian BYM2 models (relative risk per 1 SD, 95% credible interval).",
          "Pentavalent and antenatal-care coverage refer to 2008–2010 in the 2011–2013 models and to 2015–2019 and 2019–2024, respectively, in the 2025 and change models. MPI, multidimensional poverty index.",
          c(1.25, 0.85, 0.85, 0.85, 0.85, 0.85, 0.9), size = 7)
sleg <- c("FiguraS1_sensibilidad_RoRR" = "Supplementary Figure S1. Sensitivity analyses for the ratio of ratios (2025 vs 2011–2013) among infants younger than 6 weeks and 2 months.",
          "FiguraS2_calidad_datos" = "Supplementary Figure S2. Data quality. (A) Share of confirmed cases confirmed by laboratory, by year. (B) Median municipal pentavalent coverage reported in TerriData versus national WHO/UNICEF estimates of DTP3 coverage, showing the series break from 2021.",
          "FiguraS3_APC_cohortes" = "Supplementary Figure S3. Detrended cohort effects (curvature) from the Bayesian age–period–cohort model, ages 0–19 years, 2007–2025 (posterior median and 95% credible interval).",
          "FiguraS4_cronologia_2025" = "Supplementary Figure S4. Median date of symptom onset by age group during the 2025 epidemic (95% bootstrap confidence intervals).")
for (k in names(sleg)) {
  png <- file.path("04_figuras", paste0(k, "_preview.png")); dm <- dim(png::readPNG(png)); w <- if (grepl("S3|S4", k)) 5.2 else 6.27
  sd <- body_add_break(sd); sd <- body_add_img(sd, png, width = w, height = w * dm[1] / dm[2])
  sd <- P(sd, sleg[[k]], size = 9.5, sp = 1.2)
}
print(sd, target = sprintf("Material_suplementario_tosferina_%s.docx", VERSION_SUP))
cat(sprintf("Manuscrito: resumen %d palabras; texto %d palabras; %d referencias citadas.\n", abs_words, main_words, length(num)))
