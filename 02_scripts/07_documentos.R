# =============================================================================
# 07_documentos.R
# Genera los entregables en Word (versión indicada en VERSION):
#   Resumen_Frontiers_tosferina_vN.docx           (resumen en inglés + traducción)
#   Propuesta_y_avances_tosferina_vN.docx         (ideas, novedad, datos, resultados)
# Ejecutar desde la raíz del proyecto: Rscript 02_scripts/07_documentos.R
# =============================================================================
suppressMessages({ library(officer); library(flextable); library(data.table) })
VERSION <- "v5"
AUTORES <- c("Doris Cardona", "Angela Segura Cardona", "Alejandra Segura Cardona", "Pedro Velez-Pardo")
AFIL_DE <- c(1, 2, 2, 2)                     # afiliación de cada autor (índice en AFILIACIONES)
AFILIACIONES <- c("Independent Researcher, Medellín, Colombia", "Universidad CES, Medellín, Colombia")
CORRESP <- c(1, 4)                           # correspondencia: Doris Cardona y (tentativo) Pedro Velez-Pardo
CORREOS <- c("doris.cardona@gmail.com", "asegura@ces.edu.co", "dsegura@ces.edu.co", "pvelez@ces.edu.co")
ORCID <- c("https://orcid.org/0000-0003-4338-588X", "https://orcid.org/0000-0002-0010-1413",
           "https://orcid.org/0000-0002-1624-0952", "https://orcid.org/0009-0006-3850-0440")   # verificados (control y nombre) el 3/11/2026
SUP <- c("¹", "²", "³", "⁴")
FONT <- "Arial"

ft_default <- function(x) {   # sin autofit para respetar los anchos fijados
  x <- font(x, fontname = FONT, part = "all"); x <- fontsize(x, size = 8.5, part = "all")
  x <- bold(x, part = "header"); x <- bg(x, bg = "#eef3fb", part = "header")
  x <- border_remove(x); x <- hline_top(x, border = fp_border(color = "#52514e", width = 1), part = "header")
  x <- hline_bottom(x, border = fp_border(color = "#52514e", width = 1), part = "body")
  x <- hline_bottom(x, border = fp_border(color = "#c3c2b7", width = 0.5), part = "header")
  x <- padding(x, padding.top = 2, padding.bottom = 2, part = "all")
  x
}
tx  <- function(s, bold = FALSE, italic = FALSE, size = 10.5, color = "#0b0b0b")
  ftext(s, fp_text(font.family = FONT, font.size = size, bold = bold, italic = italic, color = color))
par_fp <- fp_par(text.align = "justify", padding.bottom = 6, line_spacing = 1.15)
add_p  <- function(doc, ..., align = "justify") body_add_fpar(doc, fpar(..., fp_p = fp_par(text.align = align, padding.bottom = 6, line_spacing = 1.15)))
add_h  <- function(doc, s, level = 1) body_add_par(doc, s, style = paste("heading", level))
add_bullets <- function(doc, items, size = 10.5) {   # viñetas con sangría (la plantilla base no trae estilo de lista)
  for (it in items) doc <- body_add_fpar(doc, fpar(tx("•  ", size = size), tx(it, size = size),
                                                   fp_p = fp_par(text.align = "left", padding.left = 14, padding.bottom = 3, line_spacing = 1.1)))
  doc
}
wc <- function(s) length(strsplit(gsub("[[:space:]]+", " ", trimws(s)), " ")[[1]])

# --- Cifras clave (leídas de los resultados para que el texto quede sincronizado) ----------
an   <- fread("03_resultados/tabla_serie_anual.csv")
ror  <- fread("03_resultados/tabla_razon_de_razones_2m.csv")
irr  <- fread("03_resultados/tabla_irr_edad.csv")
orB  <- fread("03_resultados/modelo_B_muerte_lactantes_OR.csv")
coh  <- fread("03_resultados/modelo_A_cohortes_pandemicas.csv")
spfe <- fread("03_resultados/modelo_C_bym2_efectos_fijos.csv")
f2 <- function(x) formatC(x, format = "f", digits = 2)
ci <- function(e, l, h) sprintf("%s; 95%% CI %s–%s", f2(e), f2(l), f2(h))
r25  <- ror[comparacion == "2025 vs 2015-2019"]; r11 <- ror[comparacion == "2011-2013 vs 2015-2019"]
r25l <- ror[comparacion == "2025 vs 2015-2019 (solo laboratorio)"]; r11l <- ror[comparacion == "2011-2013 vs 2015-2019 (solo laboratorio)"]
i2m25 <- irr[comparacion == "2025 vs 2015-2019" & grupo_edad == "<2 m"]
i2m11 <- irr[comparacion == "2011-2013 vs 2015-2019" & grupo_edad == "<2 m"]
oInd <- orB[termino == "etnia3Indígena"]; oRur <- orB[termino == "rural"]
cPan <- coh[definicion == "2020-2022"]
pPen <- spfe[epidemia == "2025" & termino == "z_penta3"]

# =============================================================================
# 1. RESUMEN PARA FRONTIERS
# =============================================================================
title_en <- "The 2025 pertussis resurgence in Colombia: an older age profile, relative protection of young infants after maternal Tdap, and persistent Indigenous inequities. A nationwide analysis of surveillance microdata, 2007–2025"
abs_en <- list(
  Background = "Pertussis resurged worldwide after the COVID-19 pandemic. In 2025 Colombia, which uses whole-cell primary vaccines and introduced maternal Tdap in 2013, had its largest epidemic in twelve years. We compared it with the 2011–2013 epidemic, which preceded maternal immunization.",
  Methods = "Nationwide population-based study of all confirmed pertussis cases notified to the national surveillance system (SIVIGILA) in 2007–2025 (n = 17,021), using public individual-level microdata and official population projections. We estimated age-specific incidence rate ratios (IRR) for each epidemic against a 2015–2019 baseline and a ratio of IRRs contrasting infants <2 months, protected only by maternal antibodies, with children aged 1–4 years. Negative binomial models tested excess risk in pandemic-era birth cohorts, mixed-effects logistic regression identified factors associated with infant death, and Bayesian spatial (BYM2) models estimated municipal relative risks.",
  Results = sprintf("In 2025, 1,207 cases (2.27 per 100,000; 15-fold the 2020–2024 mean) and 32 deaths were confirmed in 183 municipalities. Median age rose from 4 months in 2011–2013 to 33 months, and 37.5%% of cases were aged ≥10 years (vs 17.2%%). Incidence among infants <2 months did not exceed baseline (IRR %s), whereas it rose 6.2-fold in 2011–2013. The ratio of IRRs was %s (95%% CI %s–%s) in 2025 versus %s (%s–%s) in 2011–2013, robust to restriction to laboratory-confirmed cases. Infant pertussis mortality was 3.9 per 100,000 in 2025 versus 6.9 in 2011–2013. Indigenous infants accounted for 15 of 28 infant deaths in 2025 (case fatality 19.5%% vs 3.5%%); Indigenous ethnicity (aOR %s) and rural residence (aOR %s) were associated with death. Cohorts born in 2020–2022 showed a non-significant excess (IRR %s). Spatial models identified 41 high-risk municipalities, including Indigenous territories and the Medellín metropolitan area; higher departmental pentavalent coverage was associated with lower risk (RR per SD %s).",
                    ci(i2m25$irr, i2m25$lo, i2m25$hi), f2(r25$ror_2m_vs_1_4), f2(r25$lo), f2(r25$hi),
                    f2(r11$ror_2m_vs_1_4), f2(r11$lo), f2(r11$hi), ci(oInd$or, oInd$lo, oInd$hi),
                    ci(oRur$or, oRur$lo, oRur$hi), ci(cPan$irr, cPan$lo, cPan$hi), ci(pPen$rr, pPen$lo, pPen$hi)),
  Conclusions = "Colombia's post-pandemic pertussis epidemic shifted towards older children, adolescents and adults while young infants were relatively protected, consistent with a population-level effect of maternal Tdap. Excess mortality among Indigenous and rural infants calls for culturally adapted maternal immunization, timely primary vaccination and boosters targeted to underserved territories."
)
n_words <- sum(sapply(abs_en, wc))
keywords_en <- "pertussis; Bordetella pertussis; resurgence; maternal immunization; Tdap; public health surveillance; health inequities; Indigenous peoples; Bayesian spatial analysis; Colombia"

title_es <- "La resurgencia de tos ferina de 2025 en Colombia: un perfil de edad mayor, protección relativa de los lactantes pequeños tras la Tdap materna e inequidades persistentes en población indígena. Análisis nacional de microdatos de vigilancia, 2007–2025"
abs_es <- list(
  Antecedentes = "La tos ferina resurgió en el mundo después de la pandemia de COVID-19. En 2025 Colombia, que usa vacunas primarias de células enteras e introdujo la Tdap materna en 2013, tuvo su mayor epidemia en doce años. Comparamos esta epidemia con la de 2011–2013, anterior a la vacunación materna.",
  Métodos = "Estudio poblacional nacional de todos los casos confirmados de tos ferina notificados al SIVIGILA en 2007–2025 (n = 17.021), con microdatos individuales públicos y proyecciones oficiales de población del DANE. Estimamos razones de tasas de incidencia (RTI) por edad para cada epidemia frente a la línea base 2015–2019, y una razón de RTI que contrasta a los menores de 2 meses (protegidos solo por anticuerpos maternos) con los niños de 1–4 años. Modelos binomiales negativos evaluaron exceso de riesgo en cohortes nacidas en la pandemia; una regresión logística mixta identificó factores asociados con la muerte en lactantes, y modelos espaciales bayesianos (BYM2) estimaron riesgos relativos municipales.",
  Resultados = sprintf("En 2025 se confirmaron 1.207 casos (2,27 por 100.000; 15 veces la media de 2020–2024) y 32 muertes en 183 municipios. La mediana de edad pasó de 4 meses en 2011–2013 a 33 meses, y el 37,5 %% de los casos tenía ≥10 años (frente a 17,2 %%). La incidencia en menores de 2 meses no superó la línea base (RTI %s), mientras que en 2011–2013 se multiplicó por 6,2. La razón de RTI fue %s en 2025 frente a %s en 2011–2013, y se mantuvo al restringir a casos confirmados por laboratorio. La mortalidad infantil por tos ferina fue de 3,9 por 100.000 lactantes en 2025 frente a 6,9 en 2011–2013. Los lactantes indígenas aportaron 15 de las 28 muertes infantiles de 2025 (letalidad 19,5 %% frente a 3,5 %%); la etnia indígena (OR ajustado %s) y la residencia rural (OR ajustado %s) se asociaron con la muerte. Las cohortes nacidas en 2020–2022 mostraron un exceso no significativo (RTI %s). Los modelos espaciales identificaron 41 municipios de alto riesgo, incluidos territorios indígenas y el Valle de Aburrá; una mayor cobertura departamental de pentavalente se asoció con menor riesgo (RR por DE %s).",
                       sub("95% CI", "IC95%", ci(i2m25$irr, i2m25$lo, i2m25$hi)),
                       sprintf("%s (IC95%% %s–%s)", f2(r25$ror_2m_vs_1_4), f2(r25$lo), f2(r25$hi)),
                       sprintf("%s (%s–%s)", f2(r11$ror_2m_vs_1_4), f2(r11$lo), f2(r11$hi)),
                       sub("95% CI", "IC95%", ci(oInd$or, oInd$lo, oInd$hi)), sub("95% CI", "IC95%", ci(oRur$or, oRur$lo, oRur$hi)),
                       sub("95% CI", "IC95%", ci(cPan$irr, cPan$lo, cPan$hi)), sub("95% CI", "IC95%", ci(pPen$rr, pPen$lo, pPen$hi))),
  Conclusiones = "La epidemia pospandémica se desplazó hacia escolares, adolescentes y adultos, mientras los lactantes pequeños quedaron relativamente protegidos, lo que es coherente con un efecto poblacional de la Tdap materna. El exceso de mortalidad en lactantes indígenas y rurales exige vacunación materna culturalmente adaptada, vacunación primaria oportuna y refuerzos focalizados en territorios desatendidos."
)
abs_es$Resultados <- gsub("(\\d)\\.(\\d{2})(?=[ ;)–])", "\\1,\\2", abs_es$Resultados, perl = TRUE)

style_doc <- function(x) {
  x <- docx_set_paragraph_style(x, style_id = "Titre1", style_name = "heading 1", base_on = "Normal",
         fp_p = fp_par(padding.top = 14, padding.bottom = 4, keep_with_next = TRUE),
         fp_t = fp_text(font.family = FONT, font.size = 13, bold = TRUE, color = "#0b0b0b"))
  docx_set_paragraph_style(x, style_id = "Titre2", style_name = "heading 2", base_on = "Normal",
         fp_p = fp_par(padding.top = 10, padding.bottom = 3, keep_with_next = TRUE),
         fp_t = fp_text(font.family = FONT, font.size = 11.5, bold = TRUE, color = "#1c5cab"))
}
doc <- style_doc(read_docx())
doc <- body_add_fpar(doc, fpar(tx("Resumen para envío — Frontiers in Public Health", bold = TRUE, size = 9, color = "#52514e")))
doc <- body_add_fpar(doc, fpar(tx(sprintf("Research Topic: “Bordetella Pertussis in the 21st Century: Surveillance, Immunity, and Control of a Resurgent Pathogen” · Versión %s · %s", VERSION, format(Sys.Date(), "%d/%m/%Y")), size = 9, color = "#52514e")))
doc <- body_add_par(doc, "")
doc <- body_add_fpar(doc, fpar(tx(title_en, bold = TRUE, size = 14), fp_p = fp_par(padding.bottom = 8)))
aut_runs <- unlist(lapply(seq_along(AUTORES), function(i) list(
  tx(AUTORES[i], bold = (i == 1)),
  tx(paste0(SUP[AFIL_DE[i]], if (i %in% CORRESP) "*" else "", if (i < length(AUTORES)) ", " else "")))), recursive = FALSE)
doc <- do.call(add_p, c(list(doc), aut_runs, list(align = "left")))
for (k in seq_along(AFILIACIONES)) doc <- add_p(doc, tx(paste(SUP[k], AFILIACIONES[k]), size = 9, color = "#52514e"), align = "left")
doc <- add_p(doc, tx(paste0("* Correspondence: ", paste(sprintf("%s, %s", AUTORES[CORRESP], CORREOS[CORRESP]), collapse = "; ")), size = 9, color = "#52514e"), align = "left")
doc <- add_p(doc, tx("Article type: ", bold = TRUE), tx("Original Research · "), tx("Section: ", bold = TRUE), tx("Infectious Diseases: Epidemiology and Prevention"), align = "left")
doc <- add_h(doc, sprintf("Abstract (%d words; Frontiers limit 350)", n_words), 2)
for (k in names(abs_en)) doc <- add_p(doc, tx(paste0(k, ": "), bold = TRUE), tx(abs_en[[k]]))
doc <- add_p(doc, tx("Keywords: ", bold = TRUE), tx(keywords_en))
doc <- add_h(doc, "Short summary for the submission form (≈120 words)", 2)
short_en <- "Using 19 years of nationwide individual-level surveillance microdata (2007–2025; 17,021 confirmed cases), we characterise Colombia's 2025 pertussis resurgence, the largest since 2013, and compare it with the 2011–2013 epidemic that preceded maternal Tdap immunization. The 2025 epidemic shifted markedly towards older children, adolescents and adults, while infants younger than 2 months were relatively protected (ratio of incidence rate ratios 0.34 versus 1.03), consistent with a population-level effect of maternal immunization. Deaths concentrated among Indigenous and rural infants, and Bayesian spatial models located high-risk municipalities in Indigenous territories and in the Medellín metropolitan area. The findings inform maternal, infant and booster vaccination strategies and equity-oriented surveillance in Latin America."
doc <- add_p(doc, tx(short_en))
doc <- body_add_break(doc)
doc <- add_h(doc, "Versión en español (para revisión del equipo; no se envía)", 1)
doc <- body_add_fpar(doc, fpar(tx(title_es, bold = TRUE, size = 12.5), fp_p = fp_par(padding.bottom = 8)))
for (k in names(abs_es)) doc <- add_p(doc, tx(paste0(k, ": "), bold = TRUE), tx(abs_es[[k]]))
doc <- add_p(doc, tx("Palabras clave: ", bold = TRUE), tx("tos ferina; Bordetella pertussis; resurgencia; vacunación materna; Tdap; vigilancia en salud pública; inequidades en salud; pueblos indígenas; análisis espacial bayesiano; Colombia"))
doc <- add_h(doc, "Notas para el envío", 2)
doc <- add_bullets(doc, c(
  "Fecha límite de manuscrito del Research Topic: 29 de enero de 2027. El resumen puede enviarse desde ya en la página del tema (botón “Submit your abstract”).",
  "Autores, afiliaciones, correos de correspondencia y ORCID confirmados el 3/11/2026 (ver la tabla siguiente). Los cuatro ORCID se verificaron: dígito de control válido y nombre coincidente en el registro público.",
  "Pedro Velez-Pardo figura como segundo autor de correspondencia de forma tentativa. Si la Universidad CES tiene convenio de pago de APC con Frontiers, este suele aplicarse según la afiliación del autor de correspondencia: confirmarlo con la biblioteca.",
  "Datos de autores completos: afiliación, correo y ORCID de los cuatro (ver la tabla siguiente).",
  "El registro ORCID de Alejandra figura como “Alejandra Segura”; conviene usar la misma forma del nombre en todas las publicaciones.",
  "Las cifras provienen de los scripts 02_scripts/03–05 (microdatos SIVIGILA descargados el 3/11/2026); si se actualizan los datos, se regeneran automáticamente con 07_documentos.R.",
  "Las cifras son preliminares: pueden cambiar levemente al refinar los modelos para el manuscrito completo."))
doc <- add_h(doc, "Datos para el formulario de envío", 2)
rol <- c("Primera autoría · correspondencia", "Coautoría", "Coautoría", "Coautoría · correspondencia (tentativa)")
tab_aut <- data.table(Autor = AUTORES, `Afiliación` = AFILIACIONES[AFIL_DE],
                      Correo = fifelse(is.na(CORREOS), "pendiente", CORREOS), ORCID = ORCID, Rol = rol)
doc <- body_add_flextable(doc, ft_default(flextable(tab_aut)) |> fontsize(size = 8, part = "all") |>
                            width(j = 1, width = 1.15) |> width(j = 2, width = 1.45) |> width(j = 3, width = 1.35) |>
                            width(j = 4, width = 1.5) |> width(j = 5, width = 0.75))
print(doc, target = sprintf("Resumen_Frontiers_tosferina_%s.docx", VERSION))
cat("Resumen: ", n_words, "palabras (inglés)\n")

# =============================================================================
# 2. PROPUESTA DE IDEAS Y AVANCES
# =============================================================================
t1 <- fread("03_resultados/tabla1_caracteristicas_periodo.csv", encoding = "UTF-8")
fig <- function(doc, f, cap, w = 6.2, h = NULL) {   # ancho útil A4 con márgenes de 2,5 cm: 6,3 in
  png <- file.path("04_figuras", paste0(f, "_preview.png"))
  dims <- dim(png::readPNG(png)); if (is.null(h)) h <- w * dims[1] / dims[2]
  doc <- body_add_img(doc, png, width = w, height = h)
  add_p(doc, tx(cap, italic = TRUE, size = 9, color = "#52514e"), align = "left")
}
ideas <- data.table(
  `#` = c("A", "B", "C", "D", "E", "F"),
  Idea = c("Epidemia 2025 vs 2011–2013: desplazamiento de edad, firma poblacional de la Tdap materna, letalidad e inequidades, y análisis espacial bayesiano (RECOMENDADA)",
           "Brecha de inmunidad pospandémica por cohortes de nacimiento (análisis edad-periodo-cohorte, superficie de Lexis)",
           "Subestudio Antioquia: coberturas municipales de DPT (1980–2025) y riesgo de tos ferina por municipio",
           "Contexto regional/global: resurgencia pospandémica (OMS/OPS) y coberturas WUENIC",
           "Genómica: antígenos vacunales y resistencia a macrólidos en genomas públicos de B. pertussis de América Latina",
           "Salud mental: duelo parental, percepción de riesgo y vacilación vacunal en gestantes"),
  `Datos públicos` = c("SIVIGILA evento 800 (2007–2025), DANE (proyecciones, IPM, etnia), MGN 2018, MinSalud",
                       "Los mismos (edad simple y fecha de nacimiento)",
                       "datos.gov.co (Gobernación de Antioquia) + SIVIGILA",
                       "UNICEF/OMS WUENIC; casos OMS (API no disponible hoy)",
                       "NCBI/SRA; casi sin genomas colombianos",
                       "No hay datos públicos que vinculen tos ferina y salud mental"),
  Novedad = c("Alta: no hay análisis publicado de 2025 con microdatos nacionales", "Alta en América Latina", "Media-alta, local", "Baja-media (Zhang 2026 ya hizo análisis global)", "Media (Perú publicó MRBP 2025)", "Alta, pero requiere datos primarios"),
  Factibilidad = c("Muy alta (datos ya descargados y analizados)", "Alta (incluida como objetivo secundario)", "Alta", "Media", "Baja a corto plazo", "Baja para esta convocatoria"))

d <- style_doc(read_docx())
d <- body_add_fpar(d, fpar(tx(sprintf("Propuesta de artículo y avances — Tos ferina · Versión %s · %s", VERSION, format(Sys.Date(), "%d/%m/%Y")), size = 9, color = "#52514e")))
d <- body_add_fpar(d, fpar(tx("Artículo para Frontiers in Public Health: ideas, verificación de novedad, datos y resultados preliminares", bold = TRUE, size = 15), fp_p = fp_par(padding.bottom = 6)))
d <- add_p(d, tx("Autores: ", bold = TRUE), tx("Doris Cardona (investigadora independiente, Medellín; autora de correspondencia), Angela Segura Cardona, Alejandra Segura Cardona y Pedro Velez-Pardo (Universidad CES, Medellín; Pedro Velez-Pardo como autor de correspondencia tentativo)."), align = "left")
d <- add_h(d, "1. Recomendación", 1)
d <- add_p(d, tx("Proponemos un artículo original que analice la epidemia de tos ferina de 2025 en Colombia con los "), tx("microdatos nacionales del SIVIGILA 2007–2025 (17.021 casos confirmados)", bold = TRUE), tx(", y que la compare con la epidemia de 2011–2013, anterior a la vacunación materna con Tdap (introducida en 2013). Es la continuación natural del artículo de Doris Cardona en Biomédica (2016) sobre 2002–2012, ahora con métodos analíticos de mayor alcance: razones de tasas por edad, análisis por cohortes de nacimiento (superficie de Lexis), regresión logística mixta para la letalidad infantil y modelos espaciales bayesianos (BYM2) a nivel municipal."))
d <- add_p(d, tx("Tres mensajes centrales, ya respaldados por los datos: "), tx("(1)", bold = TRUE), tx(" la epidemia de 2025 se desplazó hacia escolares, adolescentes y adultos; "), tx("(2)", bold = TRUE), tx(" los menores de 2 meses quedaron relativamente protegidos, a diferencia de 2011–2013, lo que es coherente con un efecto poblacional de la Tdap materna; "), tx("(3)", bold = TRUE), tx(" las muertes se concentraron en lactantes indígenas y rurales (OR ajustado 5,0 para etnia indígena), con focos en territorios emberá y en el Valle de Aburrá."))
d <- add_h(d, "2. La convocatoria", 1)
d <- add_bullets(d, c(
  "Research Topic: “Bordetella Pertussis in the 21st Century: Surveillance, Immunity, and Control of a Resurgent Pathogen” (Frontiers in Public Health; también Frontiers in Epidemiology y Frontiers in Pediatrics).",
  "Editores: Cynthia Kenyon (Minnesota Department of Health, EE. UU.) y Benard Kulohoma (Ortholog, Kenia).",
  "Fecha límite de manuscritos: 29 de enero de 2027. Tipo sugerido: Original Research, sección Infectious Diseases: Epidemiology and Prevention.",
  "Encaja en 5 de los 10 ejes del tema: carga y resurgencia; vigilancia; vacunación materna; grupos de alto riesgo (lactantes, población indígena); determinantes sociales y estructurales."))
d <- add_h(d, "3. Verificación de novedad (3 de noviembre de 2026)", 1)
d <- add_p(d, tx("Se buscó en PubMed, Europe PMC (incluye preprints de medRxiv y Research Square), Scopus (sesión institucional CES) y LILACS/BVS. "), tx("No encontramos ningún análisis publicado de la epidemia colombiana de 2025 con microdatos nacionales.", bold = TRUE), tx(" Solo hay boletines del INS (literatura gris)."))
d <- add_p(d, tx("Antecedentes más cercanos y en qué se diferencia nuestra propuesta:", bold = TRUE), align = "left")
d <- add_bullets(d, c(
  "Cardona D et al. Biomédica 2016 (PMID 27869387): tendencias 2002–2012, descriptivo. El nuevo artículo lo actualiza hasta 2025 con métodos analíticos.",
  "Carrasquilla G et al. Vaccine 2020 (Bogotá) e Hincapié-Palacio D et al. Vaccine 2018 (Medellín): efecto de la Tdap materna en una ciudad y antes de la pandemia.",
  "Hoyos MC et al. J Public Health Res 2021: series de tiempo nacionales hasta 2018, sin edad fina, sin análisis espacial ni pospandemia.",
  "Idrovo ÁJ et al. Biomédica 2026: historia de la tos ferina en Colombia (revisión histórica).",
  "Sansone NMS et al. Front Public Health 2025: resurgencia de 2024 en Brasil, descriptivo (mismo journal; nuestro aporte analítico es mayor).",
  "Madden EF et al. Emerg Infect Dis 2026: diferencias de edad entre epidemias en Australia (país con vacuna acelular); no hay equivalente en América Latina.",
  "Zhang S et al. Hum Vaccin Immunother 2026: brecha de inmunidad global con datos GBD, sin microdatos de país.",
  "Juscamayta-López E et al. Emerg Infect Dis 2026: B. pertussis resistente a macrólidos en Perú, 2025 (argumento para vigilancia genómica en Colombia)."))
d <- add_h(d, "4. Ideas evaluadas", 1)
d <- body_add_flextable(d, ft_default(flextable(ideas)) |> width(j = 1, width = 0.25) |> width(j = 2, width = 2.15) |> width(j = 3:5, width = 1.25))
d <- add_p(d, tx("Sobre salud mental: revisamos la posibilidad de un vínculo con datos públicos. El SIVIGILA marca la “población psiquiátrica”, pero en tos ferina es prácticamente nula, y no hay bases públicas sobre duelo parental o salud mental materna tras una muerte infantil por tos ferina. Recomendamos dejarlo como un proyecto aparte con datos primarios (por ejemplo, percepción de riesgo y vacilación vacunal en gestantes), sin forzarlo en este artículo.", size = 10))
d <- add_h(d, "5. Datos (todos públicos)", 1)
d <- add_bullets(d, c(
  "SIVIGILA, INS — microdatos del evento 800 (tos ferina), 2007–2025, solo casos confirmados (laboratorio, clínica o nexo epidemiológico). Descarga mediante la API pública del portal SIVIGILA que usa el paquete oficial sivirep (Epiverse-TRACE).",
  "DANE — proyecciones y retroproyecciones de población por edad simple, sexo y área (nacional, departamental y municipal; versión posCOVID, CNPV 2018).",
  "DANE — índice de pobreza multidimensional municipal (censo 2018) y población por pertenencia étnico-racial por municipio (2018–2035).",
  "DANE — Marco Geoestadístico Nacional (MGN 2018), límites municipales.",
  "MinSalud — coberturas administrativas de pentavalente (3.ª dosis) por departamento (datos.gov.co); Gobernación de Antioquia — coberturas municipales 1980–2025."))
d <- add_h(d, "6. Métodos (resumen)", 1)
d <- add_bullets(d, c(
  "Incidencia anual y por grupos de edad finos (lactantes por meses); comparación de cada epidemia contra la línea base 2015–2019 mediante razones de tasas (RTI) con IC95%.",
  "Firma de la vacunación materna: razón de RTI de menores de 2 meses (protegidos solo por anticuerpos maternos) frente a niños de 1–4 años. Sensibilidad: solo casos confirmados por laboratorio.",
  "Cohortes de nacimiento: superficie de Lexis (edad simple × año) y modelo binomial negativo con curva suave de edad para probar exceso en cohortes 2019–2022.",
  "Letalidad infantil: regresión logística mixta (intercepto aleatorio por departamento): edad, periodo, etnia, régimen de salud, ruralidad, sexo y tipo de confirmación.",
  "Espacial: modelo bayesiano BYM2 (R-INLA) con casos esperados estandarizados por edad; covariables: IPM, ruralidad, población indígena y afro, densidad y cobertura de pentavalente."))
d <- add_h(d, "7. Resultados preliminares", 1)
d <- add_p(d, tx("Tabla 1. Características de los casos confirmados por periodo, Colombia 2007–2025.", bold = TRUE, size = 9.5), align = "left")
d <- body_add_flextable(d, ft_default(flextable(t1)) |> fontsize(size = 7.5, part = "all") |> width(j = 1, width = 1.7) |> width(j = 2:6, width = 0.9))
d <- body_add_par(d, "")
d <- fig(d, "Figura1_dinamica_2007_2025", "Figura 1. Casos semanales por inicio de síntomas (A) e incidencia anual por grupo de edad (B). Los picos de 2012–2013 corresponden a brotes investigados en Antioquia (Medellín y La Estrella).")
d <- fig(d, "Figura2_edad_vacunacion_materna", "Figura 2. Distribución de edad por año (A) y RTI por edad de cada epidemia frente a 2015–2019 (B). En 2025 los menores de 2 meses no superaron la línea base; en 2011–2013 se multiplicaron por 6,2.")
d <- fig(d, "Figura3_superficie_Lexis", "Figura 3. Superficie de Lexis de la incidencia (edad simple 0–19 × año). Las diagonales delimitan las cohortes nacidas en 2019–2021.")
d <- fig(d, "Figura4_mapas_BYM2", "Figura 4. Riesgo relativo municipal suavizado (BYM2) en las dos epidemias. Contorno negro: probabilidad posterior de RR > 1 mayor a 0,95.")
d <- fig(d, "Figura5_muerte_lactantes", "Figura 5. Factores asociados con la muerte en lactantes con tos ferina confirmada, 2011–2025 (OR ajustados).")
d <- fig(d, "FiguraS1_sensibilidad_laboratorio", "Figura S1. Sensibilidad: RTI por edad solo con casos confirmados por laboratorio.", w = 4.2)
d <- add_h(d, "8. Limitaciones a declarar y cómo abordarlas", 1)
d <- add_bullets(d, c(
  "Los microdatos públicos solo traen casos confirmados (no los ~7.000 probables notificados en 2025). Se reportará la cifra de notificación del INS como contexto.",
  "Cambios en la confirmación (45 % por laboratorio en 2011–2013 frente a 92 % en 2025) y mayor uso de PCR en mayores: se controlan con el análisis restringido a laboratorio y la razón de RTI, que compara grupos de edad dentro de cada epidemia.",
  "La letalidad depende del denominador de casos confirmados; por eso se reporta también la mortalidad poblacional (por 100.000 lactantes).",
  "No hay estado vacunal individual en los microdatos públicos. Se puede solicitar al INS la ficha complementaria (dosis de pentavalente y Tdap de la madre) para un estudio caso-control o de método de cribado (screening method).",
  "La cobertura de pentavalente es departamental (2019–2022); para el manuscrito se buscarán coberturas municipales 2019–2025 y de Tdap en gestantes."))
d <- add_h(d, "9. Próximos pasos y cronograma", 1)
d <- add_bullets(d, c(
  "Noviembre 2026: enviar el resumen (autores confirmados); confirmar ORCID y correos; carta de exención o aval del comité de ética de la Universidad CES (datos públicos y anonimizados, investigación sin riesgo según la Resolución 8430 de 1993).",
  "Noviembre–diciembre 2026: refinar modelos (APC completo, sensibilidad de la línea base, coberturas municipales y Tdap materna); solicitar al INS las variables de vacunación.",
  "Diciembre 2026: primer borrador del manuscrito (inglés, formato Frontiers) con figuras finales.",
  "Enero 2027: revisión interna, repositorio de código y datos procesados (GitHub/Zenodo), y envío antes del 29 de enero."))
d <- add_h(d, "10. Archivos del proyecto", 1)
d <- add_bullets(d, c(
  "02_scripts/: 01–07, scripts reproducibles en R (descarga, limpieza, descriptivo, modelos, figuras, documentos).",
  "03_resultados/: tablas en CSV y Tablas_descriptivas.xlsx; registros de cada ejecución.",
  "04_figuras/: cada figura en PDF y SVG (vectoriales), EPS, TIFF a 600 dpi y PNG de vista previa.",
  "versiones anteriores/: aquí se archivan las versiones previas de los documentos."))
print(d, target = sprintf("Propuesta_y_avances_tosferina_%s.docx", VERSION))
cat("Documentos generados.\n")
