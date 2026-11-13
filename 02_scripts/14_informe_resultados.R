# =============================================================================
# 14_informe_resultados.R
# Informe de resultados para el equipo (Word): plan de análisis estadístico,
# tablas generadas desde 03_resultados/, figuras de 04_figuras/, cambios frente
# al resumen enviado, limitaciones y próximos pasos.
# Ejecutar desde la raíz: Rscript 02_scripts/14_informe_resultados.R
# =============================================================================
suppressMessages({ library(officer); library(flextable); library(data.table) })
VERSION <- "v1"
FONT <- "Arial"
AUTORES <- "Doris Cardona, Angela Segura Cardona, Alejandra Segura Cardona, Pedro Velez-Pardo"
R <- function(f) fread(file.path("03_resultados", f), encoding = "UTF-8")
f2 <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")   # coma decimal (español)
ci <- function(e, l, h, d = 2) sprintf("%s (%s–%s)", f2(e, d), f2(l, d), f2(h, d))
es <- function(s) s                                             # el texto ya se construye con coma decimal
NBSP <- "\u00a0"
num_es <- function(x) ifelse(abs(x) < 10000, as.character(x), formatC(x, format = "d", big.mark = NBSP))   # norma RAE: espacio desde 5 cifras
es_cell <- function(x) gsub("(\\d)\\.(\\d)", "\\1,\\2", gsub("100\\.000", paste0("100", NBSP, "000"), x))

tx <- function(s, bold = FALSE, italic = FALSE, size = 10.5, color = "#0b0b0b")
  ftext(s, fp_text(font.family = FONT, font.size = size, bold = bold, italic = italic, color = color))
add_p <- function(doc, ..., align = "justify") body_add_fpar(doc, fpar(..., fp_p = fp_par(text.align = align, padding.bottom = 6, line_spacing = 1.15)))
add_b <- function(doc, items, size = 10.5) { for (it in items) doc <- body_add_fpar(doc, fpar(tx("•  ", size = size), tx(it, size = size),
                                              fp_p = fp_par(text.align = "left", padding.left = 14, padding.bottom = 3, line_spacing = 1.1))); doc }
add_h <- function(doc, s, l = 1) body_add_par(doc, s, style = paste("heading", l))
cap <- function(doc, s) add_p(doc, tx(s, italic = TRUE, size = 9, color = "#52514e"), align = "left")
tab <- function(doc, dt, titulo, widths = NULL, size = 8) {
  dt <- copy(dt); for (j in names(dt)) if (is.character(dt[[j]])) set(dt, j = j, value = es_cell(dt[[j]]))
  setnames(dt, es_cell(names(dt)))
  doc <- add_p(doc, tx(titulo, bold = TRUE, size = 9.5), align = "left")
  ft <- flextable(dt) |> font(fontname = FONT, part = "all") |> fontsize(size = size, part = "all") |>
    bold(part = "header") |> bg(bg = "#eef3fb", part = "header") |> border_remove() |>
    hline_top(border = fp_border(color = "#52514e", width = 1), part = "header") |>
    hline_bottom(border = fp_border(color = "#52514e", width = 1), part = "body") |>
    hline_bottom(border = fp_border(color = "#c3c2b7", width = 0.5), part = "header") |>
    padding(padding.top = 1.5, padding.bottom = 1.5, part = "all")
  if (!is.null(widths)) for (j in seq_along(widths)) ft <- width(ft, j = j, width = widths[j]) else ft <- autofit(ft)
  body_add_par(body_add_flextable(doc, ft), "")
}
fig <- function(doc, f, caption, w = 6.2) {
  png <- file.path("04_figuras", paste0(f, "_preview.png")); dm <- dim(png::readPNG(png))
  cap(body_add_img(doc, png, width = w, height = w * dm[1] / dm[2]), caption)
}

# --------------------------------------------------------------------------------
# Datos de resultados
# --------------------------------------------------------------------------------
an  <- R("tabla_serie_anual.csv"); t1 <- R("tabla1_caracteristicas_periodo.csv")
de  <- R("series_deficit_exceso.csv")[especificacion == "principal"]
irr <- R("edad_irr_nb.csv"); ror <- R("edad_ror_nb_efectos_fijos_anio.csv"); cf <- R("edad_contrafactual_evitados.csv")
se  <- R("edad_sensibilidad.csv"); coh <- R("cohortes_pandemicas_sensibilidad.csv"); apc <- R("apc_contraste_pandemia.csv")
orb <- R("letalidad_modelos_OR.csv"); let <- R("letalidad_cruda_etnia_periodo.csv"); met <- R("mortalidad_lactantes_etnia_aproximada.csv")
fe  <- R("espacial_bym2_efectos_fijos.csv"); fc <- R("espacial_cambio_efectos_fijos.csv"); mor <- R("espacial_moran.csv")
kd  <- R("espacial_kulldorff_2025.csv"); kd20 <- R("espacial_kulldorff_2025_ventana20.csv"); iq <- R("inequidades_ipm_quintiles.csv")
dup <- R("calidad_duplicados_probables.csv"); fal <- R("calidad_faltantes.csv"); tim <- R("cronologia_2025_mediana_inicio.csv")
dmed <- R("cronologia_2025_diferencia_medianas.csv")
g <- function(tb, gr, ct) tb[grupo == gr & contraste == ct]
r6 <- g(ror, "<6 wk", "RoRR 2025/2011-2013"); r2 <- g(ror, "<2 mo", "RoRR 2025/2011-2013")
o6 <- g(ror, "<6 wk", "RoR 2025"); o2 <- g(ror, "<2 mo", "RoR 2025")
ob <- orb[modelo == "Mixto principal"]; obA <- orb[grepl("prenatales", modelo)]
d_all <- de[banda == "All ages"]; d_inf <- de[banda == "<1 year"]; d_10 <- de[banda == "10+ years"]
pick <- function(d, p) d[periodo == p]
m25 <- met[periodo == "2025"]; m11 <- met[periodo == "2011-2013"]; mtot <- met[periodo == "2011-2025"]
an25 <- an[anio == 2025]

# --------------------------------------------------------------------------------
doc <- read_docx()
doc <- docx_set_paragraph_style(doc, style_id = "Titre1", style_name = "heading 1", base_on = "Normal",
         fp_p = fp_par(padding.top = 14, padding.bottom = 4, keep_with_next = TRUE),
         fp_t = fp_text(font.family = FONT, font.size = 13, bold = TRUE, color = "#0b0b0b"))
doc <- docx_set_paragraph_style(doc, style_id = "Titre2", style_name = "heading 2", base_on = "Normal",
         fp_p = fp_par(padding.top = 10, padding.bottom = 3, keep_with_next = TRUE),
         fp_t = fp_text(font.family = FONT, font.size = 11.5, bold = TRUE, color = "#1c5cab"))
doc <- body_add_fpar(doc, fpar(tx(sprintf("Informe de resultados · Versión %s · %s", VERSION, format(Sys.Date(), "%d/%m/%Y")), size = 9, color = "#52514e")))
doc <- body_add_fpar(doc, fpar(tx("Tos ferina en Colombia, 2007–2025: análisis completos para el manuscrito de Frontiers in Public Health", bold = TRUE, size = 15), fp_p = fp_par(padding.bottom = 6)))
doc <- add_p(doc, tx("Autores: ", bold = TRUE), tx(AUTORES), align = "left")

# ===== Resumen ejecutivo ==========================================================
doc <- add_h(doc, "Resumen ejecutivo", 1)
doc <- add_b(doc, es(c(
  sprintf("Magnitud: en 2025 se confirmaron %s casos (%s por 100\u00a0000) y %d muertes. Frente a lo esperado según 2015–2019, en 2020–2024 hubo un déficit acumulado de casos (razón observada/esperada %s) y en 2025 un exceso (%s).",
          num_es(an25$casos), f2(an25$inc_100k), an25$muertes,
          pick(d_all, "Déficit 2020-2024")[, ci(razon_obs_esp, razon_lo, razon_hi)], pick(d_all, "Exceso 2025")[, ci(razon_obs_esp, razon_lo, razon_hi)]),
  sprintf("El rebote fue selectivo por edad: en 10+ años el exceso de 2025 (O/E %s) superó al doble el déficit pandémico; en lactantes no superó la línea base (O/E %s).",
          pick(d_10, "Exceso 2025")[, ci(razon_obs_esp, razon_lo, razon_hi)], pick(d_inf, "Exceso 2025")[, ci(razon_obs_esp, razon_lo, razon_hi)]),
  sprintf("Firma de la Tdap materna: frente a la epidemia previa a la vacunación materna (2011–2013), el riesgo relativo de los menores de 6 semanas (protegidos solo por anticuerpos maternos) fue 67%% menor en 2025 (razón de razones %s; p = %s); en menores de 2 meses, %s. La protección se desvanece con la edad (2–3 meses: %s; 6–11 meses: %s).",
          ci(r6$est, r6$lo, r6$hi), formatC(r6$p, format = "f", digits = 3), ci(r2$est, r2$lo, r2$hi),
          g(ror, "2–3 mo", "RoRR 2025/2011-2013")[, ci(est, lo, hi)], g(ror, "6–11 mo", "RoRR 2025/2011-2013")[, ci(est, lo, hi)]),
  sprintf("Contrafactual: con el patrón de edad de 2011–2013 habrían ocurrido %s casos adicionales en menores de 2 meses en 2025 y entre %s y %s muertes adicionales (según la letalidad usada).",
          cf[grupo == "<2 mo", casos_evitados], cf[grupo == "<2 mo", muertes_evitadas_letalidad2011_13], cf[grupo == "<2 mo", muertes_evitadas_letalidad2025]),
  sprintf("Mecanismo (control negativo ecológico): la cobertura municipal de control prenatal (≥4 controles), vía por la que se aplica la Tdap materna, se asoció con menor incidencia en lactantes en 2025 (RR por DE %s) pero no en 2011–2013, antes de la Tdap (%s), ni en mayores de 10 años en 2025 (%s).",
          fe[modelo == "2025 <1 año" & termino == "z_cpn4_1924", ci(rr, lo, hi)], fe[modelo == "2011-2013 <1 año" & termino == "z_cpn4_0810", ci(rr, lo, hi)],
          fe[modelo == "2025 10+ años (control negativo)" & termino == "z_cpn4_1924", ci(rr, lo, hi)]),
  sprintf("Inequidad: la etnia indígena (OR ajustado %s), la residencia rural (%s) y la condición de migrante (%s) se asociaron con la muerte de lactantes. La brecha poblacional de mortalidad infantil indígena/no indígena pasó de %s en 2011–2013 a %s en 2025 (estimación aproximada).",
          ob[termino == "etnia3Indígena", ci(or, lo, hi)], ob[termino == "rural", ci(or, lo, hi)], ob[termino == "migrante", ci(or, lo, hi)],
          ci(m11$rr, m11$rr_lo, m11$rr_hi, 1), ci(m25$rr, m25$rr_lo, m25$rr_hi, 1)),
  sprintf("Gradiente social invertido: el índice relativo de desigualdad de la mortalidad infantil según pobreza municipal pasó de %s (2011–2013) a %s (2025): las muertes se concentraron en los municipios más pobres.",
          iq[periodo == "2011-2013", RII_mortalidad_infantil][1], iq[periodo == "2025", RII_mortalidad_infantil][1]),
  "Cohortes nacidas en la pandemia: sin exceso robusto de riesgo (depende de la especificación); la epidemia se explica mejor por un cambio de periodo que afectó a escolares, adolescentes y adultos que por dosis perdidas en esas cohortes.")))

# ===== 1. Plan de análisis estadístico ===========================================
doc <- add_h(doc, "1. Plan de análisis estadístico (SAP)", 1)
doc <- add_h(doc, "1.1 Diseño y fuentes", 2)
doc <- add_b(doc, c(
  "Estudio ecológico y de vigilancia, nacional, con microdatos individuales de todos los casos confirmados de tos ferina notificados al SIVIGILA (evento 800), 2007–2025 (n = 17\u00a0021; INS, descarga del 3/11/2026).",
  "Denominadores: proyecciones y retroproyecciones del DANE por edad simple, sexo, área y municipio (versión posCOVID, CNPV 2018). Lactantes por subgrupos de edad: población de 0 años x fracción del año.",
  "Covariables municipales: IPM censal 2018 (DANE); proporción rural; población indígena y afrodescendiente (proyección étnica DANE 2025); densidad (MGN 2018); cobertura de pentavalente y % de nacidos vivos con ≥4 controles prenatales (TerriData-DNP, fuente MinSalud).",
  "Contexto: coberturas nacionales WUENIC (OMS/UNICEF, revisión 2025)."))
doc <- add_h(doc, "1.2 Definiciones", 2)
doc <- add_b(doc, c(
  "Periodos: línea base 2015–2019 (posterior a la Tdap materna, prepandemia); epidemia previa a la Tdap materna 2011–2013; pandemia/pospandemia 2020–2024; epidemia 2025.",
  "Edad al inicio de síntomas calculada con la fecha de nacimiento (89% de los registros) o con la edad notificada. Grupos finos en lactantes: <6 semanas (anterior a la primera dosis, que desde marzo de 2025 se aplica a las 6 semanas), 6–8 semanas, 2–3, 4–5 y 6–11 meses.",
  "Desenlaces: caso confirmado; muerte (condición final); hospitalización (sin dato válido en 2008–2009)."))
doc <- add_h(doc, "1.3 Modelos", 2)
doc <- add_b(doc, c(
  "A. Series de tiempo: binomial negativa mensual con términos de Fourier (2 armónicos) y offset poblacional, ajustada en 2015–2019; casos esperados 2020–2025 con intervalos de predicción por simulación (5000 réplicas: incertidumbre de parámetros + muestreo NB). Sensibilidad: línea base 2014–2019 con tendencia.",
  "B. Estructura de edad: conteos anuales por grupo de edad x año. Modelo 1 (RTI por edad vs línea base, binomial negativa por celda). Modelo 2 (razones de RTI dentro del mismo año, binomial negativa con efectos fijos de año): log μ[a,y] = γ[y] + α[a] + δ[a,E1]·1(2011–2013) + δ[a,E2]·1(2025) + log pob; δ = log razón de RTI frente a 1–4 años; razón de razones = exp(δ[a,E2] − δ[a,E1]).",
  "C. Contrafactual: casos esperados en 2025 con el patrón de edad de 2011–2013 = observados / razón de razones; muertes = casos adicionales x letalidad (Beta-Jeffreys; 10\u00a0000 simulaciones).",
  "D. Cohortes: (i) binomial negativa de 2025 frente a la línea base con curva suave de edad (splines, 3–5 gl) e indicador de cohortes pandémicas; (ii) modelo edad-periodo-cohorte bayesiano (INLA, RW2 + iid), con curvatura de cohorte sin tendencia lineal.",
  "E. Letalidad infantil: regresión logística mixta (intercepto aleatorio por departamento); sensibilidad con regresión de Firth; contexto municipal de control prenatal; interacción periodo x etnia; mortalidad poblacional por etnia (denominador indígena aproximado = Σ población de 0 años x % indígena municipal).",
  "F. Espacial: BYM2 (INLA, priors PC) con casos esperados estandarizados por edad y covariables estandarizadas; modelos por epidemia y edad, con mayores de 10 años como control negativo; modelo de cambio 2025 frente a la línea base propia de cada municipio (dos componentes BYM2); I de Moran con ajuste bayesiano empírico; escaneo de Kulldorff (ventanas 10% y 20% de la población, 999 réplicas).",
  "G. Desigualdades: quintiles de municipios ponderados por población según IPM; índice relativo de desigualdad (RII) por regresión binomial negativa/cuasi-Poisson sobre el ridit."))
doc <- add_h(doc, "1.4 Sensibilidad y calidad de datos", 2)
doc <- add_b(doc, es(c(
  "Sensibilidad del hallazgo principal: solo casos confirmados por laboratorio; sin duplicados probables; año de inicio de síntomas; líneas base 2014–2019 y 2017–2019; sin Antioquia; cuasi-Poisson; grupo de referencia 6–11 meses.",
  sprintf("Duplicados probables (mismo nacimiento, sexo, municipio y misma fecha de inicio o consulta): %d registros (0,8%%), excluidos solo en sensibilidad.", dup$registros_excluidos_en_sensibilidad),
  sprintf("Datos faltantes: fecha de nacimiento %s%%, municipio %s%%, edad %s%%.", f2(fal[variable == "fecha_nto", porcentaje], 1), f2(fal[variable == "mpio_res", porcentaje], 1), f2(fal[variable == "edad_dias", porcentaje], 1)),
  "La cobertura municipal de pentavalente de TerriData presenta un quiebre desde 2021 (mediana ≈ 50% frente a 87–92% nacional según WUENIC); por eso solo se usó el promedio 2015–2019 (Figura S2).",
  "Software: R 4.6.1 (MASS, lme4, logistf, INLA 26.8.7, spdep, SpatialEpi, sf); scripts reproducibles 01–14 en 02_scripts/. Cálculo con 4 hilos.")))

# ===== 2. Resultados ===============================================================
doc <- add_h(doc, "2. Resultados", 1)
doc <- add_h(doc, "2.1 Características y dinámica", 2)
doc <- tab(doc, t1, "Tabla 1. Características de los casos confirmados por periodo, Colombia 2007–2025", widths = c(1.7, rep(0.9, 5)), size = 7.5)
td <- de[periodo != "Balance 2020-2025", .(Grupo = banda, Periodo = periodo, Observados = observados,
                                         `Esperados (IP95%)` = sprintf("%.0f (%.0f–%.0f)", esperados, esp_lo, esp_hi),
                                         `O/E (IP95%)` = ci(razon_obs_esp, razon_lo, razon_hi),
                                         `Recuperación del déficit` = sprintf("%.0f%%", 100 * recuperacion_mediana))]
td[, Grupo := factor(Grupo, levels = c("All ages", "<1 year", "1–9 years", "10+ years"), labels = c("Todas las edades", "<1 año", "1–9 años", "10+ años"))]
setorder(td, Grupo)
doc <- tab(doc, td, "Tabla 2. Casos observados frente a esperados según la línea base 2015–2019", widths = c(1.1, 1.15, 0.75, 1.15, 1.15, 0.9))
doc <- fig(doc, "Figura1_dinamica_deuda", "Figura 1. Curva epidémica semanal (A) y casos mensuales observados frente a esperados por edad (B).")

doc <- add_h(doc, "2.2 Desplazamiento de edad y firma de la Tdap materna", 2)
lab_g <- c(`<6 wk` = "<6 semanas", `6–8 wk` = "6–8 semanas", `<2 mo` = "<2 meses", `2–3 mo` = "2–3 meses", `4–5 mo` = "4–5 meses",
           `6–11 mo` = "6–11 meses", `1–4 y` = "1–4 años", `5–9 y` = "5–9 años", `10–14 y` = "10–14 años", `15–19 y` = "15–19 años",
           `20–39 y` = "20–39 años", `40–59 y` = "40–59 años", `60+ y` = "60+ años")
ti <- dcast(irr[, .(grupo, epidemia, v = ci(irr, lo, hi))], grupo ~ epidemia, value.var = "v")
tr <- dcast(ror[, .(grupo, contraste, v = ci(est, lo, hi))], grupo ~ contraste, value.var = "v")
t3 <- merge(ti, tr, by = "grupo", all = TRUE)
t3 <- t3[match(names(lab_g), grupo)][!is.na(grupo)]
t3[, grupo := lab_g[grupo]]
setnames(t3, c("Grupo de edad", "RTI 2011–2013 vs base", "RTI 2025 vs base", "RoR 2011–2013", "RoR 2025", "Razón de razones 2025/2011–2013"))
for (j in 2:6) set(t3, which(is.na(t3[[j]])), j, "—")
doc <- tab(doc, t3, "Tabla 3. Razones de tasas por edad (modelo 1) y razones de RTI frente a 1–4 años (modelo 2, efectos fijos de año)",
           widths = c(0.95, 1.05, 1.05, 1.05, 1.05, 1.05), size = 7.5)
doc <- add_p(doc, tx(es(sprintf("Contrafactual: con el patrón de edad de 2011–2013, en 2025 habrían ocurrido %s casos adicionales en menores de 2 meses (observados: %d) y %s en menores de 6 semanas; las muertes adicionales estimadas fueron %s con la letalidad de 2011–2013 y %s con la de 2025.",
                                cf[grupo == "<2 mo", casos_evitados], cf[grupo == "<2 mo", observados_2025], cf[grupo == "<6 wk", casos_evitados],
                                cf[grupo == "<2 mo", muertes_evitadas_letalidad2011_13], cf[grupo == "<2 mo", muertes_evitadas_letalidad2025]))))
doc <- fig(doc, "Figura2_edad_firma_Tdap", "Figura 2. Distribución de edad (A), razones de RTI frente a 1–4 años en cada epidemia (B) y razón de razones 2025/2011–2013 en lactantes (C).")
doc <- fig(doc, "Figura3_superficie_Lexis", "Figura 3. Superficie de Lexis de la incidencia por edad simple y año.")
ts <- se[, .(Análisis = analisis, `RoRR <6 sem` = `RoRR <6 sem`, `RoRR <2 m` = `RoRR <2 m`, `RoR <2 m 2025` = `RoR <2 m 2025`, `RoR 10–14 a 2025` = `RoR 10-14 a 2025`)]
doc <- tab(doc, ts, "Tabla 4. Análisis de sensibilidad del hallazgo principal", widths = c(2.3, 0.98, 0.98, 0.98, 0.96), size = 7.5)
doc <- fig(doc, "FiguraS1_sensibilidad_RoRR", "Figura S1. Sensibilidad de la razón de razones (2025/2011–2013) en lactantes pequeños.")

doc <- add_h(doc, "2.3 Cohortes nacidas en la pandemia y cronología de 2025", 2)
doc <- add_p(doc, tx(es(sprintf("El exceso en las cohortes 2019–2022 dependió de la flexibilidad de la curva de edad: con 3 grados de libertad el RR fue %s (2019–2021, base 2015–2019), pero con 4–5 grados de libertad se acercó a 1 (%s). En el modelo edad-periodo-cohorte bayesiano, las cohortes 2019–2022 no mostraron exceso frente a las de 2014–2018 (razón %s). Conclusión: no hay evidencia robusta de que las dosis perdidas durante la pandemia expliquen la epidemia.",
                                coh[base == "2015-2019" & cohortes == "2019-2021" & df == 3, ci(irr, lo, hi)],
                                coh[base == "2015-2019" & cohortes == "2019-2021" & df == 4, ci(irr, lo, hi)], apc[, ci(exp_dif, lo, hi)]))))
doc <- add_p(doc, tx(es(sprintf("Cronología de 2025: la mediana de inicio de síntomas fue más temprana en niños de 1–9 años (%s) y más tardía en adolescentes y adultos (%s y %s); la diferencia entre menores de 2 meses y mayores de 10 años fue de %.0f días (IC95%% %.0f a %.0f). No hay evidencia de que los mayores precedieran a los lactantes.",
                                tim[grp == "1–9 years", format(as.Date(mediana), "%d/%m")], tim[grp == "10–19 years", format(as.Date(mediana), "%d/%m")],
                                tim[grp == "20+ years", format(as.Date(mediana), "%d/%m")], dmed$dias, dmed$lo, dmed$hi))))
doc <- fig(doc, "FiguraS3_APC_cohortes", "Figura S3. Curvatura de cohorte en el modelo edad-periodo-cohorte bayesiano.", w = 5.2)
doc <- fig(doc, "FiguraS4_cronologia_2025", "Figura S4. Mediana de inicio de síntomas por grupo de edad en 2025.", w = 4.8)

doc <- add_h(doc, "2.4 Letalidad infantil e inequidades", 2)
lt <- c("edad_g<6 semanas" = "<6 semanas (vs 6–11 meses)", "edad_g6-8 semanas" = "6–8 semanas", "edad_g2-3 meses" = "2–3 meses", "edad_g4-5 meses" = "4–5 meses",
        "periodo42014-2019" = "2014–2019 (vs 2011–2013)", "periodo42020-2024" = "2020–2024", "periodo42025" = "2025",
        "etnia3Indígena" = "Indígena (vs sin pertenencia)", "etnia3Afrodescendiente" = "Afrodescendiente", "regimen4Subsidiado" = "Subsidiado (vs contributivo)",
        "regimen4No asegurado" = "No asegurado", "regimen4Otro" = "Otro régimen", "rural" = "Rural", "mujer" = "Mujer", "lab" = "Confirmado por laboratorio",
        "migrante" = "Migrante", "z_cpn4" = "Control prenatal municipal (por DE)")
mk <- function(mod) orb[modelo == mod, .(termino, v = ci(or, lo, hi))]
t5 <- Reduce(function(x, y) merge(x, y, by = "termino", all = TRUE),
             list(setnames(mk("Mixto principal"), "v", "Mixto (principal)"), setnames(mk("Firth (sin efecto aleatorio)"), "v", "Firth"),
                  setnames(mk("Mixto + % nacidos con >=4 controles prenatales (municipal, DE)"), "v", "Mixto + control prenatal")))
t5 <- t5[match(names(lt), termino)][!is.na(termino)][, termino := lt[termino]]
for (j in 2:4) set(t5, which(is.na(t5[[j]])), j, "—")
setnames(t5, "termino", "Variable")
doc <- tab(doc, t5, sprintf("Tabla 5. Factores asociados con la muerte en lactantes con tos ferina, 2011–2025 (n = %s; %d muertes); OR (IC95%%)",
                           num_es(ob$n[1]), ob$muertes[1]), widths = c(2.0, 1.4, 1.4, 1.4))
tl <- let[, .(Periodo = periodo4, Grupo = etnia3, Lactantes = lactantes, Muertes = muertes, `Letalidad (IC95% exacto)` = letalidad)]
doc <- tab(doc, tl, "Tabla 6. Letalidad cruda en lactantes por etnia y periodo", widths = c(1.0, 1.4, 0.9, 0.9, 1.9))
tm <- met[, .(Periodo = periodo, `Muertes indígenas` = m_ind, `Tasa indígena /100 000` = f2(tasa_ind, 1), `Muertes no indígenas` = m_noind,
              `Tasa no indígena /100 000` = f2(tasa_noind, 1), `Razón de tasas (IC95%)` = ci(rr, rr_lo, rr_hi, 1))]
doc <- tab(doc, tm, "Tabla 7. Mortalidad infantil por tos ferina según etnia (denominador indígena aproximado)", widths = c(0.9, 0.85, 1.0, 0.85, 1.0, 1.6))
ti2 <- iq[, .(Periodo = periodo, Quintil = quintil, `Casos` = O, `SIR` = f2(sir), `Mortalidad infantil /100 000` = f2(mort_inf_100k), `Letalidad` = sprintf("%.1f%%", 100 * letalidad),
              `RII incidencia` = RII_incidencia, `RII mortalidad infantil` = RII_mortalidad_infantil)]
doc <- tab(doc, ti2, "Tabla 8. Desigualdades según el IPM municipal (Q1 menos pobre, Q5 más pobre)", widths = c(0.7, 0.55, 0.55, 0.5, 0.95, 0.65, 1.15, 1.15), size = 7.5)
doc <- fig(doc, "Figura6_letalidad_inequidad", "Figura 6. Factores asociados con la muerte (A), letalidad por etnia (B) y mortalidad infantil por quintil de pobreza municipal (C).")

doc <- add_h(doc, "2.5 Análisis espacial", 2)
lc <- c(z_ipm = "IPM", z_p_rural = "% rural", z_p_ind = "% indígena", z_p_afro = "% afrodescendiente", z_log_dens = "log densidad",
        z_penta_1519 = "Pentavalente 2015–2019", z_penta_0810 = "Pentavalente 2008–2010", z_cpn4_1924 = "≥4 controles prenatales 2019–2024",
        z_cpn4_0810 = "≥4 controles prenatales 2008–2010")
t9 <- dcast(fe[termino != "(Intercept)", .(Covariable = lc[termino], modelo, v = ci(rr, lo, hi))], Covariable ~ modelo, value.var = "v")
setcolorder(t9, c("Covariable", "2011-2013 <1 año", "2011-2013 todas las edades", "2025 <1 año", "2025 todas las edades", "2025 10+ años (control negativo)"))
setnames(t9, c("Covariable", "2011–2013 <1 año", "2011–2013 todas", "2025 <1 año", "2025 todas", "2025 10+ años (control negativo)"))
for (j in 2:6) set(t9, which(is.na(t9[[j]])), j, "—")
doc <- tab(doc, t9, "Tabla 9. Modelos BYM2: riesgo relativo por 1 DE de cada covariable municipal (IC95% de credibilidad)", widths = c(1.45, 0.95, 0.95, 0.95, 0.95, 0.95), size = 7.5)
doc <- add_p(doc, tx(es(sprintf("Modelo de cambio (2025 frente a la línea base propia de cada municipio): control prenatal %s; pentavalente %s; IPM %s. Autocorrelación espacial (I de Moran, bayes empírico): 2025 I = %s (p = %s); 2011–2013 I = %s (p = %s).",
                                fc[termino == "z_cpn4_1924_x25", ci(rr, lo, hi)], fc[termino == "z_penta_1519_x25", ci(rr, lo, hi)], fc[termino == "z_ipm_x25", ci(rr, lo, hi)],
                                f2(mor[periodo == "2025", I], 3), f2(mor[periodo == "2025", p], 3), f2(mor[periodo == "2011-2013", I], 3), f2(mor[periodo == "2011-2013", p], 3)))))
tk <- rbind(kd[, .(Ventana = "10%", Conglomerado = cluster, Municipios = n_municipios, Observados = observados, Esperados = f2(esperados, 1), SMR = f2(smr), p = f2(p, 3))],
            kd20[, .(Ventana = "20%", Conglomerado = cluster, Municipios = n_municipios, Observados = observados, Esperados = f2(esperados, 1), SMR = f2(smr), p = f2(p, 3))])
doc <- tab(doc, tk, "Tabla 10. Conglomerados espaciales de Kulldorff, 2025 (ventana máxima: % de la población)", widths = c(0.7, 0.95, 0.85, 0.85, 0.85, 0.7, 0.6))
doc <- add_p(doc, tx("Composición de los conglomerados (ventana 10%): 1) Valle de Aburrá, suroeste y occidente antioqueño; 2) Urabá y Alto Sinú, territorios emberá (Mutatá, Chigorodó, Dabeiba, Carepa, Uramita, Murindó, Tierralta); 3) Cartagena y municipios vecinos; 4) Yavaraté (Vaupés); 5) Puerto Gaitán (Meta). Con ventana del 20% aparece además un conglomerado Bogotá-Cundinamarca.", size = 10))
doc <- fig(doc, "Figura4_mapas_BYM2_cambio_conglomerados", "Figura 4. Riesgo relativo municipal (BYM2) en 2011–2013 (A) y 2025 (B), cambio frente a la línea base propia (C) y conglomerados de Kulldorff.")
doc <- fig(doc, "Figura5_control_prenatal_control_negativo", "Figura 5. Control prenatal y cobertura de pentavalente en los modelos espaciales (control negativo: 10+ años y epidemia previa a la Tdap).")
doc <- fig(doc, "FiguraS2_calidad_datos", "Figura S2. Proporción de casos confirmados por laboratorio (A) y quiebre de la serie municipal de cobertura de TerriData frente a WUENIC (B).")

# ===== 3. Cambios frente al resumen ===============================================
doc <- add_h(doc, "3. Cambios frente al resumen enviado (v5)", 1)
doc <- add_b(doc, es(c(
  sprintf("La razón de RTI (<2 meses vs 1–4 años) del resumen, 0,34 (IC95%% 0,26–0,44), provenía de un cálculo de Poisson que ignora la variabilidad entre años. Con el modelo binomial negativo con efectos fijos de año, el punto se mantiene y el intervalo se amplía: %s en 2025; la comparación directa con 2011–2013 (razón de razones) es %s. El mensaje se sostiene, con incertidumbre más honesta.", ci(o2$est, o2$lo, o2$hi), ci(r2$est, r2$lo, r2$hi)),
  "La asociación con la cobertura departamental de pentavalente (RR 0,70) no se confirmó con cobertura municipal 2015–2019; el hallazgo espacial robusto es el del control prenatal, con lógica de control negativo.",
  "Hallazgos nuevos: déficit/exceso por edad (deuda de casos), contrafactual de casos y muertes evitados, migrantes con mayor letalidad, inversión del gradiente social de la mortalidad, ampliación de la brecha étnica y conglomerados de Kulldorff.",
  "Se mantienen: magnitud de 2025, desplazamiento de edad, mortalidad infantil (3,9 vs 6,9 por 100\u00a0000), exceso de muertes indígenas y ausencia de efecto robusto de cohortes pandémicas.")))

# ===== 4. Mensajes clave ==========================================================
doc <- add_h(doc, "4. Mensajes clave sugeridos para el manuscrito", 1)
doc <- add_b(doc, c(
  "1) El rebote pospandémico fue selectivo por edad: adolescentes y adultos superaron con creces lo esperado, mientras los lactantes pequeños quedaron protegidos.",
  "2) Tres evidencias convergentes apuntan a la Tdap materna: gradiente de protección por edad dentro del primer año (máxima antes de la primera dosis), contraste con la epidemia previa a la Tdap y asociación con el control prenatal solo después de su introducción y solo en lactantes.",
  "3) La protección no llegó a todos: las muertes se concentraron en lactantes indígenas, rurales, migrantes y de municipios pobres, con un gradiente social que se invirtió y una brecha étnica que se amplió.",
  "4) Implicaciones: llevar la Tdap materna y el control prenatal a territorios indígenas y rurales con enfoque intercultural, garantizar la vacunación primaria oportuna y evaluar refuerzos en adolescentes y adultos, además de fortalecer la vigilancia con confirmación por laboratorio y vigilancia genómica (resistencia a macrólidos ya detectada en Perú en 2025)."))

# ===== 5. Limitaciones ============================================================
doc <- add_h(doc, "5. Limitaciones", 1)
doc <- add_b(doc, c(
  "Solo casos confirmados: el cambio en la confirmación (más PCR en 2025) puede inflar el aumento en mayores; se mitigó con el análisis restringido a laboratorio y comparando grupos de edad dentro del mismo año.",
  "Diseño ecológico para el mecanismo: sin estado vacunal individual (madre y lactante) en los microdatos públicos. Se recomienda solicitar al INS la ficha complementaria para un estudio de casos y controles o del método de cribado.",
  "El denominador indígena es aproximado (asume la misma estructura de edad), por lo que la magnitud absoluta de la brecha es incierta, aunque su tendencia es consistente.",
  "Proyecciones de población posCOVID y municipios creados después de 2018 (reasignados a su municipio de origen).",
  "El contrafactual supone que, sin los cambios ocurridos entre epidemias, el patrón de edad de 2011–2013 se habría repetido; incluye otros cambios además de la Tdap (por ejemplo, la primera dosis a las 6 semanas, que no afecta a los menores de 6 semanas)."))

# ===== 6. Próximos pasos ==========================================================
doc <- add_h(doc, "6. Próximos pasos", 1)
doc <- add_b(doc, c(
  "Redactar el manuscrito completo en inglés (formato Frontiers) a partir de este informe: Introducción, Métodos (SAP), Resultados, Discusión y material suplementario.",
  "Lista de verificación RECORD/STROBE y declaración de disponibilidad de datos y código (repositorio en GitHub/Zenodo, previa autorización del equipo).",
  "Aval o exención del comité de ética de la Universidad CES (datos públicos anonimizados).",
  "Opcional para fortalecer: solicitar al INS el estado vacunal (ficha 800) y las notificaciones de casos probables por año; datos de cobertura de Tdap materna por departamento."))
print(doc, target = sprintf("Informe_resultados_tosferina_%s.docx", VERSION))
cat("Informe generado.\n")
