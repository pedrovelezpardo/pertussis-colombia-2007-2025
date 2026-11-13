# =============================================================================
# 06_figuras.R
# Figuras para el manuscrito (texto en inglés). Salidas en 04_figuras/:
#   PDF (vectorial, fuentes incrustadas), SVG, EPS (vectorial, sin transparencias)
#   y TIFF 600 dpi (LZW) para el sistema de envío; PNG 300 dpi solo como vista previa.
# Paleta: instancia de referencia validada (categórica azul/naranja/aqua, rampa
# ordinal azul de 5 pasos, divergente azul<->rojo con punto medio gris).
# =============================================================================
suppressMessages({
  library(data.table); library(ggplot2); library(patchwork); library(sf); library(scales)
  library(svglite); library(ragg)
})
sf_use_s2(FALSE)
dir.create("04_figuras", showWarnings = FALSE)

# --- Tokens de diseño ------------------------------------------------------------
ink1 <- "#0b0b0b"; ink2 <- "#52514e"; ink3 <- "#898781"
grid_col <- "#e1e0d9"; base_col <- "#c3c2b7"; surface <- "#ffffff"
s1 <- "#2a78d6"; s2 <- "#eb6834"; s3 <- "#1baf7a"
ramp5 <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#0d366b")
seq_blue <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
div_cols <- c("#0d366b", "#256abf", "#86b6ef", "#f0efec", "#f2a6a5", "#e34948", "#8f1f1f")
FONT <- "Arial"

theme_pub <- function(base = 8) {
  theme_minimal(base_size = base, base_family = FONT) +
    theme(text = element_text(colour = ink1),
          axis.text = element_text(colour = ink2, size = base - 1),
          axis.title = element_text(colour = ink2, size = base),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.25),
          axis.line.x = element_line(colour = base_col, linewidth = 0.3),
          axis.ticks = element_line(colour = base_col, linewidth = 0.25),
          axis.ticks.length = unit(1.5, "pt"),
          legend.title = element_text(colour = ink2, size = base - 1),
          legend.text = element_text(colour = ink1, size = base - 1),
          legend.key.size = unit(8, "pt"),
          plot.title = element_text(face = "bold", size = base + 1, colour = ink1),
          plot.subtitle = element_text(colour = ink2, size = base - 0.5),
          plot.tag = element_text(face = "bold", size = base + 2),
          plot.title.position = "plot",
          plot.background = element_rect(fill = surface, colour = NA),
          panel.background = element_rect(fill = surface, colour = NA),
          plot.margin = margin(4, 6, 4, 4))
}

# PDF con el dispositivo Quartz (incrusta Arial); EPS con PostScript usando métricas
# Helvetica bajo el nombre "Arial" (equivalentes) y codificación WinAnsi (CP1252).
postscriptFonts(Arial = postscriptFonts()$Helvetica)
dev_pdf <- function(filename, width, height, ...) grDevices::quartz(file = filename, type = "pdf", width = width, height = height, bg = surface)
dev_eps <- function(filename, width, height, ...) grDevices::postscript(filename, width = width, height = height,
  horizontal = FALSE, onefile = FALSE, paper = "special", family = "Arial", encoding = "WinAnsi.enc", colormodel = "srgb")
save_fig <- function(p, name, w_mm, h_mm) {
  f <- file.path("04_figuras", name)
  ggsave(paste0(f, ".pdf"), p, width = w_mm, height = h_mm, units = "mm", device = dev_pdf)
  ggsave(paste0(f, ".svg"), p, width = w_mm, height = h_mm, units = "mm", device = svglite::svglite,
         bg = surface, system_fonts = list(sans = FONT, Arial = FONT))
  ggsave(paste0(f, ".eps"), p, width = w_mm, height = h_mm, units = "mm", device = dev_eps)
  ggsave(paste0(f, ".tiff"), p, width = w_mm, height = h_mm, units = "mm", device = ragg::agg_tiff,
         dpi = 600, compression = "lzw", bg = surface)
  ggsave(paste0(f, "_preview.png"), p, width = w_mm, height = h_mm, units = "mm", device = ragg::agg_png,
         dpi = 300, bg = surface)
  invisible(f)
}

cs     <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
inc_ge <- readRDS("01_datos/procesados/incidencia_grupo_edad_anio.rds")
irr    <- readRDS("01_datos/procesados/irr_edad.rds")
lex    <- readRDS("01_datos/procesados/lexis_edad_anio.rds")

age_en <- c("<2 m" = "<2 mo", "2-3 m" = "2–3 mo", "4-5 m" = "4–5 mo", "6-11 m" = "6–11 mo",
            "1-4 a" = "1–4 y", "5-9 a" = "5–9 y", "10-14 a" = "10–14 y", "15-19 a" = "15–19 y",
            "20-39 a" = "20–39 y", "40-59 a" = "40–59 y", "60+ a" = "60+ y")

# =============================================================================
# Figura 1. Dinámica 2007-2025: curva epidémica semanal e incidencia anual por edad
# =============================================================================
wk <- cs[!is.na(f_inicio) & f_inicio >= as.IDate("2007-01-01") & f_inicio <= as.IDate("2025-12-31"),
         .(casos = .N), by = .(semana = f_inicio - (wday(f_inicio) - 1L))]
wk <- merge(data.table(semana = seq(as.IDate("2006-12-31"), as.IDate("2025-12-28"), by = 7)), wk,
            by = "semana", all.x = TRUE)[is.na(casos), casos := 0L]
events <- data.table(fecha = as.IDate(c("2013-01-01", "2020-03-20", "2025-03-28")),
                     lab = c("Maternal Tdap\nintroduced (2013)", "COVID-19\nlockdown (2020)",
                             "First dose at 6 wk,\nTdap push (2025)"),
                     hj = c(-0.04, -0.04, 1.04))
p1a <- ggplot(wk, aes(as.Date(semana), casos)) +
  geom_area(fill = "#dbe8f9", colour = NA) +
  geom_line(colour = s1, linewidth = 0.35, lineend = "round", linejoin = "round") +
  geom_vline(data = events, aes(xintercept = as.Date(fecha)), colour = ink3, linewidth = 0.25) +
  geom_text(data = events, aes(x = as.Date(fecha), y = Inf, label = lab, hjust = hj), vjust = 1.15,
            size = 2.3, colour = ink2, family = FONT, lineheight = 0.9) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.005)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = label_comma()) +
  labs(x = NULL, y = "Confirmed cases per week", tag = "A",
       title = "Weekly confirmed pertussis cases by week of symptom onset, Colombia, 2007–2025")

ann <- copy(inc_ge)[, banda := fcase(grupo_edad %in% c("<2 m", "2-3 m", "4-5 m", "6-11 m"), "<1 year",
                                     grupo_edad %in% c("1-4 a", "5-9 a"), "1–9 years",
                                     default = "10+ years")]
ann <- ann[, .(casos = sum(casos), pop = sum(pop)), by = .(anio, banda)][, inc := casos / pop * 1e5]
ann[, banda := factor(banda, levels = c("<1 year", "1–9 years", "10+ years"))]
lab_end <- ann[anio == 2025]
p1b <- ggplot(ann, aes(anio, inc, colour = banda)) +
  annotate("rect", xmin = 2010.5, xmax = 2013.5, ymin = 0, ymax = Inf, fill = "#f4f4f2", colour = NA) +
  annotate("rect", xmin = 2024.5, xmax = 2025.5, ymin = 0, ymax = Inf, fill = "#f4f4f2", colour = NA) +
  geom_line(linewidth = 0.5, lineend = "round") +
  geom_point(size = 1.4, stroke = 0.6, shape = 21, fill = surface) +
  geom_text(data = lab_end, aes(label = sprintf("%s  %.1f", banda, inc)), hjust = -0.12, size = 2.4,
            colour = ink1, family = FONT, show.legend = FALSE) +
  scale_colour_manual(values = c(s1, s2, s3), name = "Age group") +
  scale_y_log10(labels = label_number(drop0trailing = TRUE), breaks = c(0.01, 0.1, 1, 10, 100, 1000)) +
  scale_x_continuous(breaks = seq(2007, 2025, 2), limits = c(2007, 2029.5), expand = expansion(mult = 0.01)) +
  labs(x = NULL, y = "Incidence per 100,000 (log scale)", tag = "B",
       title = "Annual incidence by age group (shaded: 2011–2013 and 2025 epidemics)") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left")
f1 <- (p1a + theme_pub()) / p1b + plot_layout(heights = c(1, 1.15))
save_fig(f1, "Figura1_dinamica_2007_2025", 180, 150)

# =============================================================================
# Figura 2. Desplazamiento de la edad y firma de la vacunación materna
# =============================================================================
d5 <- cs[!is.na(grupo_edad), .(n = .N), by = .(anio, g = fcase(grupo_edad == "<2 m", "<2 mo",
                                                         grupo_edad %in% c("2-3 m", "4-5 m", "6-11 m"), "2–11 mo",
                                                         grupo_edad == "1-4 a", "1–4 y",
                                                         grupo_edad %in% c("5-9 a", "10-14 a"), "5–14 y",
                                                         default = "15+ y"))]
d5[, g := factor(g, levels = c("<2 mo", "2–11 mo", "1–4 y", "5–14 y", "15+ y"))]
d5[, p := n / sum(n), by = anio]
totals <- d5[, .(N = sum(n)), by = anio]
p2a <- ggplot(d5, aes(factor(anio), p, fill = g)) +
  geom_col(width = 0.72, colour = surface, linewidth = 0.35) +
  geom_text(data = totals, aes(factor(anio), 1.015, label = format(N, big.mark = ",", trim = TRUE)), inherit.aes = FALSE,
            size = 2.0, colour = ink2, family = FONT, angle = 90, hjust = 0) +
  scale_fill_manual(values = ramp5, name = "Age at onset") +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, .25), limits = c(0, 1.13), expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = "Share of confirmed cases", tag = "A",
       title = "Age distribution of confirmed cases (n above bars)") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left",
                      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ir <- irr[comparacion %in% c("2011-2013 vs 2015-2019", "2025 vs 2015-2019")]
ir[, serie := fifelse(comparacion == "2025 vs 2015-2019", "2025 epidemic (post-maternal Tdap)",
                      "2011–2013 epidemic (pre-maternal Tdap)")]
ir[, edad := factor(age_en[as.character(grupo_edad)], levels = rev(age_en))]
p2b <- ggplot(ir, aes(irr, edad, colour = serie)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, orientation = "y",
                position = position_dodge(width = 0.55)) +
  geom_point(size = 1.8, shape = 21, fill = surface, stroke = 0.7, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c(s1, s2), name = NULL) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "IRR vs 2015–2019 (log scale, 95% CI)", y = NULL, tag = "B",
       title = "Age-specific incidence rate ratios") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left",
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.25),
                      panel.grid.major.y = element_blank()) +
  guides(colour = guide_legend(nrow = 2))
f2 <- p2a + p2b + plot_layout(widths = c(1.35, 1))
save_fig(f2, "Figura2_edad_vacunacion_materna", 180, 95)

# =============================================================================
# Figura 3. Superficie de Lexis (edad simple 0-19 x año) con cohortes pandémicas
# =============================================================================
lx <- copy(lex)[, inc_plot := fifelse(casos == 0, NA_real_, inc_100k)]
coh_lines <- data.table(cohorte = c(2019, 2022))  # límites de las cohortes 2019-2021
p3 <- ggplot(lx, aes(anio, edad, fill = inc_plot)) +
  geom_tile(colour = surface, linewidth = 0.25) +
  geom_segment(data = coh_lines, aes(x = cohorte - 1, y = -0.5, xend = 2025.5, yend = 2025.5 - cohorte + 0.5),
               colour = ink1, linewidth = 0.35, inherit.aes = FALSE) +
  annotate("text", x = 2025.75, y = 5.5, label = "Birth cohorts\n2019–2021", size = 2.3, colour = ink1,
           family = FONT, hjust = 0, lineheight = 0.9) +
  scale_fill_gradientn(colours = seq_blue, trans = "log10", limits = c(0.05, 500), oob = squish,
                       na.value = "#f2f1ee", breaks = c(0.1, 1, 10, 100),
                       labels = c("0.1", "1", "10", "100"), name = "Incidence per\n100,000\n(grey = 0)") +
  scale_x_continuous(breaks = seq(2007, 2025, 2), expand = c(0, 0), limits = c(2006.5, 2028.6)) +
  scale_y_continuous(breaks = seq(0, 19, 2), expand = c(0, 0)) +
  labs(x = "Calendar year", y = "Age (completed years)",
       title = "Lexis surface of confirmed pertussis incidence, ages 0–19, Colombia 2007–2025") +
  theme_pub() + theme(panel.grid.major.y = element_blank(), legend.key.height = unit(18, "pt"))
save_fig(p3, "Figura3_superficie_Lexis", 180, 100)

# =============================================================================
# Figura 4. Riesgo relativo suavizado (BYM2) por municipio: 2011-2013 vs 2025
# =============================================================================
bres <- readRDS("01_datos/procesados/bym2_resultados.rds")
geo <- st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE)
geo <- st_make_valid(st_transform(geo, 9377)); geo$MPIO <- geo$MPIO_CDPMP
geo <- geo[!(geo$DPTO_CCDGO %in% "88"), ]          # San Andrés se omite del mapa continental
dep <- st_union(st_buffer(geo, 0), by_feature = FALSE)
dep_b <- aggregate(geo[, "DPTO_CCDGO"], by = list(DP = geo$DPTO_CCDGO), FUN = function(x) x[1])
mm <- merge(geo[, c("MPIO")], as.data.frame(bres$mun)[, c("MPIO", "rr_bym2", "p_exc", "epidemia")], by = "MPIO")
mm$epidemia <- factor(mm$epidemia, levels = c("2011-2013", "2025"),
                      labels = c("2011–2013 epidemic", "2025 epidemic"))
mm$lrr <- log2(pmin(pmax(mm$rr_bym2, 1 / 16), 16))
hot <- mm[mm$p_exc > 0.95, ]
p4 <- ggplot() +
  geom_sf(data = mm, aes(fill = lrr), colour = NA) +
  geom_sf(data = dep_b, fill = NA, colour = "#ffffff", linewidth = 0.25) +
  geom_sf(data = hot, fill = NA, colour = ink1, linewidth = 0.18) +
  facet_wrap(~epidemia) +
  scale_fill_gradientn(colours = div_cols, limits = c(-4, 4), breaks = -4:4,
                       labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16"),
                       name = "Smoothed\nrelative risk") +
  labs(title = "Age-standardised relative risk of confirmed pertussis by municipality (Bayesian BYM2 model)",
       subtitle = "Black outline: posterior probability RR > 1 above 0.95. San Andrés archipelago not shown.") +
  theme_pub() + theme(axis.text = element_blank(), panel.grid.major = element_blank(),
                      axis.line.x = element_blank(), axis.ticks = element_blank(),
                      strip.text = element_text(face = "bold", size = 8, colour = ink1),
                      legend.key.height = unit(16, "pt"))
save_fig(p4, "Figura4_mapas_BYM2", 180, 118)

# =============================================================================
# Figura 5. Determinantes de muerte en lactantes (OR ajustados)
# =============================================================================
orB <- fread("03_resultados/modelo_B_muerte_lactantes_OR.csv")
lab_map <- c("grupo_edad<2 m" = "Age <2 months (vs 6–11)", "grupo_edad2-3 m" = "Age 2–3 months (vs 6–11)",
             "grupo_edad4-5 m" = "Age 4–5 months (vs 6–11)", "periodo42014-2019" = "2014–2019 (vs 2011–2013)",
             "periodo42020-2024" = "2020–2024 (vs 2011–2013)", "periodo42025" = "2025 (vs 2011–2013)",
             "etnia3Indígena" = "Indigenous (vs no ethnic group)", "etnia3Afrodescendiente" = "Afro-descendant (vs no ethnic group)",
             "regimen4Subsidiado" = "Subsidised insurance (vs contributory)", "regimen4No asegurado" = "Uninsured (vs contributory)",
             "regimen4Otro" = "Other insurance (vs contributory)", "rural" = "Rural residence (vs urban)",
             "mujer" = "Female (vs male)", "lab" = "Laboratory-confirmed (vs clinical/epi-link)")
orB[, lab := factor(lab_map[termino], levels = rev(lab_map))]
p5 <- ggplot(orB, aes(or, lab)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.9, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  geom_text(aes(x = 12.5, label = sprintf("%.2f (%.2f–%.2f)", or, lo, hi)), size = 2.3, hjust = 0,
            colour = ink2, family = FONT) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8), labels = label_number(drop0trailing = TRUE),
                limits = c(0.2, 40)) +
  labs(x = "Adjusted odds ratio of death (log scale, 95% CI)", y = NULL,
       title = "Factors associated with death among infants with confirmed pertussis, Colombia 2011–2025",
       subtitle = sprintf("Mixed-effects logistic regression with department random intercept; n = %s infants, %d deaths",
                          format(orB$n_lactantes[1], big.mark = ","), orB$n_muertes[1])) +
  theme_pub() + theme(panel.grid.major.y = element_blank(),
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.25))
save_fig(p5, "Figura5_muerte_lactantes", 180, 85)

# =============================================================================
# Figura S1. Sensibilidad: razones de tasas por edad con casos confirmados por laboratorio
# =============================================================================
irl <- readRDS("01_datos/procesados/irr_edad_lab.rds")
irl[, serie := fifelse(grepl("^2025", comparacion), "2025 epidemic (post-maternal Tdap)",
                       "2011–2013 epidemic (pre-maternal Tdap)")]
irl[, edad := factor(age_en[as.character(grupo_edad)], levels = rev(age_en))]
pS1 <- ggplot(irl, aes(irr, edad, colour = serie)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, orientation = "y",
                position = position_dodge(width = 0.55)) +
  geom_point(size = 1.8, shape = 21, fill = surface, stroke = 0.7, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c(s1, s2), name = NULL) +
  scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "IRR vs 2015–2019 (log scale, 95% CI)", y = NULL,
       title = "Sensitivity analysis: laboratory-confirmed cases only") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left",
                      panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.25),
                      panel.grid.major.y = element_blank()) +
  guides(colour = guide_legend(nrow = 2))
save_fig(pS1, "FiguraS1_sensibilidad_laboratorio", 110, 95)
cat("Figuras generadas en 04_figuras/\n")
