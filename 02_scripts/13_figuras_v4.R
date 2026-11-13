# =============================================================================
# 13_figuras_v4.R: figuras del manuscrito (versión 4, formato Frontiers, 12-11-2026)
# Principales: 1 dinámica y déficit de casos; 2 estructura de edad; 3 estudio año por año
# (nuevo); 4 mapas (BYM2, probabilidad posterior, cambio, conglomerados); 5 control
# prenatal por grupo de edad (modelo conjunto); 6 muerte infantil y desigualdad.
# Suplementarias, en el orden en que el texto las cita: S1 flujo de registros (nueva); S2 sensibilidad;
# S3 superficie de Lexis (antes figura 3); S4 APC; S5 cronología 2025; S6 calidad de datos.
# Cambios frente a la v3 (revisión independiente del 12-11-2026):
#   1A: la introducción de la Tdap materna se marca como el año 2013 (mes no documentado)
#       y la línea de 2025 se rotula como la respuesta de la Circular 0011.
#   1B: el sombreado del déficit empieza en enero de 2020, como el análisis.
#   2A: los números sobre las barras son casos con edad conocida (así lo dice el título).
#   2C: sin el contrafactual en la figura (va en el texto como escenario hipotético).
#   4: probabilidad posterior de RR > 1 como panel propio; la leyenda del cambio dice que es
#      el componente espacial ajustado por covariables; geometría con las unidades unidas.
#   5: pendientes del control prenatal por grupo de edad del modelo conjunto.
#   6A: modelo principal sin el tipo de confirmación; 6B: indígenas frente a lactantes sin
#       pertenencia étnica, el mismo grupo del texto y de la tabla S5; 6C: IC por quintil.
# =============================================================================
suppressMessages({ library(data.table); library(sf) })
source("02_scripts/00_tema_figuras.R")
sf_use_s2(FALSE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
cs <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
fmt_ci <- function(e, l, h, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f–%.", d, "f)"), e, l, h)

# =============================================================================
# Figura 1
# =============================================================================
wk <- cs[!is.na(f_inicio) & f_inicio >= as.IDate("2007-01-01") & f_inicio <= as.IDate("2025-12-31"),
         .(casos = .N), by = .(semana = f_inicio - (wday(f_inicio) - 1L))]
wk <- merge(data.table(semana = seq(as.IDate("2006-12-31"), as.IDate("2025-12-28"), by = 7)), wk, by = "semana", all.x = TRUE)[is.na(casos), casos := 0L]
ev <- data.table(fecha = as.IDate(c("2020-03-20", "2025-03-28")),
                 lab = c("COVID-19\nlockdown (2020)", "2025 response\n(Circular 0011)"), hj = c(1.04, 1.04))
p1a <- ggplot(wk, aes(as.Date(semana), casos)) +
  annotate("rect", xmin = as.Date("2013-01-01"), xmax = as.Date("2014-01-01"), ymin = -Inf, ymax = Inf, fill = shade) +
  annotate("text", x = as.Date("2014-02-15"), y = Inf, label = "Maternal Tdap\nintroduced (2013)", hjust = 0, vjust = 1.1,
           size = TXT, colour = ink, family = FONT, lineheight = 0.9) +
  geom_area(fill = wash, colour = NA) +
  geom_line(colour = s1, linewidth = 0.35, lineend = "round", linejoin = "round") +
  geom_vline(data = ev, aes(xintercept = as.Date(fecha)), colour = line_col, linewidth = 0.3) +
  geom_text(data = ev, aes(x = as.Date(fecha), y = Inf, label = lab, hjust = hj), vjust = 1.1, size = TXT,
            colour = ink, family = FONT, lineheight = 0.9) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.005)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.32)), labels = label_comma()) +
  labs(x = NULL, y = "Confirmed cases per week",
       title = "(A) Weekly confirmed pertussis cases by week of symptom onset, Colombia, 2007–2025") + theme_pub()

sm <- readRDS("01_datos/procesados/series_mensuales_esperado.rds")[band != "All ages" & mes >= as.IDate("2018-01-01")]
band_lab <- c("<1 year" = "<1 year", "1–9 years" = "1–9 years", "10+ years" = "≥10 years")
sm[, band := factor(band_lab[band], levels = band_lab)]
de <- fread("03_resultados/series_deficit_exceso.csv", encoding = "UTF-8")[especificacion == "principal" & banda != "All ages" & periodo != "Balance 2020-2025"]
lab_de <- de[, .(txt = paste0(fifelse(periodo == "Déficit 2020-2024", "2020–2024: O/E ", "2025: O/E "),
                              fmt_ci(razon_obs_esp, razon_lo, razon_hi))), by = .(band = banda, periodo)]
lab_de <- lab_de[, .(txt = paste(txt, collapse = "\n")), by = band][, band := factor(band_lab[band], levels = band_lab)]
fin1 <- sm[band == "≥10 years"][which.max(mes)]
lab_ser <- data.table(band = factor("≥10 years", levels = band_lab), mes = as.Date(fin1$mes) + 25,
                      y = spread_labels(c(fin1$N, fin1$esp), 0.18 * max(sm[band == "≥10 years", c(N, hi)])),
                      txt = c("Observed", "Expected\n(95% PI)"))
p1b <- ggplot(sm, aes(as.Date(mes))) +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2024-12-31"), ymin = -Inf, ymax = Inf, fill = shade) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = wash2, colour = NA) +
  geom_line(aes(y = esp), colour = s2, linewidth = 0.45, linetype = "22") +
  geom_line(aes(y = N), colour = s1, linewidth = 0.45) +
  geom_text(data = lab_de, aes(x = as.Date("2018-02-01"), y = Inf, label = txt), hjust = 0, vjust = 1.15,
            size = TXT, colour = ink, family = FONT, lineheight = 0.95, inherit.aes = FALSE) +
  geom_text(data = lab_ser, aes(x = mes, y = y, label = txt), hjust = 0, size = TXT, colour = ink, family = FONT,
            lineheight = 0.9, inherit.aes = FALSE) +
  facet_wrap(~band, ncol = 3, scales = "free_y") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.55))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Confirmed cases per month",
       title = "(B) Observed versus expected monthly cases by age (shaded: January 2020 to December 2024)") +
  theme_pub() + theme(plot.margin = margin(4, 40, 4, 4), panel.spacing.x = unit(6, "mm"))
save_fig(p1a / p1b + plot_layout(heights = c(1, 1.1)), "Figura1_dinamica_deficit", 180, 165)

# =============================================================================
# Figura 2
# =============================================================================
d5 <- cs[!is.na(grupo_edad), .(n = .N), by = .(anio, g = fcase(grupo_edad == "<2 m", "<2 mo",
  grupo_edad %in% c("2-3 m", "4-5 m", "6-11 m"), "2–11 mo", grupo_edad == "1-4 a", "1–4 y",
  grupo_edad %in% c("5-9 a", "10-14 a"), "5–14 y", default = "15+ y"))]
lev5 <- c("<2 mo", "2–11 mo", "1–4 y", "5–14 y", "15+ y")
lab5 <- c("<2 mo" = "<2 months", "2–11 mo" = "2–11 months", "1–4 y" = "1–4 years", "5–14 y" = "5–14 years", "15+ y" = "≥15 years")
d5[, g := factor(g, levels = lev5)][, p := n / sum(n), by = anio]
tot <- d5[, .(N = sum(n)), by = anio]
lb5 <- d5[anio == max(anio)][order(-as.integer(g))][, ymid := cumsum(p) - p / 2]
lb5[, y_lab := spread_labels(ymid, 0.085, 0.04, 0.96)][, txt := lab5[as.character(g)]]
x_last <- length(unique(d5$anio))
p2a <- ggplot(d5, aes(factor(anio), p, fill = g)) +
  geom_col(width = 0.72, colour = surface, linewidth = 0.35) +
  geom_text(data = tot, aes(factor(anio), 1.02, label = format(N, big.mark = ",", trim = TRUE)), inherit.aes = FALSE,
            size = TXT, colour = ink, family = FONT, angle = 90, hjust = 0, vjust = 0.5) +
  geom_segment(data = lb5, aes(x = x_last + 0.38, xend = x_last + 0.62, y = ymid, yend = y_lab), inherit.aes = FALSE,
               colour = line_col, linewidth = 0.25) +
  geom_text(data = lb5, aes(x = x_last + 0.68, y = y_lab, label = txt), inherit.aes = FALSE, hjust = 0,
            size = TXT, colour = ink, family = FONT) +
  annotate("text", x = x_last + 0.68, y = 1.02, label = "Age", hjust = 0, vjust = 0, size = TXT,
           fontface = "bold", colour = ink, family = FONT) +
  scale_fill_manual(values = setNames(ramp5, lev5)) +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, .25), limits = c(0, 1.17), expand = expansion(mult = 0)) +
  scale_x_discrete(expand = expansion(add = c(0.6, 0.6))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Share of confirmed cases", title = "(A) Age distribution of confirmed cases (cases with known age above bars)") +
  theme_pub() + theme(plot.margin = margin(4, 72, 4, 4), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

em <- readRDS("01_datos/procesados/edad_modelos.rds")
ord <- c("<6 wk", "6–8 wk", "2–3 mo", "4–5 mo", "6–11 mo", "5–9 y", "10–14 y", "15–19 y", "20–39 y", "40–59 y", "60+ y")
lab_age <- c(`<6 wk` = "<6 weeks", `6–8 wk` = "6–8 weeks", `2–3 mo` = "2–3 months", `4–5 mo` = "4–5 months", `6–11 mo` = "6–11 months",
             `5–9 y` = "5–9 years", `10–14 y` = "10–14 years", `15–19 y` = "15–19 years", `20–39 y` = "20–39 years",
             `40–59 y` = "40–59 years", `60+ y` = "≥60 years")
ro <- em$ror[grupo %in% ord & contraste %in% c("RoR 2011-2013", "RoR 2025")]
ro[, `:=`(serie = factor(fifelse(contraste == "RoR 2025", "2025 epidemic", "2011–2013 epidemic\n(before maternal Tdap)"),
                         levels = c("2011–2013 epidemic\n(before maternal Tdap)", "2025 epidemic")),
          per = fifelse(contraste == "RoR 2025", "2025", "2011–2013"),
          edad = factor(lab_age[grupo], levels = rev(lab_age[ord])))]
p2b <- ggplot(ro, aes(est, edad, colour = per)) +
  geom_vline(xintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, orientation = "y") +
  geom_point(size = 1.8, shape = 21, fill = surface, stroke = 0.7) +
  facet_wrap(~serie, ncol = 2) +
  scale_colour_manual(values = per_cols) +
  scale_x_log10(breaks = c(0.1, 0.3, 1, 3, 10), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "Ratio of IRRs vs children aged 1–4 years,\nrelative to 2015–2019 (log scale, 95% CI)", y = NULL,
       title = "(B) Age-specific risk relative to children aged 1–4 years") +
  theme_forest() + theme(panel.spacing.x = unit(5, "mm"), strip.clip = "off")
rr <- em$ror[grupo %in% ord[1:5] & contraste == "RoRR 2025/2011-2013"][, edad := factor(lab_age[grupo], levels = rev(lab_age[ord[1:5]]))]
p2c <- ggplot(rr, aes(est, edad)) +
  geom_vline(xintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.9, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  geom_text(aes(x = Inf, label = sprintf("%.2f (%.2f–%.2f)", est, lo, hi)), hjust = -0.08, size = TXT, colour = ink, family = FONT) +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = label_number(drop0trailing = TRUE), limits = c(0.1, 3)) +
  coord_cartesian(clip = "off") +
  labs(x = "Ratio of ratios, 2025 vs 2011–2013\n(log scale, 95% CI)", y = NULL, title = "(C) Ratio of ratios among infants") +
  theme_forest() + theme(plot.margin = margin(4, 82, 4, 4))
save_fig(p2a / ((p2b | p2c) + plot_layout(widths = c(1.3, 1))) + plot_layout(heights = c(1, 1.3)), "Figura2_estructura_edad", 180, 190)

# =============================================================================
# Figura 3 (estudio año por año)
# =============================================================================
es <- fread("03_resultados/edad_estudio_eventos.csv", encoding = "UTF-8")[analisis == "Todos los confirmados"]
es[, ref := factor(referencia, levels = c("1–4 y", "6–11 mo"),
                   labels = c("(A) Infants <2 months vs children aged 1–4 years", "(B) Infants <2 months vs infants aged 6–11 months"))]
es[, fase := fifelse(anio %in% 2020:2024, "pandemia", "otro")]
ylim3 <- c(0.08, 12)
lb3 <- data.table(ref = factor(levels(es$ref)[1], levels = levels(es$ref)),
                  x = c(2012.8, 2017), y = c(ylim3[2], ylim3[2]), hj = c(1, 0.5),
                  txt = c("Maternal Tdap\nintroduced (2013)", "2015–2019\nbaseline"))
lb3p <- data.table(ref = factor(levels(es$ref)[1], levels = levels(es$ref)), x = 2025.4, y = ylim3[1] * 1.12,
                   txt = "2020–2024: few cases (gray)")
p3 <- ggplot(es, aes(anio, est)) +
  annotate("rect", xmin = 2014.5, xmax = 2019.5, ymin = ylim3[1], ymax = ylim3[2], fill = shade) +
  geom_hline(yintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_vline(xintercept = 2013, colour = line_col, linewidth = 0.3, linetype = "22") +
  geom_errorbar(aes(ymin = lo, ymax = hi, colour = fase), width = 0, linewidth = 0.45) +
  geom_point(aes(colour = fase), size = 1.8, shape = 21, fill = surface, stroke = 0.7) +
  geom_text(data = lb3, aes(x = x, y = y, label = txt, hjust = hj), vjust = 1, size = TXT, colour = ink, family = FONT,
            lineheight = 0.9, inherit.aes = FALSE) +
  geom_text(data = lb3p, aes(x = x, y = y, label = txt), hjust = 1, size = TXT, colour = ink, family = FONT, inherit.aes = FALSE) +
  facet_wrap(~ref, ncol = 2) +
  scale_colour_manual(values = c(otro = s1, pandemia = line_col)) +
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 8), labels = label_number(drop0trailing = TRUE), limits = ylim3, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(2007, 2025, 3), limits = c(2006.5, 2025.5)) +
  labs(x = NULL, y = "Ratio of incidence rate ratios\nrelative to 2015–2019 (log scale, 95% CI)",
       title = "Incidence in young infants relative to two reference groups, by year, compared with the 2015–2019 baseline") +
  theme_forest() + theme(panel.spacing.x = unit(6, "mm"), strip.clip = "off")
save_fig(p3, "Figura3_estudio_anio_por_anio", 180, 85)

# =============================================================================
# Figura 4 (mapas)
# =============================================================================
er <- readRDS("01_datos/procesados/espacial_resultados.rds")
geo <- er$geo[substr(er$geo$MPIO, 1, 2) != "88", ]
dep_b <- aggregate(geo[, "MPIO"], by = list(DP = substr(geo$MPIO, 1, 2)), FUN = function(x) x[1])
MUN <- as.data.table(er$MUN)
mm <- MUN[modelo %in% c("2011-2013 todas las edades", "2025 todas las edades"), .(MPIO, val = rr, panel = modelo)]
mm[, panel := factor(panel, levels = c("2011-2013 todas las edades", "2025 todas las edades"),
                     labels = c("(A) 2011–2013: relative risk", "(B) 2025: relative risk and clusters"))]
mg <- merge(geo[, "MPIO"], as.data.frame(mm), by = "MPIO")
mg$l2 <- log2(pmin(pmax(mg$val, 1 / 16), 16))
clp <- do.call(rbind, lapply(seq_along(er$kd_ids), function(i) {
  x <- geo[geo$MPIO %in% er$kd_ids[[i]], ]; if (!nrow(x)) return(NULL)
  st_sf(cluster = i, geometry = st_union(st_buffer(x, 0)))
}))
clp$panel <- factor("(B) 2025: relative risk and clusters", levels = levels(mm$panel))
th_map <- theme_pub() + theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.line.x = element_blank(),
                              axis.ticks = element_blank(), legend.position = "right", legend.key.height = unit(16, "pt"))
esc_div <- scale_fill_gradientn(colours = div_cols, limits = c(-4, 4), breaks = -4:4,
                                labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16"), name = "Ratio",
                                guide = guide_colourbar(display = "rectangles"))
p4ab <- ggplot() +
  geom_sf(data = mg, aes(fill = l2), colour = NA) +
  geom_sf(data = dep_b, fill = NA, colour = "#ffffff", linewidth = 0.2) +
  geom_sf(data = clp, fill = NA, colour = ink, linewidth = 0.35) +
  facet_wrap(~panel, ncol = 2) + esc_div + th_map
pe <- merge(geo[, "MPIO"], as.data.frame(MUN[modelo == "2025 todas las edades", .(MPIO, p_exc)]), by = "MPIO")
p4c <- ggplot() +
  geom_sf(data = pe, aes(fill = p_exc), colour = NA) +
  geom_sf(data = dep_b, fill = NA, colour = "#ffffff", linewidth = 0.2) +
  geom_sf(data = pe[pe$p_exc > 0.95, ], fill = NA, colour = ink, linewidth = 0.25) +
  scale_fill_gradientn(colours = c("#F2F2F2", seq_blue), limits = c(0, 1), breaks = c(0, 0.5, 0.95),
                       labels = c("0", "0.5", "0.95"), name = "Probability",
                       guide = guide_colourbar(display = "rectangles")) +
  labs(title = "(C) 2025: posterior probability that RR > 1\n(outlined: above 0.95)") + th_map
ch <- merge(geo[, "MPIO"], as.data.frame(er$chg[, .(MPIO, l2 = log2(pmin(pmax(cambio, 1 / 16), 16)))]), by = "MPIO")
p4d <- ggplot() +
  geom_sf(data = ch, aes(fill = l2), colour = NA) +
  geom_sf(data = dep_b, fill = NA, colour = "#ffffff", linewidth = 0.2) +
  esc_div + labs(title = "(D) 2025 vs own 2015–2019 baseline: spatial\ncomponent of the change, adjusted for covariates") + th_map
save_fig(p4ab / (p4c | p4d) + plot_layout(heights = c(1, 1)), "Figura4_mapas_riesgo_municipal", 180, 205)

# =============================================================================
# Figura 5 (control prenatal por grupo de edad, modelo conjunto)
# =============================================================================
cj <- fread("03_resultados/espacial_cpn_modelo_conjunto.csv", encoding = "UTF-8")[modelo %in% c("Conjunto 2025", "Conjunto 2011-2013") & grepl("^CPN", termino)]
est_lev <- c("<2 m", "2-5 m", "6-11 m", "1-9 a", "10+ a")
est_lab <- c("<2 months", "2–5 months", "6–11 months", "1–9 years", "≥10 years")
cj[, est := factor(est_lab[match(sub("CPN, ", "", termino), est_lev)], levels = rev(est_lab))]
cj[, per := factor(fifelse(modelo == "Conjunto 2025", "2025", "2011–2013"), levels = c("2011–2013", "2025"))]
cj[, y := as.integer(est) + fifelse(per == "2025", 0.17, -0.17)]
lb5c <- cj[est == "<2 months"][, .(y = y, txt = fifelse(per == "2025", "2025", "2011–2013 (before maternal Tdap)"))]
p5 <- ggplot(cj, aes(rr, y, colour = per)) +
  geom_vline(xintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, orientation = "y") +
  geom_point(aes(shape = per), size = 1.9, fill = surface, stroke = 0.7) +
  geom_text(aes(x = 1.62, label = sprintf("%.2f (%.2f–%.2f)", rr, lo, hi)), hjust = 0, size = TXT, colour = ink, family = FONT) +
  geom_text(data = lb5c, aes(x = 0.36, y = y, label = txt), hjust = 0, size = TXT, colour = ink, family = FONT, inherit.aes = FALSE) +
  scale_colour_manual(values = per_cols) + scale_shape_manual(values = c(`2011–2013` = 21, `2025` = 24)) +
  scale_y_continuous(breaks = seq_along(levels(cj$est)), labels = levels(cj$est), expand = expansion(add = 0.5)) +
  scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1, 1.25), labels = label_number(drop0trailing = TRUE), limits = c(0.35, 2.6)) +
  labs(x = "Relative risk per 1 SD of municipal antenatal-care coverage (log scale, 95% CrI)", y = NULL,
       title = "Antenatal-care coverage and confirmed incidence by age group, joint Bayesian municipal model") +
  theme_forest()
save_fig(p5, "Figura5_control_prenatal_por_edad", 180, 80)

# =============================================================================
# Figura 6 (muerte infantil y desigualdad)
# =============================================================================
ob <- fread("03_resultados/letalidad_modelos_OR.csv", encoding = "UTF-8")[modelo == "Principal (sin tipo de confirmación)"]
lm6 <- c("edad_g<6 semanas" = "Age <6 weeks (vs 6–11 months)", "edad_g6-8 semanas" = "Age 6–8 weeks", "edad_g2-3 meses" = "Age 2–3 months",
         "edad_g4-5 meses" = "Age 4–5 months", "periodo42014-2019" = "2014–2019 (vs 2011–2013)", "periodo42020-2024" = "2020–2024",
         "periodo42025" = "2025", "etnia3Indígena" = "Indigenous (vs no ethnic group)", "etnia3Afrodescendiente" = "Afro-descendant",
         "regimen4Subsidiado" = "Subsidized insurance (vs contributory)", "regimen4No asegurado" = "Uninsured",
         "regimen4Especial" = "Special or exception scheme", "rural" = "Rural residence", "mujer" = "Female", "migrante" = "Migrant")
ob[, lab := factor(lm6[termino], levels = rev(lm6))]
p6a <- ggplot(ob, aes(or, lab)) +
  geom_vline(xintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.7, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8, 16), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "Adjusted odds ratio of death\n(log scale, 95% CI)", y = NULL,
       title = sprintf("(A) Death among infants, 2011–2025\n(n = %s; %d deaths)", format(ob$n[1], big.mark = ","), ob$muertes[1])) +
  theme_forest()
lc <- fread("03_resultados/letalidad_cruda_etnia_periodo.csv", encoding = "UTF-8")[etnia3 %in% c("Indígena", "Otro/ninguno")]
lc[, grp := fifelse(etnia3 == "Indígena", "Indigenous", "No ethnic group")][, periodo4 := gsub("-", "–", periodo4)]
lc[, c("p", "lo", "hi") := { b <- mapply(function(x, n) binom.test(x, n)$conf.int, muertes, lactantes); list(muertes / lactantes, b[1, ], b[2, ]) }]
dg <- 0.45
lc[, xn := as.integer(factor(periodo4)) + fifelse(grp == "Indigenous", dg / 4, -dg / 4)]
lb6b <- lc[periodo4 == max(periodo4)][, .(xn = xn + 0.18, y = p, txt = grp)]
p6b <- ggplot(lc, aes(xn, p, colour = grp, shape = grp)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.45) +
  geom_point(size = 1.9, fill = surface, stroke = 0.7) +
  geom_text(data = lb6b, aes(x = xn, y = y, label = txt), inherit.aes = FALSE, hjust = 0, size = TXT, colour = ink, family = FONT) +
  scale_colour_manual(values = c(Indigenous = s2, `No ethnic group` = s1)) +
  scale_shape_manual(values = c(Indigenous = 24, `No ethnic group` = 21)) +
  scale_x_continuous(breaks = 1:4, labels = sub("–", "–\n", sort(unique(lc$periodo4))), expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Case fatality (95% CI)", title = "(B) Infant case fatality by ethnicity") +
  theme_pub() + theme(plot.margin = margin(4, 70, 4, 4))
iq <- fread("03_resultados/inequidades_ipm_quintiles.csv", encoding = "UTF-8")
iq[, periodo := factor(periodo, levels = c("2011-2013", "2014-2019", "2025"), labels = c("2011–2013", "2014–2019", "2025"))]
iq[, xq := quintil + (as.integer(periodo) - 2) * 0.16]
ymax6 <- max(iq$mort_hi, na.rm = TRUE)
rii <- unique(iq[, .(periodo, RII_mortalidad_infantil, SII_mortalidad_infantil)])
rii[, txt := paste0(periodo, ": RII ", gsub("(\\d)-(\\d)", "\\1–\\2", RII_mortalidad_infantil))]
lb6c <- iq[quintil == 5, .(periodo, y = mort_inf_100k)][, y_lab := spread_labels(y, 0.075 * ymax6)]
p6c <- ggplot(iq, aes(xq, mort_inf_100k, colour = periodo, shape = periodo)) +
  geom_line(linewidth = 0.4) +
  geom_errorbar(aes(ymin = mort_lo, ymax = mort_hi), width = 0, linewidth = 0.4) +
  geom_point(size = 1.7, fill = surface, stroke = 0.7) +
  geom_text(data = lb6c, aes(x = 5.35, y = y_lab, label = periodo), inherit.aes = FALSE, hjust = 0, size = TXT, colour = ink, family = FONT) +
  annotate("text", x = 0.75, y = ymax6 * 1.32, label = paste(rii$txt, collapse = "\n"), hjust = 0, vjust = 1,
           size = TXT, colour = ink, family = FONT, lineheight = 0.95) +
  scale_colour_manual(values = per_cols) +
  scale_shape_manual(values = c(`2011–2013` = 21, `2014–2019` = 22, `2025` = 24)) +
  scale_x_continuous(breaks = 1:5, labels = c("Q1\nleast poor", "Q2", "Q3", "Q4", "Q5\npoorest"), expand = expansion(add = c(0.25, 0.15))) +
  scale_y_continuous(limits = c(0, ymax6 * 1.32), expand = expansion(mult = c(0, 0.02))) +
  coord_cartesian(clip = "off") +
  labs(x = "Municipal poverty quintile (MPI)", y = "Infant deaths per 100,000\ninfant-years (95% CI)",
       title = "(C) Infant mortality by municipal poverty") +
  theme_pub() + theme(plot.margin = margin(4, 62, 4, 4))
save_fig(p6a | (p6b / p6c) + plot_layout(widths = c(1.05, 1)), "Figura6_muerte_infantil_desigualdad", 180, 165)

# =============================================================================
# Suplementarias
# =============================================================================
# S1: flujo de registros por análisis
recode_mpio <- function(x) fcase(x == "94663", "94343", x == "27493", "27615", default = x)
fl <- fread("03_resultados/letalidad_flujo_exclusiones.csv", encoding = "UTF-8")
n_tot <- nrow(cs); n_edad <- cs[!is.na(edad_dias), .N]
n_ts <- cs[!is.na(f_inicio) & f_inicio >= as.IDate("2007-01-01") & f_inicio <= as.IDate("2025-12-31"), .N]
n_mun <- cs[!is.na(edad_dias) & !is.na(mpio_res) & recode_mpio(mpio_res) %in% er$geo$MPIO, .N]
n_inf <- fl[paso == "Lactantes notificados 2011-2025", n]; n_dp <- fl[paso == "Sin departamento de residencia", n]
n_reg <- fl[paso == "Régimen de salud indeterminado", n]; n_m <- fl[paso == "Analizados (casos completos)", n]
f0 <- function(x) format(x, big.mark = ",")
cajas <- data.table(
  x = c(0.5, 0.18, 0.5, 0.82, 0.5, 0.5),
  y = c(0.92, 0.62, 0.62, 0.62, 0.32, 0.06),
  txt = c(sprintf("Confirmed pertussis cases in the public SIVIGILA microdata,\nevent 800, 2007–2025: n = %s", f0(n_tot)),
          sprintf("Time-series analyses\n(onset 2007–2025)\nn = %s", f0(n_ts)),
          sprintf("Age-structure analyses\n(age recorded)\nn = %s\n(no age: %s)", f0(n_edad), f0(n_tot - n_edad)),
          sprintf("Municipal analyses\n(age and municipality\nrecorded): n = %s", f0(n_mun)),
          sprintf("Infants notified in 2011–2025: n = %s\nExcluded: no department of residence (%s),\nunknown insurance scheme (%s)", f0(n_inf), f0(n_dp), f0(n_reg)),
          sprintf("Infant death models: n = %s (%s deaths)", f0(n_m), f0(fl[paso == "Analizados (casos completos)", muertes]))))
fle <- data.table(x = c(0.5, 0.5, 0.5, 0.5), xend = c(0.18, 0.5, 0.82, 0.5), y = c(0.86, 0.86, 0.86, 0.53), yend = c(0.70, 0.70, 0.70, 0.40))
fle <- rbind(fle, data.table(x = 0.5, xend = 0.5, y = 0.24, yend = 0.10))
pS1 <- ggplot() +
  geom_segment(data = fle, aes(x = x, y = y, xend = xend, yend = yend), colour = line_col, linewidth = 0.35,
               arrow = arrow(length = unit(1.6, "mm"), type = "closed")) +
  geom_label(data = cajas, aes(x = x, y = y, label = txt), size = TXT, family = FONT, colour = ink, fill = surface,
             label.size = 0.3, label.padding = unit(2.2, "mm"), lineheight = 0.95) +
  scale_x_continuous(limits = c(0, 1)) + scale_y_continuous(limits = c(0, 1)) +
  theme_void(base_family = FONT) + theme(plot.background = element_rect(fill = surface, colour = NA))
save_fig(pS1, "FiguraS1_flujo_registros", 180, 110)

# S2: sensibilidad de la razón de razones
se <- fread("03_resultados/edad_sensibilidad.csv", encoding = "UTF-8")
en_map <- c("Principal (confirmados; base 2015-2019; NB con efectos fijos de año)" = "Main analysis (all confirmed cases, 2015–2019\nbaseline, NB model with year fixed effects)",
            "Solo confirmados por laboratorio" = "Laboratory-confirmed only", "Sin duplicados probables" = "Probable duplicates removed",
            "Sin posibles renotificaciones (marcador amplio)" = "Possible renotifications removed (broad marker)",
            "Año de inicio de síntomas" = "Year of symptom onset", "Línea base 2014-2019" = "Baseline 2014–2019",
            "Línea base 2017-2019" = "Baseline 2017–2019", "Sin Antioquia" = "Excluding Antioquia", "Cuasi-Poisson" = "Quasi-Poisson",
            "Referencia 6-11 meses" = "Reference group 6–11 months")
s1d <- rbind(cbind(se[, .(analisis)], as.data.table(parse_ci(se$`RoRR <6 sem`)), grupo = "Infants <6 weeks"),
             cbind(se[, .(analisis)], as.data.table(parse_ci(se$`RoRR <2 m`)), grupo = "Infants <2 months"))
setnames(s1d, c("V1", "V2", "V3"), c("est", "lo", "hi"))
s1d[, an := factor(en_map[analisis], levels = rev(en_map))]
pS2 <- ggplot(s1d, aes(est, an)) +
  geom_vline(xintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.8, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  facet_wrap(~grupo) + scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "Ratio of ratios, 2025 vs 2011–2013 (log scale, 95% CI)", y = NULL,
       title = "Sensitivity analyses for the ratio of ratios among young infants") +
  theme_forest() + theme(panel.spacing.x = unit(5, "mm"))
save_fig(pS2, "FiguraS2_sensibilidad_RoRR", 180, 100)

# S3: calidad de los datos
qc <- fread("03_resultados/calidad_confirmacion_por_anio.csv")
pS3a <- ggplot(qc, aes(anio, pct_laboratorio / 100)) + geom_col(width = 0.7, fill = s1) +
  scale_x_continuous(breaks = seq(2007, 2025, 3)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Laboratory-confirmed (%)", title = "(A) Share of cases confirmed by laboratory") +
  theme_pub()
tw <- melt(fread("03_resultados/calidad_cobertura_terridata_vs_wuenic.csv")[, .(anio, `TerriData municipal median` = mediana_municipal,
                                                                              `WUENIC national DTP3` = wuenic_dtp3)], id.vars = "anio")
lbS3 <- tw[!is.na(value), .SD[which.max(anio)], by = variable][, y_lab := spread_labels(value, 9)]
lbS3[, txt := fifelse(variable == "WUENIC national DTP3", "WUENIC\nnational DTP3", "TerriData\nmunicipal median")]
pS3b <- ggplot(tw, aes(anio, value, colour = variable, shape = variable)) +
  annotate("rect", xmin = 2020.5, xmax = 2025.5, ymin = -Inf, ymax = Inf, fill = shade) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.5, fill = surface, stroke = 0.6) +
  annotate("text", x = 2020.7, y = 104, label = "Series break in\nmunicipal data", hjust = 0, vjust = 1, size = TXT, colour = ink, family = FONT, lineheight = 0.9) +
  geom_text(data = lbS3, aes(x = anio + 0.7, y = y_lab, label = txt), inherit.aes = FALSE, hjust = 0, size = TXT, colour = ink,
            family = FONT, lineheight = 0.9) +
  scale_colour_manual(values = c(`TerriData municipal median` = s1, `WUENIC national DTP3` = s2)) +
  scale_shape_manual(values = c(`TerriData municipal median` = 21, `WUENIC national DTP3` = 24)) +
  scale_y_continuous(limits = c(40, 106)) + coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Pentavalent or DTP3 coverage (%)", title = "(B) Municipal pentavalent coverage vs national estimates") +
  theme_pub() + theme(plot.margin = margin(4, 58, 4, 4))
save_fig(pS3a | pS3b, "FiguraS6_calidad_datos", 180, 85)

# S4: superficie de Lexis (figura 3 de la v3)
lex <- readRDS("01_datos/procesados/lexis_edad_anio.rds")[, inc_plot := fifelse(casos == 0, NA_real_, inc_100k)]
cl <- data.table(cohorte = c(2019, 2022))
pS4 <- ggplot(lex, aes(anio, edad, fill = inc_plot)) +
  geom_tile(colour = surface, linewidth = 0.25) +
  geom_segment(data = cl, aes(x = cohorte - 1, y = -0.5, xend = 2025.5, yend = 2025.5 - cohorte + 0.5), colour = ink, linewidth = 0.35, inherit.aes = FALSE) +
  annotate("text", x = 2025.75, y = 5.5, label = "Birth cohorts\n2019–2021", size = TXT, colour = ink, family = FONT, hjust = 0, lineheight = 0.9) +
  scale_fill_gradientn(colours = seq_blue, trans = "log10", limits = c(0.05, 500), oob = squish, na.value = zero_fill,
                       breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"), name = "Incidence per\n100,000\n(grey = 0)",
                       guide = guide_colourbar(display = "rectangles")) +
  scale_x_continuous(breaks = seq(2007, 2025, 2), expand = c(0, 0), limits = c(2006.5, 2029.2)) +
  scale_y_continuous(breaks = seq(0, 19, 2), expand = c(0, 0)) +
  labs(x = "Calendar year", y = "Age (completed years)", title = "Lexis surface of confirmed pertussis incidence, ages 0–19 years, Colombia, 2007–2025") +
  theme_pub() + theme(panel.grid.major.y = element_blank(), axis.line.x = element_blank(), legend.position = "right", legend.key.height = unit(20, "pt"))
save_fig(pS4, "FiguraS3_superficie_Lexis", 180, 105)

# S5: APC
ap <- readRDS("01_datos/procesados/apc_inla.rds")$coh
ylimS5 <- c(0.95 * min(exp(ap$lo)), 1.9)
pS5 <- ggplot(ap, aes(cohorte, exp(dev))) +
  annotate("rect", xmin = 2018.5, xmax = 2022.5, ymin = ylimS5[1], ymax = ylimS5[2], fill = shade) +
  geom_hline(yintercept = 1, colour = line_col, linewidth = 0.3) +
  geom_ribbon(aes(ymin = exp(lo), ymax = exp(hi)), fill = wash) + geom_line(colour = s1, linewidth = 0.5) +
  annotate("text", x = 2018.3, y = 1.85, label = "Pandemic-era\ncohorts (shaded)", hjust = 1, vjust = 1, size = TXT, colour = ink, family = FONT, lineheight = 0.9) +
  scale_y_log10(limits = ylimS5, expand = c(0, 0)) + labs(x = "Birth cohort", y = "Detrended cohort effect\n(rate ratio, log scale)",
                         title = "Bayesian age–period–cohort model (ages 0–19 years, 2007–2025): cohort effects after removing a linear trend") + theme_pub()
save_fig(pS5, "FiguraS4_APC_cohortes", 180, 85)

# S6: cronología 2025
tm <- fread("03_resultados/cronologia_2025_mediana_inicio.csv")
grp_lab <- c("<2 months" = "<2 months", "2–11 months" = "2–11 months", "1–9 years" = "1–9 years", "10–19 years" = "10–19 years", "20+ years" = "≥20 years")
tm[, grp := factor(grp_lab[grp], levels = rev(grp_lab))]
pS6 <- ggplot(tm, aes(as.Date(mediana), grp)) +
  geom_errorbar(aes(xmin = as.Date(lo), xmax = as.Date(hi)), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.9, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  geom_text(aes(x = as.Date("2025-09-12"), label = paste0("n = ", n)), hjust = 0, size = TXT, colour = ink, family = FONT) +
  scale_x_date(date_labels = "%b %Y", limits = as.Date(c("2025-05-15", "2025-10-05"))) +
  labs(x = "Median date of symptom onset (95% bootstrap CI)", y = NULL, title = "Timing of the 2025 epidemic by age group (cases with onset in 2025)") + theme_forest()
save_fig(pS6, "FiguraS5_cronologia_2025", 180, 70)
cat("Figuras v4 generadas en", FIG_DIR, "\n")
cat("Flujo: total", n_tot, "| edad", n_edad, "| series", n_ts, "| municipal", n_mun, "| lactantes", n_inf, "| modelo muerte", n_m, "\n")
