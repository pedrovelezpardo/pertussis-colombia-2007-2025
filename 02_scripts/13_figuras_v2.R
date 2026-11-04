# =============================================================================
# 13_figuras_v2.R — figuras del manuscrito (versión 2, análisis completos)
# Principales: 1 dinámica y deuda de casos; 2 edad y firma de la Tdap materna;
# 3 superficie de Lexis; 4 mapas (BYM2, cambio, conglomerados); 5 control
# prenatal como control negativo; 6 letalidad e inequidad.
# Suplementarias: S1 sensibilidad; S2 calidad de datos; S3 APC; S4 cronología 2025.
# =============================================================================
suppressMessages({ library(data.table); library(sf) })
source("02_scripts/00_tema_figuras.R")
sf_use_s2(FALSE)
dir.create("04_figuras", showWarnings = FALSE)
cs <- readRDS("01_datos/procesados/tosferina_casos_2007_2025.rds")
fmt_ci <- function(e, l, h, d = 2) sprintf(paste0("%.", d, "f (%.", d, "f–%.", d, "f)"), e, l, h)

# =============================================================================
# Figura 1
# =============================================================================
wk <- cs[!is.na(f_inicio) & f_inicio >= as.IDate("2007-01-01") & f_inicio <= as.IDate("2025-12-31"),
         .(casos = .N), by = .(semana = f_inicio - (wday(f_inicio) - 1L))]
wk <- merge(data.table(semana = seq(as.IDate("2006-12-31"), as.IDate("2025-12-28"), by = 7)), wk, by = "semana", all.x = TRUE)[is.na(casos), casos := 0L]
ev <- data.table(fecha = as.IDate(c("2013-01-01", "2020-03-20", "2025-03-28")),
                 lab = c("Maternal Tdap\nintroduced (2013)", "COVID-19\nlockdown (2020)", "First dose at 6 wk,\nTdap push (2025)"),
                 hj = c(-0.04, -0.04, 1.04))
p1a <- ggplot(wk, aes(as.Date(semana), casos)) +
  geom_area(fill = wash, colour = NA) +
  geom_line(colour = s1, linewidth = 0.35, lineend = "round", linejoin = "round") +
  geom_vline(data = ev, aes(xintercept = as.Date(fecha)), colour = ink3, linewidth = 0.25) +
  geom_text(data = ev, aes(x = as.Date(fecha), y = Inf, label = lab, hjust = hj), vjust = 1.15, size = 2.3,
            colour = ink2, family = FONT, lineheight = 0.9) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.005)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = label_comma()) +
  labs(x = NULL, y = "Confirmed cases per week", tag = "A",
       title = "Weekly confirmed pertussis cases by week of symptom onset, Colombia, 2007–2025") + theme_pub()

sm <- readRDS("01_datos/procesados/series_mensuales_esperado.rds")[band != "All ages" & mes >= as.IDate("2018-01-01")]
sm[, band := factor(band, levels = c("<1 year", "1–9 years", "10+ years"))]
de <- fread("03_resultados/series_deficit_exceso.csv", encoding = "UTF-8")[especificacion == "principal" & banda != "All ages" & periodo != "Balance 2020-2025"]
lab_de <- de[, .(txt = paste0(fifelse(periodo == "Déficit 2020-2024", "2020–2024: O/E ", "2025: O/E "),
                              fmt_ci(razon_obs_esp, razon_lo, razon_hi))), by = .(band = banda, periodo)]
lab_de <- lab_de[, .(txt = paste(txt, collapse = "\n")), by = band][, band := factor(band, levels = levels(sm$band))]
p1b <- ggplot(sm, aes(as.Date(mes))) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2024-12-31"), ymin = -Inf, ymax = Inf, fill = shade) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = "Expected from 2015–2019 (95% PI)"), colour = NA) +
  geom_line(aes(y = esp, colour = "Expected from 2015–2019 (95% PI)"), linewidth = 0.35) +
  geom_line(aes(y = N, colour = "Observed"), linewidth = 0.45) +
  geom_text(data = lab_de, aes(x = as.Date("2018-02-01"), y = Inf, label = txt), hjust = 0, vjust = 1.2,
            size = 2.2, colour = ink2, family = FONT, lineheight = 0.95, inherit.aes = FALSE) +
  facet_wrap(~band, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("Expected from 2015–2019 (95% PI)" = wash), guide = "none") +
  scale_colour_manual(values = c("Expected from 2015–2019 (95% PI)" = s1, "Observed" = s2), name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(x = NULL, y = "Confirmed cases per month", tag = "B",
       title = "Observed versus expected monthly cases by age (shaded: pandemic period, Mar 2020–Dec 2024)") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left")
save_fig(p1a / p1b + plot_layout(heights = c(1, 1.05)), "Figura1_dinamica_deuda", 180, 150)

# =============================================================================
# Figura 2
# =============================================================================
d5 <- cs[!is.na(grupo_edad), .(n = .N), by = .(anio, g = fcase(grupo_edad == "<2 m", "<2 mo",
  grupo_edad %in% c("2-3 m", "4-5 m", "6-11 m"), "2–11 mo", grupo_edad == "1-4 a", "1–4 y",
  grupo_edad %in% c("5-9 a", "10-14 a"), "5–14 y", default = "15+ y"))]
d5[, g := factor(g, levels = c("<2 mo", "2–11 mo", "1–4 y", "5–14 y", "15+ y"))][, p := n / sum(n), by = anio]
tot <- d5[, .(N = sum(n)), by = anio]
p2a <- ggplot(d5, aes(factor(anio), p, fill = g)) +
  geom_col(width = 0.72, colour = surface, linewidth = 0.35) +
  geom_text(data = tot, aes(factor(anio), 1.015, label = format(N, big.mark = ",", trim = TRUE)), inherit.aes = FALSE,
            size = 2.0, colour = ink2, family = FONT, angle = 90, hjust = 0) +
  scale_fill_manual(values = ramp5, name = "Age at onset") +
  scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, .25), limits = c(0, 1.13), expand = expansion(mult = 0)) +
  labs(x = NULL, y = "Share of confirmed cases", tag = "A", title = "Age distribution of confirmed cases (n above bars)") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

em <- readRDS("01_datos/procesados/edad_modelos.rds")
ord <- c("<6 wk", "6–8 wk", "2–3 mo", "4–5 mo", "6–11 mo", "5–9 y", "10–14 y", "15–19 y", "20–39 y", "40–59 y", "60+ y")
lab_age <- c(`<6 wk` = "<6 weeks", `6–8 wk` = "6–8 weeks", `2–3 mo` = "2–3 months", `4–5 mo` = "4–5 months", `6–11 mo` = "6–11 months",
             `5–9 y` = "5–9 years", `10–14 y` = "10–14 years", `15–19 y` = "15–19 years", `20–39 y` = "20–39 years",
             `40–59 y` = "40–59 years", `60+ y` = "60+ years")
ro <- em$ror[grupo %in% ord & contraste %in% c("RoR 2011-2013", "RoR 2025")]
ro[, `:=`(serie = fifelse(contraste == "RoR 2025", "2025 epidemic (post-maternal Tdap)", "2011–2013 epidemic (pre-maternal Tdap)"),
          edad = factor(lab_age[grupo], levels = rev(lab_age[ord])))]
p2b <- ggplot(ro, aes(est, edad, colour = serie)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, orientation = "y", position = position_dodge(width = 0.6)) +
  geom_point(size = 1.8, shape = 21, fill = surface, stroke = 0.7, position = position_dodge(width = 0.6)) +
  scale_colour_manual(values = c(s1, s2), name = NULL) +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "Ratio of IRRs vs children 1–4 years (log scale, 95% CI)", y = NULL, tag = "B",
       title = "Age-specific risk relative to children aged 1–4 years",
       subtitle = "NB model with year fixed effects;\neach epidemic vs 2015–2019 baseline") +
  theme_forest() + theme(legend.position = "top", legend.justification = "left") + guides(colour = guide_legend(nrow = 2))

rr <- em$ror[grupo %in% ord[1:5] & contraste == "RoRR 2025/2011-2013"][, edad := factor(lab_age[grupo], levels = rev(lab_age[ord[1:5]]))]
cf <- fread("03_resultados/edad_contrafactual_evitados.csv", encoding = "UTF-8")
txt_cf <- sprintf("Counterfactual (2011–2013 age pattern): %s\nadditional cases <2 months; %s among <6 weeks",
                  sub(" \\(", " (95% UI ", cf[grupo == "<2 mo", casos_evitados]), cf[grupo == "<6 wk", casos_evitados])
p2c <- ggplot(rr, aes(est, edad)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s2, orientation = "y") +
  geom_point(size = 1.9, shape = 21, fill = surface, colour = s2, stroke = 0.7) +
  geom_text(aes(x = 3.0, label = sprintf("%.2f (%.2f–%.2f)", est, lo, hi)), hjust = 0, size = 2.2, colour = ink2, family = FONT) +
  scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = label_number(drop0trailing = TRUE), limits = c(0.1, 22)) +
  labs(x = "Ratio of ratios, 2025 vs 2011–2013 (log scale, 95% CI)", y = NULL, tag = "C",
       title = "Additional relative protection of young infants in 2025", subtitle = txt_cf) +
  theme_forest() + theme(plot.margin = margin(4, 6, 4, 4))
save_fig(p2a / (p2b | p2c) + plot_layout(heights = c(1, 1.15)), "Figura2_edad_firma_Tdap", 180, 175)

# =============================================================================
# Figura 3 (Lexis)
# =============================================================================
lex <- readRDS("01_datos/procesados/lexis_edad_anio.rds")[, inc_plot := fifelse(casos == 0, NA_real_, inc_100k)]
cl <- data.table(cohorte = c(2019, 2022))
p3 <- ggplot(lex, aes(anio, edad, fill = inc_plot)) +
  geom_tile(colour = surface, linewidth = 0.25) +
  geom_segment(data = cl, aes(x = cohorte - 1, y = -0.5, xend = 2025.5, yend = 2025.5 - cohorte + 0.5), colour = ink1, linewidth = 0.35, inherit.aes = FALSE) +
  annotate("text", x = 2025.75, y = 5.5, label = "Birth cohorts\n2019–2021", size = 2.3, colour = ink1, family = FONT, hjust = 0, lineheight = 0.9) +
  scale_fill_gradientn(colours = seq_blue, trans = "log10", limits = c(0.05, 500), oob = squish, na.value = "#f2f1ee",
                       breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100"), name = "Incidence per\n100,000\n(grey = 0)") +
  scale_x_continuous(breaks = seq(2007, 2025, 2), expand = c(0, 0), limits = c(2006.5, 2028.6)) +
  scale_y_continuous(breaks = seq(0, 19, 2), expand = c(0, 0)) +
  labs(x = "Calendar year", y = "Age (completed years)", title = "Lexis surface of confirmed pertussis incidence, ages 0–19, Colombia 2007–2025") +
  theme_pub() + theme(panel.grid.major.y = element_blank(), legend.key.height = unit(18, "pt"))
save_fig(p3, "Figura3_superficie_Lexis", 180, 100)

# =============================================================================
# Figura 4 (mapas)
# =============================================================================
er <- readRDS("01_datos/procesados/espacial_resultados.rds")
geo <- st_read("01_datos/raw/dane_mgn/mgn2018_mpio_simpl.geojson", quiet = TRUE)
geo <- st_make_valid(st_transform(geo, 9377)); geo$MPIO <- geo$MPIO_CDPMP
geo <- geo[geo$DPTO_CCDGO != "88", ]
dep_b <- aggregate(geo[, "DPTO_CCDGO"], by = list(DP = geo$DPTO_CCDGO), FUN = function(x) x[1])
mm <- rbind(as.data.table(er$MUN)[modelo %in% c("2011-2013 todas las edades", "2025 todas las edades"), .(MPIO, val = rr, panel = modelo)],
            er$chg[, .(MPIO, val = cambio, panel = "cambio")])
mm[, panel := factor(panel, levels = c("2011-2013 todas las edades", "2025 todas las edades", "cambio"),
                     labels = c("A  2011–2013: relative risk", "B  2025: relative risk",
                                "C  2025: change vs own baseline"))]
mg <- merge(geo[, "MPIO"], as.data.frame(mm), by = "MPIO")
mg$l2 <- log2(pmin(pmax(mg$val, 1 / 16), 16))
clp <- do.call(rbind, lapply(seq_along(er$kd_ids), function(i) {
  x <- geo[geo$MPIO %in% er$kd_ids[[i]], ]; if (!nrow(x)) return(NULL)
  st_sf(cluster = i, geometry = st_union(st_buffer(x, 0)))
}))
clp$panel <- factor("B  2025: relative risk", levels = levels(mm$panel))
p4 <- ggplot() +
  geom_sf(data = mg, aes(fill = l2), colour = NA) +
  geom_sf(data = dep_b, fill = NA, colour = "#ffffff", linewidth = 0.2) +
  geom_sf(data = clp, fill = NA, colour = ink1, linewidth = 0.3) +
  facet_wrap(~panel, ncol = 3) +
  scale_fill_gradientn(colours = div_cols, limits = c(-4, 4), breaks = -4:4,
                       labels = c("1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16"), name = "Ratio") +
  labs(title = "Municipal risk of confirmed pertussis: Bayesian BYM2 models and spatial scan",
       subtitle = "A–B: age-standardised relative risk. C: change 2025 vs 2015–2019 relative to the national change.\nBlack outlines in B: Kulldorff spatial-scan clusters (p < 0.05). San Andrés archipelago not shown.") +
  theme_pub() + theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.line.x = element_blank(),
                      axis.ticks = element_blank(), legend.key.height = unit(16, "pt"), strip.text = element_text(size = 7.5))
save_fig(p4, "Figura4_mapas_BYM2_cambio_conglomerados", 180, 100)

# =============================================================================
# Figura 5 (control prenatal: control negativo)
# =============================================================================
fe <- fread("03_resultados/espacial_bym2_efectos_fijos.csv", encoding = "UTF-8")
fc <- fread("03_resultados/espacial_cambio_efectos_fijos.csv", encoding = "UTF-8")
f5 <- rbind(fe[termino %in% c("z_cpn4_1924", "z_cpn4_0810", "z_penta_1519", "z_penta_0810")],
            fc[termino %in% c("z_cpn4_1924_x25", "z_penta_1519_x25")])
f5[, covar := fifelse(grepl("cpn4", termino), "Antenatal care (% births with ≥4 visits)", "Pentavalent coverage (pre-epidemic period)")]
f5[, mod := factor(fcase(modelo == "2011-2013 <1 año", "2011–2013, infants <1 y (pre-maternal Tdap)",
                         modelo == "2011-2013 todas las edades", "2011–2013, all ages",
                         modelo == "2025 <1 año", "2025, infants <1 y",
                         modelo == "2025 todas las edades", "2025, all ages",
                         modelo == "2025 10+ años (control negativo)", "2025, ages 10+ (negative control)",
                         default = "Change 2025 vs 2015–2019"),
                   levels = rev(c("2011–2013, infants <1 y (pre-maternal Tdap)", "2011–2013, all ages", "2025, infants <1 y",
                                  "2025, ages 10+ (negative control)", "2025, all ages", "Change 2025 vs 2015–2019")))]
p5 <- ggplot(f5, aes(rr, mod)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.9, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  geom_text(aes(x = 1.75, label = sprintf("%.2f (%.2f–%.2f)", rr, lo, hi)), hjust = 0, size = 2.2, colour = ink2, family = FONT) +
  facet_wrap(~covar, ncol = 2) +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5), labels = label_number(drop0trailing = TRUE), limits = c(0.4, 3.2)) +
  labs(x = "Relative risk per 1 SD of the municipal covariate (log scale, 95% CrI)", y = NULL,
       title = "Antenatal care coverage was associated with lower infant risk only after maternal Tdap introduction",
       subtitle = "Bayesian BYM2 models adjusted for poverty (IPM), rurality, Indigenous and Afro-descendant share, and population density") +
  theme_forest()
save_fig(p5, "Figura5_control_prenatal_control_negativo", 180, 72)

# =============================================================================
# Figura 6 (letalidad e inequidad)
# =============================================================================
ob <- fread("03_resultados/letalidad_modelos_OR.csv", encoding = "UTF-8")[modelo == "Mixto principal"]
lm6 <- c("edad_g<6 semanas" = "Age <6 weeks (vs 6–11 months)", "edad_g6-8 semanas" = "Age 6–8 weeks", "edad_g2-3 meses" = "Age 2–3 months",
         "edad_g4-5 meses" = "Age 4–5 months", "periodo42014-2019" = "2014–2019 (vs 2011–2013)", "periodo42020-2024" = "2020–2024",
         "periodo42025" = "2025", "etnia3Indígena" = "Indigenous (vs no ethnic group)", "etnia3Afrodescendiente" = "Afro-descendant",
         "regimen4Subsidiado" = "Subsidised insurance (vs contributory)", "regimen4No asegurado" = "Uninsured", "regimen4Otro" = "Other insurance",
         "rural" = "Rural residence", "mujer" = "Female", "lab" = "Laboratory-confirmed", "migrante" = "Migrant")
ob[, lab := factor(lm6[termino], levels = rev(lm6))]
p6a <- ggplot(ob, aes(or, lab)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.7, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8, 16), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "Adjusted odds ratio of death (log scale, 95% CI)", y = NULL, tag = "A",
       title = sprintf("Death among infants, 2011–2025 (n = %s; %d deaths)", format(ob$n[1], big.mark = ","), ob$muertes[1])) +
  theme_forest()
lc <- fread("03_resultados/letalidad_cruda_etnia_periodo.csv", encoding = "UTF-8")
lc[, grp := fifelse(etnia3 == "Indígena", "Indigenous", "Non-Indigenous")]
lc <- lc[, .(lact = sum(lactantes), m = sum(muertes)), by = .(periodo4, grp)][, periodo4 := gsub("-", "–", periodo4)]
lc[, c("p", "lo", "hi") := { b <- mapply(function(x, n) binom.test(x, n)$conf.int, m, lact); list(m / lact, b[1, ], b[2, ]) }]
p6b <- ggplot(lc, aes(periodo4, p, colour = grp)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.45, position = position_dodge(width = 0.45)) +
  geom_point(size = 1.8, shape = 21, fill = surface, stroke = 0.7, position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = c(Indigenous = s2, `Non-Indigenous` = s1), name = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(x = NULL, y = "Case fatality (95% CI)", tag = "B", title = "Infant case fatality by ethnicity") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left")
iq <- fread("03_resultados/inequidades_ipm_quintiles.csv", encoding = "UTF-8")
iq[, periodo := factor(periodo, levels = c("2011-2013", "2014-2019", "2025"), labels = c("2011–2013", "2014–2019", "2025"))]
rii <- unique(iq[, .(periodo, RII_mortalidad_infantil)])
rii[, txt := paste0(periodo, ": RII ", RII_mortalidad_infantil)]
p6c <- ggplot(iq, aes(quintil, mort_inf_100k, colour = periodo)) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.6, shape = 21, fill = surface, stroke = 0.7) +
  annotate("text", x = 2.45, y = max(iq$mort_inf_100k) * 1.02, label = paste(rii$txt, collapse = "\n"), hjust = 0, vjust = 1,
           size = 2.1, colour = ink2, family = FONT, lineheight = 0.95) +
  scale_colour_manual(values = c(`2011–2013` = s1, `2014–2019` = s3, `2025` = s2), name = NULL) +
  scale_x_continuous(breaks = 1:5, labels = c("Q1\nleast poor", "Q2", "Q3", "Q4", "Q5\npoorest")) +
  labs(x = "Municipal poverty quintile (IPM)", y = "Infant deaths per 100,000 infants", tag = "C",
       title = "Infant mortality by municipal poverty") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left")
save_fig(p6a | (p6b / p6c) + plot_layout(widths = c(1.15, 1)), "Figura6_letalidad_inequidad", 180, 135)

# =============================================================================
# Suplementarias
# =============================================================================
se <- fread("03_resultados/edad_sensibilidad.csv", encoding = "UTF-8")
s1d <- rbind(cbind(se[, .(analisis)], as.data.table(parse_ci(se$`RoRR <6 sem`)), grupo = "Infants <6 weeks"),
             cbind(se[, .(analisis)], as.data.table(parse_ci(se$`RoRR <2 m`)), grupo = "Infants <2 months"))
setnames(s1d, c("V1", "V2", "V3"), c("est", "lo", "hi"))
en_an <- c("Main (all confirmed; 2015–2019 baseline; NB, year fixed effects)", "Laboratory-confirmed only", "Probable duplicates removed",
           "Year of symptom onset", "Baseline 2014–2019", "Baseline 2017–2019", "Excluding Antioquia", "Quasi-Poisson", "Reference group 6–11 months")
s1d[, an := factor(en_an[match(analisis, unique(se$analisis))], levels = rev(en_an))]
pS1 <- ggplot(s1d, aes(est, an)) +
  geom_vline(xintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = s2, orientation = "y") +
  geom_point(size = 1.8, shape = 21, fill = surface, colour = s2, stroke = 0.7) +
  facet_wrap(~grupo) + scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = label_number(drop0trailing = TRUE)) +
  labs(x = "Ratio of ratios, 2025 vs 2011–2013 (log scale, 95% CI)", y = NULL,
       title = "Sensitivity analyses for the relative protection of young infants in 2025") + theme_forest()
save_fig(pS1, "FiguraS1_sensibilidad_RoRR", 180, 80)

qc <- fread("03_resultados/calidad_confirmacion_por_anio.csv")
pS2a <- ggplot(qc, aes(factor(anio), pct_laboratorio / 100)) + geom_col(width = 0.7, fill = s1) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Laboratory-confirmed (%)", tag = "A", title = "Share of cases confirmed by laboratory") +
  theme_pub() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
tw <- melt(fread("03_resultados/calidad_cobertura_terridata_vs_wuenic.csv")[, .(anio, `TerriData municipal median` = mediana_municipal,
                                                                              `WUENIC national DTP3` = wuenic_dtp3)], id.vars = "anio")
pS2b <- ggplot(tw, aes(anio, value, colour = variable)) +
  annotate("rect", xmin = 2020.5, xmax = 2025.5, ymin = -Inf, ymax = Inf, fill = shade) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.4, shape = 21, fill = surface, stroke = 0.6) +
  annotate("text", x = 2020.7, y = 103, label = "Series break in\nmunicipal data", hjust = 0, size = 2.2, colour = ink2, family = FONT) +
  scale_colour_manual(values = c(s1, s2), name = NULL) + scale_y_continuous(limits = c(40, 106)) +
  labs(x = NULL, y = "Pentavalent / DTP3 coverage (%)", tag = "B", title = "Municipal pentavalent coverage vs national estimates") +
  theme_pub() + theme(legend.position = "top", legend.justification = "left")
save_fig(pS2a | pS2b, "FiguraS2_calidad_datos", 180, 75)

ap <- readRDS("01_datos/procesados/apc_inla.rds")$coh
pS3 <- ggplot(ap, aes(cohorte, exp(dev))) +
  geom_hline(yintercept = 1, colour = base_col, linewidth = 0.3) +
  geom_ribbon(aes(ymin = exp(lo), ymax = exp(hi)), fill = wash) + geom_line(colour = s1, linewidth = 0.5) +
  geom_vline(xintercept = c(2018.5, 2022.5), colour = ink3, linewidth = 0.3) +
  annotate("text", x = 2018.3, y = 1.6, label = "Pandemic-era\ncohorts", hjust = 1, size = 2.3, colour = ink2, family = FONT, lineheight = 0.9) +
  scale_y_log10() + labs(x = "Birth cohort", y = "Detrended cohort effect (rate ratio, log scale)",
                         title = "Bayesian age-period-cohort model (ages 0–19, 2007–2025): cohort curvature",
                         subtitle = "Vertical lines bound the pandemic-era cohorts (2019–2022). Linear trends are not identifiable and were removed.") + theme_pub()
save_fig(pS3, "FiguraS3_APC_cohortes", 150, 80)

tm <- fread("03_resultados/cronologia_2025_mediana_inicio.csv")
tm[, grp := factor(grp, levels = rev(c("<2 months", "2–11 months", "1–9 years", "10–19 years", "20+ years")))]
pS4 <- ggplot(tm, aes(as.Date(mediana), grp)) +
  geom_errorbar(aes(xmin = as.Date(lo), xmax = as.Date(hi)), width = 0, linewidth = 0.45, colour = s1, orientation = "y") +
  geom_point(size = 1.9, shape = 21, fill = surface, colour = s1, stroke = 0.7) +
  geom_text(aes(x = as.Date("2025-09-12"), label = paste0("n = ", n)), hjust = 0, size = 2.2, colour = ink2, family = FONT) +
  scale_x_date(date_labels = "%b %Y", limits = as.Date(c("2025-05-15", "2025-10-01"))) +
  labs(x = "Median date of symptom onset (95% bootstrap CI)", y = NULL, title = "Timing of the 2025 epidemic by age group") + theme_forest()
save_fig(pS4, "FiguraS4_cronologia_2025", 140, 60)
cat("Figuras v2 generadas.\n")
