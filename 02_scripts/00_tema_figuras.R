# =============================================================================
# 00_tema_figuras.R: colores, tema y guardado común de las figuras (v3, 12-11-2026).
# Reglas de Frontiers: 180 mm de ancho, texto de al menos 8 pt, RGB, paneles (A), (B).
# Reglas de Pedro: Arial, todo el texto en negro, azul #002060 y verde #00A39A,
# etiquetas directas en vez de leyendas y ninguna línea vertical de cuadrícula.
# Paletas comprobadas con el validador de la skill dataviz (validate_palette.py, superficie
# blanca): la categórica pasa CVD y visión normal; el azul #002060 queda fuera de la banda de
# luminosidad, a propósito, porque es el color de Pedro. Las rampas pasan la prueba ordinal.
# =============================================================================
suppressMessages({ library(ggplot2); library(patchwork); library(scales); library(svglite); library(ragg) })
ink <- "#000000"                                    # todo el texto
line_col <- "#7F7F7F"; grid_col <- "#D9D9D9"; surface <- "#ffffff"
s1 <- "#002060"; s2 <- "#00A39A"; s3 <- "#C55A11"   # azul, verde y naranja (series)
wash <- "#DFEAFD"; wash2 <- "#CFF0EC"; shade <- "#EFEFEF"   # rellenos claros: azul, verde y gris
ramp5 <- c("#96B7EF", "#638EDC", "#3165C5", "#0B3F9C", "#002060")
seq_blue <- c("#96B7EF", "#739BE2", "#5180D4", "#2F64C3", "#174AA7", "#023287", "#002060")
div_cols <- c("#002060", "#1548A6", "#4B7BD1", "#98B8F0", "#F2F2F2", "#E0A98E", "#C55A11", "#803704", "#4B2005")
zero_fill <- "#EDEDED"
# Colores fijos por periodo en todas las figuras
per_cols <- c(`2011–2013` = s1, `2014–2019` = s3, `2025` = s2)
FONT <- "Arial"
TXT <- 8 / .pt                                      # tamaño de geom_text equivalente a 8 pt

theme_pub <- function(base = 8) {
  theme_minimal(base_size = base, base_family = FONT) +
    theme(text = element_text(colour = ink, size = base),
          axis.text = element_text(colour = ink, size = base),
          axis.title = element_text(colour = ink, size = base),
          panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.25),
          axis.line.x = element_line(colour = line_col, linewidth = 0.3),
          axis.ticks = element_line(colour = line_col, linewidth = 0.25),
          axis.ticks.length = unit(2, "pt"),
          legend.position = "none",                 # etiquetas directas en lugar de leyendas
          legend.title = element_text(colour = ink, size = base),
          legend.text = element_text(colour = ink, size = base),
          plot.title = element_text(face = "bold", size = base + 0.5, colour = ink, margin = margin(b = 3)),
          plot.subtitle = element_text(colour = ink, size = base),
          plot.tag = element_text(face = "bold", size = base + 1, colour = ink),
          plot.title.position = "plot",
          strip.text = element_text(face = "bold", size = base, colour = ink, hjust = 0),
          plot.background = element_rect(fill = surface, colour = NA),
          panel.background = element_rect(fill = surface, colour = NA),
          plot.margin = margin(4, 6, 4, 4))
}
# Gráficos de bosque: sin cuadrícula; solo la línea de referencia en 1
theme_forest <- function(base = 8) theme_pub(base) + theme(panel.grid.major.y = element_blank())

# Separa etiquetas directas que se montan: y ordenadas, distancia mínima d (unidades de datos)
spread_labels <- function(y, d, lo = -Inf, hi = Inf) {
  o <- order(y); z <- y[o]
  for (k in 1:100) {
    moved <- FALSE
    for (i in seq_along(z)[-1]) if (z[i] - z[i - 1] < d) { m <- (z[i] + z[i - 1]) / 2; z[i - 1] <- m - d / 2; z[i] <- m + d / 2; moved <- TRUE }
    z <- pmin(pmax(z, lo), hi); if (!moved) break
  }
  y[o] <- z; y
}

# PDF con Quartz (incrusta Arial); SVG con svglite; EPS con rsvg desde el SVG (texto en contornos,
# receta del kit); TIFF a 600 ppp con LZW y vista previa PNG a 300 ppp con ragg.
FIG_DIR <- getOption("fig.dir", "04_figuras")
dev_pdf <- function(filename, width, height, ...) grDevices::quartz(file = filename, type = "pdf", width = width, height = height, bg = surface)
save_fig <- function(p, name, w_mm, h_mm, eps = TRUE) {
  f <- file.path(FIG_DIR, name)
  ggsave(paste0(f, ".pdf"), p, width = w_mm, height = h_mm, units = "mm", device = dev_pdf)
  ggsave(paste0(f, ".svg"), p, width = w_mm, height = h_mm, units = "mm", device = svglite::svglite,
         bg = surface, system_fonts = list(sans = FONT, Arial = FONT))
  if (eps) rsvg::rsvg_eps(paste0(f, ".svg"), paste0(f, ".eps"))
  ggsave(paste0(f, ".tiff"), p, width = w_mm, height = h_mm, units = "mm", device = ragg::agg_tiff,
         dpi = 600, compression = "lzw", bg = surface)
  ggsave(paste0(f, "_preview.png"), p, width = w_mm, height = h_mm, units = "mm", device = ragg::agg_png,
         dpi = 300, bg = surface)
  invisible(f)
}
# "0.33 (0.12–0.88)" -> c(est, lo, hi)
parse_ci <- function(s) { m <- regmatches(s, regexec("([0-9.]+) \\(([0-9.]+)[–-]([0-9.]+)\\)", s))
  t(vapply(m, function(z) as.numeric(z[2:4]), numeric(3))) }
