# =============================================================================
# 00_tema_figuras.R — tokens de diseño, tema y guardado común de figuras.
# Paleta validada (instancia de referencia): categórica azul/naranja/aqua,
# rampa ordinal azul de 5 pasos, divergente azul<->rojo con punto medio gris.
# =============================================================================
suppressMessages({ library(ggplot2); library(patchwork); library(scales); library(svglite); library(ragg) })
ink1 <- "#0b0b0b"; ink2 <- "#52514e"; ink3 <- "#898781"
grid_col <- "#e1e0d9"; base_col <- "#c3c2b7"; surface <- "#ffffff"; wash <- "#dbe8f9"; shade <- "#f4f4f2"
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
          strip.text = element_text(face = "bold", size = base, colour = ink1, hjust = 0),
          plot.background = element_rect(fill = surface, colour = NA),
          panel.background = element_rect(fill = surface, colour = NA),
          plot.margin = margin(4, 6, 4, 4))
}
theme_forest <- function(base = 8) theme_pub(base) +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = grid_col, linewidth = 0.25))

# PDF con Quartz (incrusta Arial); EPS con PostScript (métricas Helvetica bajo "Arial", WinAnsi)
postscriptFonts(Arial = postscriptFonts()$Helvetica)
dev_pdf <- function(filename, width, height, ...) grDevices::quartz(file = filename, type = "pdf", width = width, height = height, bg = surface)
dev_eps <- function(filename, width, height, ...) grDevices::postscript(filename, width = width, height = height,
  horizontal = FALSE, onefile = FALSE, paper = "special", family = "Arial", encoding = "WinAnsi.enc", colormodel = "srgb")
save_fig <- function(p, name, w_mm, h_mm, eps = TRUE) {
  f <- file.path("04_figuras", name)
  ggsave(paste0(f, ".pdf"), p, width = w_mm, height = h_mm, units = "mm", device = dev_pdf)
  ggsave(paste0(f, ".svg"), p, width = w_mm, height = h_mm, units = "mm", device = svglite::svglite,
         bg = surface, system_fonts = list(sans = FONT, Arial = FONT))
  if (eps) ggsave(paste0(f, ".eps"), p, width = w_mm, height = h_mm, units = "mm", device = dev_eps)
  ggsave(paste0(f, ".tiff"), p, width = w_mm, height = h_mm, units = "mm", device = ragg::agg_tiff,
         dpi = 600, compression = "lzw", bg = surface)
  ggsave(paste0(f, "_preview.png"), p, width = w_mm, height = h_mm, units = "mm", device = ragg::agg_png,
         dpi = 300, bg = surface)
  invisible(f)
}
# "0.33 (0.12–0.88)" -> c(est, lo, hi)
parse_ci <- function(s) { m <- regmatches(s, regexec("([0-9.]+) \\(([0-9.]+)[–-]([0-9.]+)\\)", s))
  t(vapply(m, function(z) as.numeric(z[2:4]), numeric(3))) }
