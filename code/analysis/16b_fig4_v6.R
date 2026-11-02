# =============================================================================
# 16b_fig4_v6.R  -  Fig. 4 (v6): model-standardised probability of reported displacement
# for the variables named in the hypotheses (slow-onset hazard count, impact severity,
# current-location exposure), by district. Data: outputs/v4/FigData_gcomp_revision.csv.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(patchwork) })
F <- "outputs/figures_v6"; dir.create(F, showWarnings = FALSE)
g <- read.csv("outputs/v4/FigData_gcomp_revision.csv", na.strings = character(0))
col_d <- c(Barishal = "#8C510A", Kurigram = "#1A1A1A"); shp_d <- c(Barishal = 17, Kurigram = 15)
theme_pub <- function(b = 8) theme_classic(base_size = b, base_family = "sans") +
  theme(axis.line = element_line(linewidth = .35, colour = "grey25"), axis.ticks = element_line(linewidth = .35),
        axis.text = element_text(size = b, colour = "grey10"), legend.position = "bottom", legend.title = element_blank(),
        panel.grid.major.y = element_line(linewidth = .2, colour = "grey90"),
        plot.title = element_text(size = 8.5, face = "bold"))
pan <- c(slow = "a  Slow-onset hazards reported (count)", imp_rapid3 = "b  Rapid-onset impact rated",
         imp_slow3 = "c  Slow-onset impact rated", exposure = "d  Exposure of current location (count)")
ll <- c("None or minor" = "None or minor", "Recoverable" = "Recoverable", "Irrecoverable" = "Irrecoverable")
plots <- lapply(seq_along(pan), function(i) {
  v <- names(pan)[i]
  s <- g %>% filter(variable == v) %>% mutate(lab = ifelse(level %in% names(ll), ll[level], level))
  s$lab <- factor(s$lab, levels = unique(s$lab))
  ggplot(s, aes(lab, 100 * prob, colour = district, shape = district, group = district)) +
    geom_line(linewidth = .45, position = position_dodge(.3)) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0, linewidth = .45, position = position_dodge(.3)) +
    geom_point(size = 1.9, position = position_dodge(.3)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    scale_colour_manual(values = col_d) + scale_shape_manual(values = shp_d) +
    labs(title = pan[[v]], x = NULL, y = if (i %in% c(1, 3)) "Standardised probability of displacement (%)" else NULL) +
    theme_pub(8.5)
})
p <- wrap_plots(plots, ncol = 2, guides = "collect") & theme(legend.position = "bottom")
ggsave(file.path(F, "Fig4_standardised_probabilities.png"), p, width = 174, height = 140, units = "mm", dpi = 600, bg = "white")
ggsave(file.path(F, "Fig4_standardised_probabilities.pdf"), p, width = 174, height = 140, units = "mm", bg = "white", device = cairo_pdf)
cat("Fig 4 written\n")
