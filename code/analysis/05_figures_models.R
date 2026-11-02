# =============================================================================
# 05_figures_models.R
# Publication figures from the regression outputs.
#   Fig 4  Pooled and district-specific adjusted odds ratios (Firth)
#   Fig 5  Model-standardised probability of displacement by district
#   Fig S1 Discrimination (ROC) and bootstrap-corrected calibration
#   Fig S2 Robustness of principal estimates across nine specifications
# Okabe-Ito colours with redundant shape coding so figures survive greyscale.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(patchwork); library(tidyr) })
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

col_d   <- c(Pooled = "#4D4D4D", Barishal = "#0072B2", Kurigram = "#D55E00")
shape_d <- c(Pooled = 16, Barishal = 17, Kurigram = 15)
theme_pub <- function(base = 8.5) {
  theme_classic(base_size = base, base_family = "sans") +
    theme(axis.line = element_line(linewidth = 0.35, colour = "grey25"),
          axis.ticks = element_line(linewidth = 0.35, colour = "grey25"),
          axis.text = element_text(colour = "grey15"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0, size = base),
          legend.position = "top", legend.title = element_blank(),
          legend.key.height = unit(3, "mm"),
          panel.grid.major.x = element_line(linewidth = 0.2, colour = "grey88"),
          plot.title = element_text(face = "bold", size = base + 1),
          plot.tag = element_text(face = "bold"))
}
save_fig <- function(p, name, w, h) {
  ggsave(file.path("outputs/figures", paste0(name, ".png")), p, width = w, height = h,
         units = "mm", dpi = 600, bg = "white")
  ggsave(file.path("outputs/figures", paste0(name, ".pdf")), p, width = w, height = h,
         units = "mm", bg = "white", device = cairo_pdf)
}

# -----------------------------------------------------------------------------
# Fig 4: forest plot
# -----------------------------------------------------------------------------
fd  <- read.csv("outputs/tables/FigData_forest.csv")
het <- read.csv("outputs/tables/Table_heterogeneity_by_predictor.csv")
keep <- tribble(
  ~term,                      ~label,                                                  ~group,
  "res6-20 years",            "Residence 6–20 y (ref. ≤5 y)",                          "res",
  "resAbove 20 years",        "Residence >20 y (ref. ≤5 y)",                            "res",
  "wdmNot sure",              "Women's decision-making: not sure (ref. no)",           "wdm",
  "wdmYes",                   "Women's decision-making: yes (ref. no)",                "wdm",
  "exposure",                 "Exposure of current location (per item)",               "exposure",
  "rapid",                    "Rapid-onset hazard score (per item)",                   "rapid",
  "slow",                     "Slow-onset hazard score (per item)",                    "slow",
  "imp_rapid3Irrecoverable",  "Rapid-onset impact irrecoverable (ref. none/minor)",    "imp_rapid3",
  "imp_slow3Irrecoverable",   "Slow-onset impact irrecoverable (ref. none/minor)",     "imp_slow3",
  "neld",                     "NELD score (per item)",                                 "neld",
  "supportImmediate Relief",  "Immediate relief (ref. food assistance)",               "support",
  "supportMicro-credit",      "Microcredit (ref. food assistance)",                    "support")
group_lab <- c(res = "Residence duration", wdm = "Women's decision-making",
               exposure = "Exposure of current location", rapid = "Hazard experience",
               slow = "Hazard experience", imp_rapid3 = "Perceived impact severity",
               imp_slow3 = "Perceived impact severity", neld = "Non-economic loss",
               support = "Support type")
fp <- fd %>% inner_join(keep, by = "term") %>%
  left_join(het %>% select(variable, p_het), by = c("group" = "variable")) %>%
  mutate(district = factor(district, levels = c("Pooled", "Barishal", "Kurigram")),
         label = factor(label, levels = rev(keep$label)),
         lo_c = pmax(lo, 0.02), hi_c = pmin(hi, 30),
         trunc_hi = hi > 50, trunc_lo = lo < 0.02,
         het_lab = ifelse(p_het < 0.001, "p<0.001", sprintf("p=%.3f", p_het)))
# heterogeneity p shown once per variable (on its first displayed level)
first_lab <- keep %>% group_by(group) %>% slice(1) %>% pull(label)
het_txt <- fp %>% distinct(label, het_lab) %>% filter(label %in% first_lab)

p4 <- ggplot(fp, aes(x = OR, y = label, colour = district, shape = district)) +
  geom_vline(xintercept = 1, linewidth = 0.35, colour = "grey40", linetype = "dashed") +
  geom_errorbar(aes(xmin = lo_c, xmax = hi_c), width = 0, linewidth = 0.45, orientation = "y",
                 position = position_dodge(width = 0.65)) +
  geom_point(size = 1.9, position = position_dodge(width = 0.65)) +
  geom_text(data = het_txt, aes(x = 45, y = label, label = het_lab), inherit.aes = FALSE,
            size = 2.5, hjust = 0, colour = "grey20") +
  annotate("text", x = 45, y = length(keep$label) + 0.85, label = "p (district
interaction)",
           size = 2.5, hjust = 0, fontface = "bold", colour = "grey20") +
  scale_x_log10(limits = c(0.02, 140), breaks = c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 10, 25),
                labels = c("0.05", "0.1", "0.25", "0.5", "1", "2", "4", "10", "25")) +
  coord_cartesian(clip = "off", ylim = c(1, length(keep$label) + 0.6)) +
  scale_colour_manual(values = col_d) + scale_shape_manual(values = shape_d) +
  labs(x = "Adjusted odds ratio (log scale), Firth penalised likelihood, 95% CI", y = NULL) +
  theme_pub() + theme(plot.margin = margin(4, 4, 4, 4))
save_fig(p4, "Fig4_forest_pooled_vs_district", 174, 120)

# -----------------------------------------------------------------------------
# Fig 5: g-computation standardised probabilities by district
# -----------------------------------------------------------------------------
g <- read.csv("outputs/tables/FigData_gcomp_by_district.csv")
panel <- c(res = "a  Residence duration", wdm = "b  Women's decision-making",
           exposure = "c  Exposure of location", slow = "d  Slow-onset hazards",
           imp_rapid3 = "e  Rapid-onset impact", imp_slow3 = "f  Slow-onset impact",
           neld = "g  NELD score", support = "h  Support type")
lev_lab <- c("Up to 5 years" = "≤5 y", "6-20 years" = "6–20 y", "Above 20 years" = ">20 y",
             "No" = "No", "Not sure" = "Unsure", "Yes" = "Yes",
             "None or minor" = "Minor", "Recoverable" = "Recov.", "Irrecoverable" = "Irrecov.",
             "Food assistance" = "Food", "Immediate Relief" = "Relief", "Micro-credit" = "Credit", "None" = "None")
g5 <- g %>% filter(variable %in% names(panel)) %>%
  mutate(panel = factor(panel[variable], levels = panel),
         lab = ifelse(level %in% names(lev_lab), lev_lab[level], level),
         district = factor(district, levels = c("Barishal", "Kurigram")))
g5 <- g5 %>% group_by(variable) %>%
  mutate(lab = factor(lab, levels = unique(lab[order(match(level, unique(level)))]))) %>% ungroup()
plots <- lapply(seq_along(levels(g5$panel)), function(i) {
  pn <- levels(g5$panel)[i]
  s <- g5 %>% filter(panel == pn) %>% droplevels()
  ggplot(s, aes(x = lab, y = 100 * prob, colour = district, shape = district, group = district)) +
    geom_line(linewidth = 0.45, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0, linewidth = 0.45,
                  position = position_dodge(width = 0.3)) +
    geom_point(size = 1.8, position = position_dodge(width = 0.3)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    scale_colour_manual(values = col_d[c("Barishal", "Kurigram")]) +
    scale_shape_manual(values = shape_d[c("Barishal", "Kurigram")]) +
    labs(title = pn, x = NULL, y = if (i %in% c(1, 5)) "Displaced, standardised (%)" else NULL) +
    theme_pub(7.5) + theme(plot.title = element_text(size = 7.5, face = "bold"),
                           panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey90"),
                           panel.grid.major.x = element_blank(),
                           axis.text.x = element_text(size = 6.5))
})
p5 <- wrap_plots(plots, ncol = 4, guides = "collect") & theme(legend.position = "bottom")
save_fig(p5, "Fig5_standardised_probabilities", 174, 112)

# -----------------------------------------------------------------------------
# Fig S1: ROC and calibration
# -----------------------------------------------------------------------------
roc <- read.csv("outputs/tables/FigData_roc.csv")
cal <- read.csv("outputs/tables/FigData_calibration.csv")
perf <- read.csv("outputs/tables/Table_performance_pooled.csv")
pa <- ggplot(roc, aes(fpr, tpr)) +
  geom_abline(linetype = "dashed", colour = "grey55", linewidth = 0.35) +
  geom_step(direction = "vh", linewidth = 0.55, colour = "#4D4D4D") +
  annotate("text", x = 0.30, y = 0.10, hjust = 0, size = 2.4,
           label = sprintf("Apparent AUC %.3f (%.3f-%.3f)\nOptimism-corrected AUC %.3f",
                           perf$AUC, perf$AUC_lo, perf$AUC_hi, perf$AUC_optimism_corrected)) +
  coord_equal() + labs(x = "1 - specificity", y = "Sensitivity", tag = "a") + theme_pub()
cl <- cal %>% pivot_longer(c(apparent, corrected), names_to = "type", values_to = "obs") %>%
  mutate(type = recode(type, apparent = "Apparent", corrected = "Bootstrap bias-corrected"))
pb <- ggplot(cl, aes(predicted, obs, linetype = type)) +
  geom_abline(colour = "grey55", linewidth = 0.35) +
  geom_line(linewidth = 0.55, colour = "#4D4D4D") +
  annotate("text", x = 0.05, y = 0.93, hjust = 0, size = 2.6,
           label = sprintf("Calibration slope (corrected) %.2f\nHosmer-Lemeshow p = %.3f",
                           perf$CalSlope_corrected, perf$HL_p)) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Predicted probability", y = "Observed proportion", tag = "b") +
  theme_pub() + theme(legend.position = c(0.68, 0.12))
save_fig(pa + pb, "FigS1_roc_calibration", 174, 85)

# -----------------------------------------------------------------------------
# Fig S2: robustness across specifications
# -----------------------------------------------------------------------------
sens <- read.csv("outputs/tables/TableS_sensitivity_key_estimates.csv")
term_lab <- c("resAbove 20 years" = "Residence >20 y", "wdmYes" = "Women's decision-making: yes",
              "districtKurigram" = "Kurigram (vs Barishal)", "exposure" = "Exposure score",
              "neld" = "NELD score", "supportMicro-credit" = "Microcredit")
ss <- sens %>% filter(term %in% names(term_lab)) %>%
  mutate(term = factor(term_lab[term], levels = term_lab),
         spec = factor(spec, levels = rev(unique(sens$spec))),
         sig = ifelse(is.na(p), lo > 1 | hi < 1, p < 0.05),
         sig = ifelse(sig, "95% CI excludes 1", "95% CI includes 1"))
ps2 <- ggplot(ss, aes(OR, spec, shape = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45, colour = "#4D4D4D", orientation = "y") +
  geom_point(size = 1.8, colour = "#4D4D4D", fill = "white") +
  scale_shape_manual(values = c("95% CI excludes 1" = 16, "95% CI includes 1" = 21)) +
  scale_x_log10() + facet_wrap(~term, ncol = 3, scales = "free_x") +
  labs(x = "Adjusted odds ratio (log scale)", y = NULL) + theme_pub(7.5)
save_fig(ps2, "FigS2_robustness_specifications", 174, 110)
cat("Figures written.\n")
