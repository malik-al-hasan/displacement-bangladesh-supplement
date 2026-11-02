# =============================================================================
# 10_figures_revision.R  -  figures for the revised analysis
#   Fig 4  Adjusted prevalence ratios, pooled and by district (primary model and
#          singly added correlates)
#   Fig 5  Model-standardised probabilities by district
#   Fig S1 Discrimination and calibration of the primary model
#   Fig S2 Key primary-model estimates across specifications
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(patchwork); library(tidyr) })
R <- "outputs/revision"; F <- "outputs/figures"
col_d <- c(Pooled = "#4D4D4D", Barishal = "#0072B2", Kurigram = "#D55E00")
shp_d <- c(Pooled = 16, Barishal = 17, Kurigram = 15)
theme_pub <- function(b = 8) theme_classic(base_size = b, base_family = "sans") +
  theme(axis.line = element_line(linewidth = .35, colour = "grey25"), axis.ticks = element_line(linewidth = .35),
        strip.background = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
        legend.position = "top", legend.title = element_blank(),
        panel.grid.major.x = element_line(linewidth = .2, colour = "grey88"))
save_fig <- function(p, n, w, h) {
  ggsave(file.path(F, paste0(n, ".png")), p, width = w, height = h, units = "mm", dpi = 600, bg = "white")
  ggsave(file.path(F, paste0(n, ".pdf")), p, width = w, height = h, units = "mm", bg = "white", device = cairo_pdf)
}

# ---- Fig 4 -------------------------------------------------------------------------
prim <- read.csv(file.path(R, "Table_primary_model_pooled.csv")) %>% transmute(term, PR = aPR, lo = aPR_lo, hi = aPR_hi, sample = "Pooled")
strat <- read.csv(file.path(R, "Table_primary_model_by_district.csv")) %>% transmute(term, PR = aPR, lo = aPR_lo, hi = aPR_hi, sample = district)
corr <- read.csv(file.path(R, "Table_correlates_aPR.csv")) %>% transmute(term, PR, lo, hi, sample)
het <- read.csv(file.path(R, "Table_heterogeneity_tests.csv"))
lab <- tribble(~term, ~label, ~var, ~section,
  "genderFemale", "Woman (ref. man)", "gender", "Primary model",
  "age36-65", "Age 36–65 (ref. 18–35)", "age", "Primary model",
  "age65 or older", "Age >65 (ref. 18–35)", "age", "Primary model",
  "edu3Below primary", "Below primary (ref. no education)", "edu3", "Primary model",
  "edu3Secondary or above", "Secondary or above (ref. no education)", "edu3", "Primary model",
  "rapid", "Rapid-onset hazard count, per item", "rapid", "Primary model",
  "slow", "Slow-onset hazard count, per item", "slow", "Primary model",
  "imp_rapid3Recoverable", "Rapid-onset impact recoverable (ref. none/minor)", "imp_rapid3", "Primary model",
  "imp_rapid3Irrecoverable", "Rapid-onset impact irrecoverable (ref. none/minor)", "imp_rapid3", "Primary model",
  "imp_slow3Recoverable", "Slow-onset impact recoverable (ref. none/minor)", "imp_slow3", "Primary model",
  "imp_slow3Irrecoverable", "Slow-onset impact irrecoverable (ref. none/minor)", "imp_slow3", "Primary model",
  "neld", "NELD score, per item", "neld", "Correlates (added singly)",
  "wdmYes", "Women hold decision-making power (ref. no)", "wdm", "Correlates (added singly)",
  "exposure", "Exposure of current location, per item", "exposure", "Correlates (added singly)",
  "resAbove 20 years", "Residence >20 y (ref. ≤5 y)", "res", "Correlates (added singly)",
  "support4Microcredit", "Microcredit (ref. in-kind relief)", "support4", "Correlates (added singly)",
  "occ2Other", "Non-manual occupation (ref. manual)", "occ2", "Correlates (added singly)")
fd <- bind_rows(prim, strat, corr) %>% inner_join(lab, by = "term") %>%
  left_join(het %>% select(variable, p), by = c("var" = "variable")) %>%
  mutate(sample = factor(sample, levels = c("Pooled", "Barishal", "Kurigram")),
         label = factor(label, levels = rev(lab$label)),
         section = factor(section, levels = c("Primary model", "Correlates (added singly)")),
         plab = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
first <- lab %>% group_by(var) %>% slice(1) %>% pull(label)
ptxt <- fd %>% distinct(label, section, plab) %>% filter(label %in% first)
p4 <- ggplot(fd, aes(PR, label, colour = sample, shape = sample)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45", linewidth = .35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = .45, orientation = "y", position = position_dodge(.65)) +
  geom_point(size = 1.7, position = position_dodge(.65)) +
  geom_text(data = ptxt, aes(x = 3.3, y = label, label = plab), inherit.aes = FALSE, size = 2.3, hjust = 0, colour = "grey20") +
  facet_grid(section ~ ., scales = "free_y", space = "free_y") +
  scale_x_log10(limits = c(0.22, 4.6), breaks = c(0.25, 0.5, 1, 2)) +
  scale_colour_manual(values = col_d) + scale_shape_manual(values = shp_d) +
  labs(x = "Adjusted prevalence ratio (log scale) with 95% CI;  right column: p for district interaction", y = NULL) +
  theme_pub(7.5) + theme(strip.text.y = element_text(angle = 0, hjust = 0), panel.spacing = unit(3, "mm"))
save_fig(p4, "Fig4_prevalence_ratios_pooled_vs_district", 174, 125)

# ---- Fig 5 -------------------------------------------------------------------------
g <- read.csv(file.path(R, "FigData_gcomp_revision.csv"), na.strings = character(0))
pan <- c(slow = "a  Slow-onset hazard count", imp_rapid3 = "b  Rapid-onset impact", imp_slow3 = "c  Slow-onset impact",
         neld = "d  NELD score†", wdm = "e  Women's decision-making†", exposure = "f  Current-location exposure†",
         res = "g  Residence duration†", support4 = "h  Support received†")
ll <- c("None or minor" = "Minor", "Recoverable" = "Recov.", "Irrecoverable" = "Irrecov.", "Not sure" = "Unsure",
        "Up to 5 years" = "≤5 y", "6-20 years" = "6–20 y", "Above 20 years" = ">20 y",
        "In-kind relief" = "Relief", "Microcredit" = "Credit", "Training or other" = "Train/oth.")
plots <- lapply(seq_along(pan), function(i) {
  v <- names(pan)[i]
  s <- g %>% filter(variable == v) %>% mutate(lab = ifelse(level %in% names(ll), ll[level], level))
  s$lab <- factor(s$lab, levels = unique(s$lab))
  ggplot(s, aes(lab, 100 * prob, colour = district, shape = district, group = district)) +
    geom_line(linewidth = .45, position = position_dodge(.3)) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0, linewidth = .45, position = position_dodge(.3)) +
    geom_point(size = 1.7, position = position_dodge(.3)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    scale_colour_manual(values = col_d[2:3]) + scale_shape_manual(values = shp_d[2:3]) +
    labs(title = pan[[v]], x = NULL, y = if (i %in% c(1, 5)) "Standardised probability (%)" else NULL) +
    theme_pub(7.2) + theme(plot.title = element_text(size = 7.2, face = "bold"), axis.text.x = element_text(size = 6.2),
                           panel.grid.major.y = element_line(linewidth = .2, colour = "grey90"), panel.grid.major.x = element_blank())
})
p5 <- wrap_plots(plots, ncol = 4, guides = "collect") & theme(legend.position = "bottom")
save_fig(p5, "Fig5_standardised_probabilities", 174, 112)

# ---- Fig S1 ------------------------------------------------------------------------
roc <- read.csv(file.path(R, "FigData_roc_M1.csv")); cal <- read.csv(file.path(R, "FigData_calibration_M1.csv"))
pf <- read.csv(file.path(R, "Table_performance_revision.csv"))
pa <- ggplot(roc, aes(fpr, tpr)) + geom_abline(linetype = "dashed", colour = "grey55", linewidth = .35) +
  geom_step(direction = "vh", linewidth = .55, colour = "#4D4D4D") +
  annotate("text", x = .32, y = .08, hjust = 0, size = 2.4,
           label = sprintf("Apparent AUC %.3f\nOptimism-corrected AUC %.3f\nWith district interactions: %.3f (corrected)",
                           pf$AUC[1], pf$AUC_corrected[1], pf$AUC_corrected[2])) +
  coord_equal() + labs(x = "1 - specificity", y = "Sensitivity", tag = "a") + theme_pub()
cl <- cal %>% pivot_longer(c(apparent, corrected), names_to = "type", values_to = "obs") %>%
  mutate(type = recode(type, apparent = "Apparent", corrected = "Bootstrap bias-corrected"))
pb <- ggplot(cl, aes(predicted, obs, linetype = type)) + geom_abline(colour = "grey55", linewidth = .35) +
  geom_line(linewidth = .55, colour = "#4D4D4D") +
  annotate("text", x = .05, y = .93, hjust = 0, size = 2.4,
           label = sprintf("Calibration slope, optimism-corrected: %.2f", pf$slope_corrected[1])) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + labs(x = "Predicted probability", y = "Observed proportion", tag = "b") +
  theme_pub() + theme(legend.position = c(.68, .12))
save_fig(pa + pb, "FigS1_roc_calibration", 174, 85)

# ---- Fig S2 ------------------------------------------------------------------------
se <- read.csv(file.path(R, "TableS_sensitivity_primary.csv"))
tl <- c(districtKurigram = "Kurigram (ref. Barishal)", slow = "Slow-onset hazard count", rapid = "Rapid-onset hazard count",
        imp_slow3Irrecoverable = "Slow-onset impact irrecoverable", imp_rapid3Irrecoverable = "Rapid-onset impact irrecoverable",
        sudden = "Sudden-onset count (alternative coding)", gradual = "Gradual-onset count (alternative coding)")
ss <- se %>% filter(term %in% names(tl)) %>%
  mutate(term = factor(tl[term], levels = tl), spec = factor(spec, levels = rev(unique(se$spec))),
         sig = ifelse(lo > 1 | hi < 1, "95% CI excludes 1", "95% CI includes 1"))
ps2 <- ggplot(ss, aes(PR, spec, shape = sig)) + geom_vline(xintercept = 1, linetype = "dashed", colour = "grey45", linewidth = .35) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = .45, colour = "#4D4D4D", orientation = "y") +
  geom_point(size = 1.8, colour = "#4D4D4D", fill = "white") +
  scale_shape_manual(values = c("95% CI excludes 1" = 16, "95% CI includes 1" = 21)) +
  scale_x_log10() + facet_wrap(~term, ncol = 2, scales = "free_x") +
  labs(x = "Adjusted prevalence ratio (log scale)", y = NULL) + theme_pub(7)
save_fig(ps2, "FigS2_robustness_specifications", 174, 130)
cat("revision figures written\n")
