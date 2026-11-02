# =============================================================================
# 02_heterogeneity_models.R
# Does Bangladesh have one displacement mechanism or two?
#  (a) global test: main-effects model vs model with every predictor x district
#  (b) predictor-specific heterogeneity tests (one interaction at a time)
#  (c) district-stratified Firth models
#  (d) model-standardised (g-computation) predicted probabilities by district
#      with bootstrap confidence intervals
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(logistf); library(pROC) })
set.seed(20250102)                        # last day of fieldwork
d <- readRDS("outputs/analysis_data.rds")

# impact severity enters in three levels (none/minor, recoverable, irrecoverable);
# the five-level coding leaves 2-3 respondents in the Kurigram reference cell
covs_nd <- c("gender", "age", "edu", "occ", "res", "wdm", "exposure", "rapid", "slow",
             "neld", "imp_rapid3", "imp_slow3", "support", "provider", "autonomy")
f_main <- reformulate(c(covs_nd, "district"), "displaced")
m_main <- glm(f_main, binomial, d)

# --- (a) global heterogeneity test ------------------------------------------
f_int  <- reformulate(c("district", covs_nd, paste0("district:", covs_nd)), "displaced")
m_int  <- suppressWarnings(glm(f_int, binomial, d))
lr_ml  <- anova(m_main, m_int, test = "LRT")
fm_main <- logistf(f_main, data = d, pl = FALSE)
fm_int  <- logistf(f_int,  data = d, pl = FALSE)
pen_lr  <- 2 * (fm_int$loglik["full"] - fm_main$loglik["full"])
pen_df  <- fm_int$df - fm_main$df
global <- data.frame(
  test = c("Likelihood ratio (ML)", "Penalised likelihood ratio (Firth)"),
  chisq = c(lr_ml$Deviance[2], pen_lr), df = c(lr_ml$Df[2], pen_df),
  p = c(lr_ml$`Pr(>Chi)`[2], pchisq(pen_lr, pen_df, lower.tail = FALSE)),
  AIC_main = AIC(m_main), AIC_interaction = AIC(m_int),
  BIC_main = BIC(m_main), BIC_interaction = BIC(m_int))
write.csv(global, "outputs/tables/Table_global_heterogeneity.csv", row.names = FALSE)

# --- (b) predictor-specific heterogeneity -----------------------------------
het <- bind_rows(lapply(covs_nd, function(v) {
  m1 <- suppressWarnings(glm(update(f_main, paste(". ~ . + district:", v)), binomial, d))
  a  <- anova(m_main, m1, test = "LRT")
  data.frame(variable = v, LR_chisq = a$Deviance[2], df = a$Df[2], p_het = a$`Pr(>Chi)`[2])
}))
het$p_het_BH <- p.adjust(het$p_het, method = "BH")
write.csv(het, "outputs/tables/Table_heterogeneity_by_predictor.csv", row.names = FALSE)

# --- (c) district-stratified Firth models -----------------------------------
f_strat <- reformulate(covs_nd, "displaced")
strat <- list(); strat_models <- list()
for (k in levels(d$district)) {
  s  <- droplevels(d[d$district == k, ])
  fm <- logistf(f_strat, data = s)
  strat_models[[k]] <- fm
  strat[[k]] <- data.frame(district = k, term = names(coef(fm)), OR = exp(coef(fm)),
                           lo = exp(fm$ci.lower), hi = exp(fm$ci.upper), p = fm$prob,
                           row.names = NULL)[-1, ]
  # apparent discrimination of each stratified model
  pr <- predict(fm, type = "response")
  strat[[k]]$AUC <- as.numeric(pROC::roc(s$displaced, pr, quiet = TRUE, direction = "<")$auc)
  strat[[k]]$n <- nrow(s); strat[[k]]$events <- sum(s$displaced)
}
strat <- bind_rows(strat)
write.csv(strat, "outputs/tables/Table4_stratified_firth.csv", row.names = FALSE)

# pooled Firth estimates for side-by-side display
fp <- logistf(f_main, data = d)
pooled_firth <- data.frame(district = "Pooled", term = names(coef(fp)), OR = exp(coef(fp)),
                           lo = exp(fp$ci.lower), hi = exp(fp$ci.upper), p = fp$prob,
                           row.names = NULL)[-1, ]
write.csv(bind_rows(pooled_firth, strat[, 1:6]), "outputs/tables/FigData_forest.csv", row.names = FALSE)

# --- (d) g-computation: standardised predicted probabilities by district -----
# For each district model, every respondent in that district is assigned each
# level of the focal variable in turn; predictions are averaged. Bootstrap
# resamples within district (B = 1000) give percentile intervals.
focal <- list(
  res      = list(type = "factor", levels = levels(d$res)),
  wdm      = list(type = "factor", levels = levels(d$wdm)),
  exposure = list(type = "num", levels = 0:4),
  neld     = list(type = "num", levels = 0:3),
  rapid    = list(type = "num", levels = 1:3),
  slow     = list(type = "num", levels = 0:3),
  imp_rapid3 = list(type = "factor", levels = c("None or minor", "Recoverable", "Irrecoverable")),
  imp_slow3  = list(type = "factor", levels = c("None or minor", "Recoverable", "Irrecoverable")),
  support  = list(type = "factor", levels = c("Food assistance", "Immediate Relief", "Micro-credit", "None")))

gcomp <- function(fit, s) {
  out <- list()
  for (v in names(focal)) for (l in focal[[v]]$levels) {
    nd <- s
    nd[[v]] <- if (focal[[v]]$type == "factor") factor(l, levels = levels(d[[v]])) else l
    out[[length(out) + 1]] <- data.frame(variable = v, level = as.character(l),
                                         prob = mean(predict(fit, newdata = nd, type = "response")))
  }
  bind_rows(out)
}
B <- 1000; gres <- list()
for (k in levels(d$district)) {
  s   <- droplevels(d[d$district == k, ])
  # keep full factor levels so prediction for unseen levels is defined by the model frame
  for (v in c("support", "provider", "imp_rapid3", "imp_slow3", "edu", "occ", "res", "wdm", "age"))
    s[[v]] <- factor(s[[v]], levels = levels(d[[v]]))
  s <- droplevels(s)
  fit <- logistf(f_strat, data = s, pl = FALSE)
  pt  <- gcomp(fit, s)
  boots <- matrix(NA, nrow(pt), B); b <- 0; tries <- 0
  fac <- names(Filter(is.factor, s[, covs_nd]))
  while (b < B && tries < 5 * B) {
    tries <- tries + 1
    sb <- s[sample(nrow(s), replace = TRUE), ]
    # a resample must contain every factor level so the design matrix is identical
    ok <- all(sapply(fac, function(v) all(levels(s[[v]]) %in% unique(as.character(sb[[v]])))))
    if (!ok) next
    fb <- try(logistf(f_strat, data = sb, pl = FALSE,
                      control = logistf.control(maxit = 100)), silent = TRUE)
    if (inherits(fb, "try-error")) next
    gb <- try(gcomp(fb, s), silent = TRUE)
    if (inherits(gb, "try-error")) next
    b <- b + 1; boots[, b] <- gb$prob
  }
  pt$lo <- apply(boots, 1, quantile, 0.025, na.rm = TRUE)
  pt$hi <- apply(boots, 1, quantile, 0.975, na.rm = TRUE)
  pt$B_valid <- sum(colSums(is.na(boots)) == 0)
  pt$district <- k
  gres[[k]] <- pt
}
gres <- bind_rows(gres)
write.csv(gres, "outputs/tables/FigData_gcomp_by_district.csv", row.names = FALSE)

# district contrasts in the standardised risk difference for key contrasts
rd <- gres %>% group_by(district, variable) %>%
  summarise(contrast = paste0(last(level), " vs ", first(level)),
            RD_pp = 100 * (last(prob) - first(prob)), .groups = "drop")
write.csv(rd, "outputs/tables/TableS_gcomp_risk_differences.csv", row.names = FALSE)

cat("\n=== Global heterogeneity ===\n"); print(global, digits = 4)
cat("\n=== Predictor-specific heterogeneity ===\n"); print(het, digits = 3)
cat("\n=== Stratified Firth (selected) ===\n")
print(strat %>% filter(grepl("res|wdm|exposure|rapid|slow|neld|support|occ|edu|age|gender", term)) %>%
        mutate(across(c(OR, lo, hi), ~ round(.x, 2)), p = round(p, 3)) %>% select(-AUC, -n, -events),
      row.names = FALSE)
cat("\nStratified AUC:\n"); print(unique(strat[, c("district", "AUC", "n", "events")]))
cat("\n=== g-computation ===\n"); print(gres, digits = 3)
