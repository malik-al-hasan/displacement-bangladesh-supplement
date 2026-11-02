# =============================================================================
# 01_main_models.R
# Descriptive tables, pooled multivariable logistic regression (replication of
# the submitted model), penalised (Firth) and modified-Poisson estimators,
# average marginal effects, collinearity, calibration and bootstrap validation.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(logistf); library(rms); library(pROC)
  library(ResourceSelection); library(sandwich); library(lmtest); library(car)
  library(marginaleffects); library(EValue)
})
set.seed(20240531)                       # first day of fieldwork
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
d <- readRDS("outputs/analysis_data.rds")

fmt  <- function(x, k = 2) formatC(x, format = "f", digits = k)
fmtp <- function(p) ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3))

# -----------------------------------------------------------------------------
# 1. Descriptive comparison (Table 1): displaced vs not displaced
# -----------------------------------------------------------------------------
cat_test <- function(x, y) {
  t <- table(x, y)
  e <- suppressWarnings(chisq.test(t)$expected)
  if (any(e < 5)) {
    p <- fisher.test(t, simulate.p.value = TRUE, B = 1e5)$p.value; test <- "Fisher"
  } else {
    p <- chisq.test(t, correct = FALSE)$p.value; test <- "Chi-square"
  }
  list(p = p, test = test)
}
desc_vars <- c(gender = "Sex", age = "Age group", edu = "Educational status",
               occ = "Occupation", res = "Residence duration",
               wdm = "Women's decision-making", district = "District",
               exposure = "Exposure score", rapid = "Rapid-onset hazard score",
               slow = "Slow-onset hazard score", neld = "NELD score",
               imp_rapid = "Impacts of rapid-onset disasters",
               imp_slow = "Impacts of slow-onset disasters",
               support = "Financial support received", provider = "Support provider",
               autonomy = "Autonomy in use of support")
t1 <- list()
for (v in names(desc_vars)) {
  x <- factor(d[[v]]); tst <- cat_test(x, d$displaced)
  t1[[length(t1) + 1]] <- data.frame(Variable = desc_vars[[v]], Level = "",
                                     Overall = "", NotDisplaced = "", Displaced = "",
                                     p = fmtp(tst$p), Test = tst$test)
  for (l in levels(x)) {
    n_all <- sum(x == l); n0 <- sum(x == l & d$displaced == 0); n1 <- sum(x == l & d$displaced == 1)
    t1[[length(t1) + 1]] <- data.frame(
      Variable = "", Level = l,
      Overall = sprintf("%d (%.1f)", n_all, 100 * n_all / nrow(d)),
      NotDisplaced = sprintf("%d (%.1f)", n0, 100 * n0 / sum(d$displaced == 0)),
      Displaced = sprintf("%d (%.1f)", n1, 100 * n1 / sum(d$displaced == 1)),
      p = "", Test = "")
  }
}
t1 <- bind_rows(t1)
write.csv(t1, "outputs/tables/Table1_characteristics_by_displacement.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 2. District hazard profiles (Table 2): item prevalence by district
# -----------------------------------------------------------------------------
items <- c(v_flood = "Flood-prone location", v_lowland = "Low-lying land",
           v_erosion = "Erosion-prone location", v_salinity = "Salinity-affected location",
           v_drought = "Drought-prone location",
           h_flood = "Flood", h_cyclone = "Cyclone", h_drought = "Drought",
           h_tidal = "Tidal surge", h_heat = "Heatwave",
           h_slr = "Sea-level rise", h_erosion = "Riverbank erosion",
           h_salinity = "Salinity intrusion", h_temp = "Rising temperature",
           n_culture = "Loss of cultural heritage", n_health = "Adverse health impacts",
           n_biodiv = "Loss of biodiversity", n_social = "Loss of social cohesion")
domain <- c(rep("Exposure (location)", 5), rep("Rapid-onset hazard", 5),
            rep("Slow-onset hazard", 4), rep("Non-economic loss", 4))
t2 <- bind_rows(lapply(seq_along(items), function(i) {
  v <- names(items)[i]
  B <- d[[v]][d$district == "Barishal"]; K <- d[[v]][d$district == "Kurigram"]
  tst <- cat_test(factor(d[[v]], levels = 0:1), d$district)
  tb  <- table(factor(d[[v]], levels = 0:1), d$district)
  cv  <- sqrt(suppressWarnings(chisq.test(tb, correct = FALSE)$statistic) / nrow(d))
  # displacement prevalence among respondents reporting / not reporting the item
  pB1 <- mean(d$displaced[d$district == "Barishal" & d[[v]] == 1])
  pK1 <- mean(d$displaced[d$district == "Kurigram" & d[[v]] == 1])
  data.frame(Domain = domain[i], Item = items[[i]], var = v,
             Barishal_n = sum(B), Barishal_pct = 100 * mean(B),
             Kurigram_n = sum(K), Kurigram_pct = 100 * mean(K),
             CramersV = as.numeric(cv), p = tst$p, test = tst$test,
             DispIfReported_Barishal = 100 * pB1, DispIfReported_Kurigram = 100 * pK1)
}))
write.csv(t2, "outputs/tables/Table2_hazard_profiles_by_district.csv", row.names = FALSE)

# district contrasts for scores (Wilcoxon) and displacement prevalence
score_tests <- bind_rows(lapply(c("exposure", "rapid", "slow", "neld", "imp_rapid_n", "imp_slow_n"), function(v) {
  w <- wilcox.test(d[[v]] ~ d$district, exact = FALSE)
  data.frame(score = v,
             Barishal_mean = mean(d[[v]][d$district == "Barishal"]),
             Barishal_sd   = sd(d[[v]][d$district == "Barishal"]),
             Kurigram_mean = mean(d[[v]][d$district == "Kurigram"]),
             Kurigram_sd   = sd(d[[v]][d$district == "Kurigram"]), p = w$p.value)
}))
write.csv(score_tests, "outputs/tables/TableS_scores_by_district.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 3. Internal consistency of the composite indices (KR-20)
# -----------------------------------------------------------------------------
kr20 <- function(m) {
  m <- as.matrix(m); k <- ncol(m); p <- colMeans(m); vt <- var(rowSums(m))
  (k / (k - 1)) * (1 - sum(p * (1 - p)) / vt)
}
idx <- list(Exposure = c("v_flood", "v_lowland", "v_erosion", "v_salinity", "v_drought"),
            Rapid = c("h_flood", "h_cyclone", "h_drought", "h_tidal", "h_heat"),
            Slow = c("h_slr", "h_erosion", "h_salinity", "h_temp"),
            NELD = c("n_culture", "n_health", "n_biodiv", "n_social"))
rel <- bind_rows(lapply(names(idx), function(k) {
  m <- d[, idx[[k]]]; ph <- cor(m); ph <- ph[upper.tri(ph)]
  data.frame(Index = k, Items = length(idx[[k]]), KR20 = kr20(m),
             KR20_Barishal = kr20(d[d$district == "Barishal", idx[[k]]][, apply(d[d$district == "Barishal", idx[[k]]], 2, var) > 0]),
             KR20_Kurigram = kr20(d[d$district == "Kurigram", idx[[k]]][, apply(d[d$district == "Kurigram", idx[[k]]], 2, var) > 0]),
             MeanInterItemPhi = mean(ph, na.rm = TRUE))
}))
write.csv(rel, "outputs/tables/TableS_index_reliability.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 4. Pooled crude and adjusted logistic regression (replication)
# -----------------------------------------------------------------------------
covs <- c("gender", "age", "edu", "occ", "res", "wdm", "district", "exposure", "rapid",
          "slow", "neld", "imp_rapid", "imp_slow", "support", "provider", "autonomy")
f_full <- reformulate(covs, response = "displaced")
m_full <- glm(f_full, family = binomial, data = d)

tidy_or <- function(m, prof = TRUE) {
  ci <- if (prof) suppressMessages(confint(m)) else confint.default(m)
  data.frame(term = names(coef(m)), est = exp(coef(m)), lo = exp(ci[, 1]), hi = exp(ci[, 2]),
             p = summary(m)$coefficients[, 4], row.names = NULL)[-1, ]
}
crude <- bind_rows(lapply(covs, function(v) tidy_or(glm(reformulate(v, "displaced"), binomial, d))))
adj   <- tidy_or(m_full)

# Firth penalised logistic regression (bias reduction; Firth 1993, Heinze & Schemper 2002)
m_firth <- logistf(f_full, data = d)
firth <- data.frame(term = names(coef(m_firth)), est = exp(coef(m_firth)),
                    lo = exp(m_firth$ci.lower), hi = exp(m_firth$ci.upper),
                    p = m_firth$prob, row.names = NULL)[-1, ]

# Modified Poisson regression with robust variance (Zou 2004): adjusted prevalence ratios
m_pois <- glm(f_full, family = poisson(link = "log"), data = d)
V_rob  <- sandwich::vcovHC(m_pois, type = "HC0")
ct     <- lmtest::coeftest(m_pois, vcov. = V_rob)
pr <- data.frame(term = rownames(ct), est = exp(ct[, 1]),
                 lo = exp(ct[, 1] - 1.96 * ct[, 2]), hi = exp(ct[, 1] + 1.96 * ct[, 2]),
                 p = ct[, 4], row.names = NULL)[-1, ]

# Average marginal effects (percentage-point change in probability)
ame <- avg_slopes(m_full)
ame <- data.frame(variable = ame$term, contrast = ame$contrast,
                  AME_pp = 100 * ame$estimate, lo = 100 * ame$conf.low,
                  hi = 100 * ame$conf.high, p = ame$p.value)

main_tab <- crude %>% rename(COR = est, COR_lo = lo, COR_hi = hi, COR_p = p) %>%
  full_join(adj %>% rename(AOR = est, AOR_lo = lo, AOR_hi = hi, AOR_p = p), by = "term") %>%
  full_join(firth %>% rename(Firth = est, Firth_lo = lo, Firth_hi = hi, Firth_p = p), by = "term") %>%
  full_join(pr %>% rename(aPR = est, aPR_lo = lo, aPR_hi = hi, aPR_p = p), by = "term")
write.csv(main_tab, "outputs/tables/Table3_pooled_models_raw.csv", row.names = FALSE)
write.csv(ame, "outputs/tables/TableS_AME_pooled.csv", row.names = FALSE)
saveRDS(list(m_full = m_full, m_firth = m_firth, m_pois = m_pois, V_rob = V_rob),
        "outputs/models_pooled.rds")

# -----------------------------------------------------------------------------
# 5. Collinearity (generalised VIF; Fox & Monette 1992)
# -----------------------------------------------------------------------------
gv <- car::vif(m_full)
vif_tab <- data.frame(variable = rownames(gv), GVIF = gv[, 1], df = gv[, 2],
                      GVIF_adj = gv[, 3], GVIF_adj_sq = gv[, 3]^2, row.names = NULL)
write.csv(vif_tab, "outputs/tables/TableS_GVIF.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 6. Model performance, calibration and bootstrap internal validation
# -----------------------------------------------------------------------------
perf <- function(m, y) {
  p   <- fitted(m); n <- length(y)
  ll1 <- as.numeric(logLik(m)); ll0 <- sum(dbinom(y, 1, mean(y), log = TRUE))
  r2cs <- 1 - exp(2 * (ll0 - ll1) / n); r2n <- r2cs / (1 - exp(2 * ll0 / n))
  roc1 <- pROC::roc(y, p, quiet = TRUE, direction = "<")
  ci   <- pROC::ci.auc(roc1, method = "delong")
  hl   <- ResourceSelection::hoslem.test(y, p, g = 10)
  lp   <- qlogis(p); cal <- glm(y ~ lp, family = binomial)
  data.frame(n = n, events = sum(y), k = length(coef(m)) - 1,
             EPV = min(sum(y), n - sum(y)) / (length(coef(m)) - 1),
             AIC = AIC(m), BIC = BIC(m), McFadden = 1 - ll1 / ll0, Nagelkerke = r2n,
             Tjur = mean(p[y == 1]) - mean(p[y == 0]),
             Brier = mean((y - p)^2), AUC = as.numeric(roc1$auc), AUC_lo = ci[1], AUC_hi = ci[3],
             HL_chisq = unname(hl$statistic), HL_df = unname(hl$parameter), HL_p = hl$p.value,
             CalSlope_apparent = coef(cal)[2])
}
perf_full <- perf(m_full, d$displaced)

# rms refit for optimism-corrected indices (Harrell et al. 1996; 1000 bootstrap resamples)
dd <- datadist(d); options(datadist = "dd")
l_full <- lrm(f_full, data = d, x = TRUE, y = TRUE)
val    <- validate(l_full, method = "boot", B = 1000)
val_df <- data.frame(index = rownames(val), apparent = val[, "index.orig"],
                     optimism = val[, "optimism"], corrected = val[, "index.corrected"],
                     row.names = NULL)
val_df <- bind_rows(val_df,
  data.frame(index = "AUC", apparent = 0.5 + val["Dxy", "index.orig"] / 2,
             optimism = val["Dxy", "optimism"] / 2,
             corrected = 0.5 + val["Dxy", "index.corrected"] / 2))
write.csv(val_df, "outputs/tables/TableS_bootstrap_validation.csv", row.names = FALSE)
cal <- calibrate(l_full, method = "boot", B = 1000)
cal_df <- data.frame(predicted = cal[, "predy"], apparent = cal[, "calibrated.orig"],
                     corrected = cal[, "calibrated.corrected"])
write.csv(cal_df, "outputs/tables/FigData_calibration.csv", row.names = FALSE)
roc_df <- with(pROC::roc(d$displaced, fitted(m_full), quiet = TRUE, direction = "<"),
               data.frame(fpr = 1 - specificities, tpr = sensitivities))
write.csv(roc_df, "outputs/tables/FigData_roc.csv", row.names = FALSE)

perf_full$AUC_optimism_corrected <- val_df$corrected[val_df$index == "AUC"]
perf_full$CalSlope_corrected     <- val_df$corrected[val_df$index == "Slope"]
perf_full$Brier_corrected        <- val_df$corrected[val_df$index == "B"]
write.csv(perf_full, "outputs/tables/Table_performance_pooled.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 7. Nested domain blocks: what does each domain add?
# -----------------------------------------------------------------------------
blocks <- list(
  B1 = c("gender", "age", "edu", "occ", "res", "wdm"),
  B2 = "district",
  B3 = c("exposure", "rapid", "slow", "imp_rapid", "imp_slow"),
  B4 = "neld",
  B5 = c("support", "provider", "autonomy"))
lab <- c(B1 = "Sociodemographic", B2 = "+ District", B3 = "+ Exposure and hazards",
         B4 = "+ NELD", B5 = "+ Financial support")
nest <- list(); prev <- NULL; prev_roc <- NULL; vars <- c()
for (b in names(blocks)) {
  vars <- c(vars, blocks[[b]])
  m  <- glm(reformulate(vars, "displaced"), binomial, d)
  pf <- perf(m, d$displaced); r <- pROC::roc(d$displaced, fitted(m), quiet = TRUE, direction = "<")
  lr <- if (is.null(prev)) anova(glm(displaced ~ 1, binomial, d), m, test = "LRT") else anova(prev, m, test = "LRT")
  dl <- if (is.null(prev_roc)) NA else pROC::roc.test(r, prev_roc, method = "delong", paired = TRUE)$p.value
  nest[[b]] <- data.frame(Model = lab[[b]], k = pf$k, AIC = pf$AIC, BIC = pf$BIC,
                          Nagelkerke = pf$Nagelkerke, AUC = pf$AUC,
                          LR_chisq = lr$Deviance[2], LR_df = lr$Df[2], LR_p = lr$`Pr(>Chi)`[2],
                          DeLong_p_vs_previous = dl)
  prev <- m; prev_roc <- r
}
nest <- bind_rows(nest)
write.csv(nest, "outputs/tables/Table_nested_blocks.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# 8. E-values for the principal adjusted associations (VanderWeele & Ding 2017)
# -----------------------------------------------------------------------------
ev_terms <- c("resAbove 20 years", "wdmYes", "districtKurigram", "exposure", "neld",
              "supportMicro-credit")
ev <- bind_rows(lapply(ev_terms, function(t) {
  r <- pr[pr$term == t, ]
  e <- EValue::evalues.RR(r$est, r$lo, r$hi)
  data.frame(term = t, aPR = r$est, lo = r$lo, hi = r$hi,
             E_point = e["E-values", "point"],
             E_CI = ifelse(is.na(e["E-values", "lower"]), e["E-values", "upper"], e["E-values", "lower"]))
}))
write.csv(ev, "outputs/tables/TableS_Evalues.csv", row.names = FALSE)

cat("\n=== Pooled adjusted model (replication check) ===\n")
print(adj %>% mutate(across(c(est, lo, hi), ~ round(.x, 2)), p = round(p, 3)), row.names = FALSE)
cat("\n=== Performance ===\n"); print(t(perf_full))
cat("\n=== Nested blocks ===\n"); print(nest, digits = 3)
cat("\n=== E-values ===\n"); print(ev, digits = 3)
cat("\n=== GVIF ===\n"); print(vif_tab, digits = 3)
cat("\n=== Reliability ===\n"); print(rel, digits = 3)
