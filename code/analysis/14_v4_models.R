# =============================================================================
# 14_v4_models.R
# The revision analysis (08_revision_models.R) re-estimated on the corrected
# analytic sample recovered from the raw KoboToolbox export:
#   adults (18+) with a valid Yes/No answer to item 5.13, n = 571.
# Model structure, estimands and tests are unchanged from 08; only the sample
# and outcome coding differ. Outputs go to outputs/v4/.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(logistf); library(sandwich); library(lmtest)
  library(rms); library(pROC); library(EValue)
})
set.seed(20240531)
out <- "outputs/v4"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
d_all <- readRDS("outputs/v4/analysis_data_v4_all602.rds")
d <- d_all %>% filter(in_primary) %>% mutate(displaced = displaced_valid) %>% droplevels()
saveRDS(d, file.path(out, "analysis_data_v4_primary.rds"))

M1_vars <- c("gender", "age", "edu3", "district", "rapid", "slow", "imp_rapid3", "imp_slow3")
CORR <- c(neld = "NELD score", wdm = "Women hold decision-making power", exposure = "Exposure of current location",
          res = "Residence duration", support4 = "Support received", occ2 = "Occupation")

pr_tab <- function(m) {
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC0"))
  data.frame(term = rownames(ct), PR = exp(ct[, 1]), lo = exp(ct[, 1] - 1.96 * ct[, 2]),
             hi = exp(ct[, 1] + 1.96 * ct[, 2]), p = ct[, 4], row.names = NULL)[-1, ]
}
or_tab <- function(m) {
  ci <- suppressMessages(confint(m))
  data.frame(term = names(coef(m)), OR = exp(coef(m)), lo = exp(ci[, 1]), hi = exp(ci[, 2]),
             p = summary(m)$coefficients[, 4], row.names = NULL)[-1, ]
}
firth_tab <- function(fm) data.frame(term = names(coef(fm)), OR = exp(coef(fm)), lo = exp(fm$ci.lower),
                                     hi = exp(fm$ci.upper), p = fm$prob, row.names = NULL)[-1, ]
fpois <- function(v, data, y = "displaced") glm(reformulate(v, y), family = poisson(link = "log"), data = data)
flogit <- function(v, data, y = "displaced") glm(reformulate(v, y), family = binomial, data = data)

# ---- 1. temporal classification table ---------------------------------------
temporal <- tribble(
  ~Variable, ~Timing, ~Role,
  "Sex", "Pre-displacement (fixed)", "Primary model",
  "Age group", "Pre-displacement (fixed)", "Primary model",
  "Education", "Pre-displacement (largely completed in early adulthood)", "Primary model",
  "District", "Pre-displacement for within-district moves; origin district not recorded", "Primary model (stratifier)",
  "Hazard experience, rapid-onset and slow-onset counts", "Time-ambiguous (refers to hazards experienced; may span origin and destination)", "Primary model",
  "Perceived impact severity", "Time-ambiguous (recalled impacts of hazards experienced)", "Primary model",
  "Occupation", "Time-ambiguous (may change with a move)", "Correlate, added singly",
  "NELD score", "Time-ambiguous (reported at interview; may describe origin or current place)", "Correlate, added singly",
  "Women hold decision-making power", "Potentially post-displacement (current household)", "Correlate, added singly",
  "Exposure of current location", "Potentially post-displacement (describes destination for movers)", "Correlate, added singly",
  "Residence duration", "Post-displacement for movers (resets with a move)", "Correlate, added singly",
  "Support received, provider, autonomy", "Potentially post-displacement (timing not recorded)", "Correlate, added singly (support type only)")
write.csv(temporal, file.path(out, "Table_temporal_classification.csv"), row.names = FALSE)

# ---- 2. primary model, pooled -------------------------------------------------
m1_p <- fpois(M1_vars, d); m1_l <- flogit(M1_vars, d); m1_f <- logistf(reformulate(M1_vars, "displaced"), data = d)
prim <- pr_tab(m1_p) %>% rename(aPR = PR, aPR_lo = lo, aPR_hi = hi, aPR_p = p) %>%
  left_join(or_tab(m1_l) %>% rename(AOR = OR, AOR_lo = lo, AOR_hi = hi, AOR_p = p), by = "term") %>%
  left_join(firth_tab(m1_f) %>% rename(Firth = OR, Firth_lo = lo, Firth_hi = hi, Firth_p = p), by = "term")
write.csv(prim, file.path(out, "Table_primary_model_pooled.csv"), row.names = FALSE)

# ---- 3. heterogeneity: global and per-covariate (logistic LR) ----------------------
nd <- setdiff(M1_vars, "district")
m1_int <- suppressWarnings(flogit(c("district", nd, paste0("district:", nd)), d))
g_ml <- anova(m1_l, m1_int, test = "LRT")
f_main <- logistf(reformulate(M1_vars, "displaced"), data = d, pl = FALSE)
f_int  <- logistf(reformulate(c("district", nd, paste0("district:", nd)), "displaced"), data = d, pl = FALSE)
pen <- 2 * (f_int$loglik["full"] - f_main$loglik["full"]); pdf_ <- f_int$df - f_main$df
global <- data.frame(model = "Primary model (M1)", LR = g_ml$Deviance[2], df = g_ml$Df[2], p = g_ml$`Pr(>Chi)`[2],
                     penLR = pen, pen_df = pdf_, pen_p = pchisq(pen, pdf_, lower.tail = FALSE),
                     AIC_main = AIC(m1_l), AIC_int = AIC(m1_int), BIC_main = BIC(m1_l), BIC_int = BIC(m1_int))
het <- bind_rows(lapply(nd, function(v) {
  a <- anova(m1_l, suppressWarnings(flogit(c(M1_vars, paste0("district:", v)), d)), test = "LRT")
  data.frame(variable = v, LR = a$Deviance[2], df = a$Df[2], p = a$`Pr(>Chi)`[2])
}))
conf_set <- c("rapid", "slow", "imp_rapid3", "imp_slow3")           # confirmatory family
het$family <- ifelse(het$variable %in% conf_set, "confirmatory", "exploratory")
het$q <- NA
het$q[het$family == "confirmatory"] <- p.adjust(het$p[het$family == "confirmatory"], "BH")
het$q[het$family == "exploratory"]  <- p.adjust(het$p[het$family == "exploratory"], "BH")

# ---- 4. district-specific primary models -----------------------------------------
strat <- list(); spars <- list(); gres <- list()
for (k in c("Barishal", "Kurigram")) {
  s <- droplevels(d[d$district == k, ]); v <- setdiff(M1_vars, "district")
  mp <- fpois(v, s); fl <- logistf(reformulate(v, "displaced"), data = s)
  strat[[k]] <- pr_tab(mp) %>% rename(aPR = PR, aPR_lo = lo, aPR_hi = hi, aPR_p = p) %>%
    left_join(firth_tab(fl) %>% rename(FirthOR = OR, F_lo = lo, F_hi = hi, F_p = p), by = "term") %>%
    mutate(district = k, n = nrow(s), events = sum(s$displaced), k_params = length(coef(mp)) - 1,
           EPV = min(sum(s$displaced), nrow(s) - sum(s$displaced)) / (length(coef(mp)) - 1))
  # sparse-cell counts for every categorical term used in M1 and the correlates
  for (f in c("gender", "age", "edu3", "imp_rapid3", "imp_slow3", "wdm", "res", "support4", "occ2")) {
    tb <- table(s[[f]], s$displaced)
    spars[[paste(k, f)]] <- data.frame(district = k, variable = f, level = rownames(tb),
                                       not_displaced = tb[, "0"], displaced = tb[, "1"], row.names = NULL)
  }
}
strat <- bind_rows(strat); spars <- bind_rows(spars)
write.csv(strat, file.path(out, "Table_primary_model_by_district.csv"), row.names = FALSE)
write.csv(spars, file.path(out, "TableS_sparse_cells_by_district.csv"), row.names = FALSE)

# ---- 5. correlates: each time-ambiguous characteristic added singly to M1 ---------------
corr_res <- list(); corr_het <- list()
for (x in names(CORR)) {
  vx <- c(M1_vars, x)
  pooled <- pr_tab(fpois(vx, d)) %>% filter(startsWith(term, x)) %>% mutate(sample = "Pooled")
  by_d <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
    s <- droplevels(d[d$district == k, ])
    pr_tab(fpois(setdiff(vx, "district"), s)) %>% filter(startsWith(term, x)) %>% mutate(sample = k)
  }))
  corr_res[[x]] <- bind_rows(pooled, by_d) %>% mutate(variable = x)
  a <- anova(flogit(vx, d), suppressWarnings(flogit(c(vx, paste0("district:", x)), d)), test = "LRT")
  corr_het[[x]] <- data.frame(variable = x, LR = a$Deviance[2], df = a$Df[2], p = a$`Pr(>Chi)`[2])
}
corr_res <- bind_rows(corr_res); corr_het <- bind_rows(corr_het)
corr_het$q <- p.adjust(corr_het$p, "BH")
write.csv(corr_res, file.path(out, "Table_correlates_aPR.csv"), row.names = FALSE)
write.csv(bind_rows(het, corr_het %>% mutate(family = "correlate (exploratory)")),
          file.path(out, "Table_heterogeneity_tests.csv"), row.names = FALSE)
write.csv(global, file.path(out, "Table_global_heterogeneity_M1.csv"), row.names = FALSE)

# ---- 6. standardised probabilities by district (g-computation, Firth, bootstrap) -------
focal <- list(slow = 0:3, rapid = 1:3, imp_rapid3 = levels(d$imp_rapid3), imp_slow3 = levels(d$imp_slow3),
              neld = 0:3, wdm = levels(d$wdm), exposure = 0:4, res = levels(d$res),
              support4 = levels(d$support4))
gcomp_one <- function(fit, s, v) sapply(focal[[v]], function(l) {
  nd2 <- s; nd2[[v]] <- if (is.factor(d[[v]])) factor(l, levels = levels(d[[v]])) else l
  mean(predict(fit, newdata = nd2, type = "response"))
})
B <- 1000
for (k in c("Barishal", "Kurigram")) {
  s0 <- d[d$district == k, ]
  for (v in names(focal)) {
    vars <- if (v %in% names(CORR)) c(setdiff(M1_vars, "district"), v) else setdiff(M1_vars, "district")
    fac <- vars[sapply(vars, function(z) is.factor(d[[z]]))]
    s <- s0; for (z in fac) s[[z]] <- factor(s[[z]], levels = levels(d[[z]])); s <- droplevels(s)
    fit <- logistf(reformulate(vars, "displaced"), data = s, pl = FALSE)
    est <- gcomp_one(fit, s, v)
    boots <- matrix(NA, length(est), B); b <- 0; tries <- 0
    while (b < B && tries < 6 * B) {
      tries <- tries + 1
      sb <- s[sample(nrow(s), replace = TRUE), ]
      if (!all(sapply(fac, function(z) all(levels(s[[z]]) %in% unique(as.character(sb[[z]])))))) next
      fb <- try(logistf(reformulate(vars, "displaced"), data = sb, pl = FALSE,
                        control = logistf.control(maxit = 100)), silent = TRUE)
      if (inherits(fb, "try-error")) next
      gb <- try(gcomp_one(fb, s, v), silent = TRUE); if (inherits(gb, "try-error")) next
      b <- b + 1; boots[, b] <- gb
    }
    gres[[paste(k, v)]] <- data.frame(district = k, variable = v, level = as.character(focal[[v]]), prob = est,
                                      lo = apply(boots, 1, quantile, .025, na.rm = TRUE),
                                      hi = apply(boots, 1, quantile, .975, na.rm = TRUE), B_valid = b,
                                      model = ifelse(v %in% names(CORR), "M1 + correlate", "M1"))
  }
}
gres <- bind_rows(gres)
write.csv(gres, file.path(out, "FigData_gcomp_revision.csv"), row.names = FALSE)

# ---- 7. sensitivity analyses for the primary model and correlates --------------------
sens <- list()
key_terms <- c("districtKurigram", "rapid", "slow", "imp_rapid3Irrecoverable", "imp_slow3Irrecoverable")
sens[["Primary model (modified Poisson)"]] <- pr_tab(m1_p)
sens[["Strict outcome (work-motivated and unexplained moves excluded)"]] <- pr_tab(fpois(M1_vars, d, "displaced_strict"))
alt <- c("gender", "age", "edu3", "district", "sudden", "gradual", "imp_rapid3", "imp_slow3")
sens[["Alternative hazard taxonomy (sudden vs gradual)"]] <- pr_tab(fpois(alt, d))
sens[["Primary model + all correlates jointly"]] <- pr_tab(fpois(c(M1_vars, names(CORR)), d))
sens[["Original full model (35 terms)"]] <- pr_tab(fpois(c("gender", "age", "edu", "occ", "res", "wdm", "district",
  "exposure", "rapid", "slow", "neld", "imp_rapid", "imp_slow", "support", "provider", "autonomy"), d))
d602 <- d_all %>% mutate(displaced = displaced_data1)
sens[["data1 coding (non-response = No, Not sure = Yes, minors kept; n = 602)"]] <- pr_tab(fpois(M1_vars, d602))
dlo <- d_all %>% filter(adult) %>% mutate(displaced = ifelse(is.na(displaced_valid), 0L, displaced_valid))
dhi <- d_all %>% filter(adult) %>% mutate(displaced = ifelse(is.na(displaced_valid), 1L, displaced_valid))
sens[["Bound: all item non-response coded not displaced"]] <- pr_tab(fpois(M1_vars, dlo))
sens[["Bound: all item non-response coded displaced"]] <- pr_tab(fpois(M1_vars, dhi))
sens_df <- bind_rows(lapply(names(sens), function(s) mutate(sens[[s]], spec = s)))
write.csv(sens_df, file.path(out, "TableS_sensitivity_primary.csv"), row.names = FALSE)
# correlates under the strict outcome
cs <- bind_rows(lapply(names(CORR), function(x) pr_tab(fpois(c(M1_vars, x), d, "displaced_strict")) %>%
  filter(startsWith(term, x)) %>% mutate(variable = x)))
write.csv(cs, file.path(out, "TableS_correlates_strict_outcome.csv"), row.names = FALSE)
# district heterogeneity under the alternative taxonomy
alt_int <- suppressWarnings(flogit(c("district", setdiff(alt, "district"), paste0("district:", setdiff(alt, "district"))), d))
g_alt <- anova(flogit(alt, d), alt_int, test = "LRT")
alt_strat <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- droplevels(d[d$district == k, ])
  pr_tab(fpois(setdiff(alt, "district"), s)) %>% mutate(district = k)
}))
write.csv(alt_strat, file.path(out, "TableS_alt_taxonomy_by_district.csv"), row.names = FALSE)
g_strict <- {
  dd <- d[!is.na(d$displaced_strict), ]
  a <- anova(flogit(M1_vars, dd, "displaced_strict"),
             suppressWarnings(flogit(c("district", nd, paste0("district:", nd)), dd, "displaced_strict")), test = "LRT")
  data.frame(model = "Primary model, strict outcome", LR = a$Deviance[2], df = a$Df[2], p = a$`Pr(>Chi)`[2], n = nrow(dd))
}
write.csv(bind_rows(global %>% select(model, LR, df, p),
                    data.frame(model = "Alternative hazard taxonomy", LR = g_alt$Deviance[2], df = g_alt$Df[2], p = g_alt$`Pr(>Chi)`[2]),
                    g_strict %>% select(model, LR, df, p)),
          file.path(out, "TableS_global_heterogeneity_sensitivity.csv"), row.names = FALSE)

# reasons for moving by displacement status (outcome definition check)
rr <- as.data.frame(table(reason = d$reason, displaced = d$displaced))
write.csv(rr, file.path(out, "TableS_reason_by_status.csv"), row.names = FALSE)

# ---- 8. performance and internal validation of the primary model ------------------------
dd2 <- d; dd_ <- datadist(dd2); options(datadist = "dd_")
l1 <- lrm(reformulate(M1_vars, "displaced"), data = dd2, x = TRUE, y = TRUE)
val <- validate(l1, B = 1000)
cal <- calibrate(l1, B = 1000)
roc1 <- pROC::roc(d$displaced, fitted(m1_l), quiet = TRUE, direction = "<")
perf <- data.frame(model = "Primary model (M1)", n = nrow(d), events = sum(d$displaced), k = length(coef(m1_l)) - 1,
                   EPV = min(sum(d$displaced), sum(1 - d$displaced)) / (length(coef(m1_l)) - 1), AUC = as.numeric(roc1$auc),
                   AUC_lo = pROC::ci.auc(roc1)[1], AUC_hi = pROC::ci.auc(roc1)[3],
                   AUC_corrected = 0.5 + val["Dxy", "index.corrected"] / 2,
                   slope_corrected = val["Slope", "index.corrected"], Brier = val["B", "index.orig"],
                   Brier_corrected = val["B", "index.corrected"], R2_apparent = val["R2", "index.orig"],
                   R2_corrected = val["R2", "index.corrected"])
# the interaction model's in-sample gain, corrected
l1i <- lrm(reformulate(c("district", nd, paste0("district:", nd)), "displaced"), data = dd2, x = TRUE, y = TRUE)
vali <- validate(l1i, B = 1000)
perf <- bind_rows(perf, data.frame(model = "M1 with district interactions", n = nrow(d), events = sum(d$displaced),
                   k = length(coef(l1i)) - 1, EPV = min(sum(d$displaced), sum(1 - d$displaced)) / (length(coef(l1i)) - 1),
                   AUC = 0.5 + vali["Dxy", "index.orig"] / 2, AUC_corrected = 0.5 + vali["Dxy", "index.corrected"] / 2,
                   slope_corrected = vali["Slope", "index.corrected"], Brier = vali["B", "index.orig"],
                   Brier_corrected = vali["B", "index.corrected"], R2_apparent = vali["R2", "index.orig"],
                   R2_corrected = vali["R2", "index.corrected"]))
write.csv(perf, file.path(out, "Table_performance_revision.csv"), row.names = FALSE)
write.csv(data.frame(predicted = cal[, "predy"], apparent = cal[, "calibrated.orig"], corrected = cal[, "calibrated.corrected"]),
          file.path(out, "FigData_calibration_M1.csv"), row.names = FALSE)
write.csv(with(roc1, data.frame(fpr = 1 - specificities, tpr = sensitivities)), file.path(out, "FigData_roc_M1.csv"), row.names = FALSE)

# E-values for the primary-model aPRs
ev <- bind_rows(lapply(seq_len(nrow(prim)), function(i) {
  r <- prim[i, ]; e <- EValue::evalues.RR(r$aPR, r$aPR_lo, r$aPR_hi)
  data.frame(term = r$term, aPR = r$aPR, E_point = e["E-values", "point"],
             E_CI = ifelse(is.na(e["E-values", "lower"]), e["E-values", "upper"], e["E-values", "lower"]))
}))
write.csv(ev, file.path(out, "TableS_Evalues_primary.csv"), row.names = FALSE)

# ---- console summary ---------------------------------------------------------------
fmt <- function(df) df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n== Primary model (pooled) ==\n"); print(fmt(prim[, 1:5]), row.names = FALSE)
cat("\n== Global heterogeneity ==\n"); print(fmt(global), row.names = FALSE)
cat("\n== Per-covariate heterogeneity ==\n"); print(fmt(bind_rows(het, corr_het)), row.names = FALSE)
cat("\n== District primary models (aPR) ==\n"); print(fmt(strat[, c("district", "term", "aPR", "aPR_lo", "aPR_hi", "aPR_p", "EPV")]), row.names = FALSE)
cat("\n== Correlates ==\n"); print(fmt(corr_res), row.names = FALSE)
cat("\n== Sensitivity (key) ==\n"); print(fmt(sens_df %>% filter(term %in% c(key_terms, "sudden", "gradual"))), row.names = FALSE)
cat("\n== Strict-outcome correlates ==\n"); print(fmt(cs), row.names = FALSE)
cat("\n== Global heterogeneity sensitivity ==\n"); print(read.csv(file.path(out, "TableS_global_heterogeneity_sensitivity.csv")))
cat("\n== Alt taxonomy by district ==\n"); print(fmt(alt_strat %>% filter(term %in% c("sudden", "gradual"))), row.names = FALSE)
cat("\n== Performance ==\n"); print(fmt(perf))
cat("\n== Reasons by status ==\n"); print(rr)
cat("\n== g-comp ==\n"); print(fmt(gres), row.names = FALSE)
