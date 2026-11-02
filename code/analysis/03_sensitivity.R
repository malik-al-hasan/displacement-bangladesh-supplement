# =============================================================================
# 03_sensitivity.R
# Robustness of the principal estimates to estimator, specification and
# temporal-ordering concerns; item-level decomposition of the composite indices.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(logistf); library(sandwich); library(lmtest) })
set.seed(602)
d <- readRDS("outputs/analysis_data.rds")

covs <- c("gender", "age", "edu", "occ", "res", "wdm", "district", "exposure", "rapid",
          "slow", "neld", "imp_rapid", "imp_slow", "support", "provider", "autonomy")
key <- c("resAbove 20 years", "wdmYes", "districtKurigram", "exposure", "rapid", "slow",
         "neld", "supportMicro-credit")

or_glm <- function(m, robust = FALSE) {
  if (robust) {
    ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1"))
    data.frame(term = rownames(ct), OR = exp(ct[, 1]), lo = exp(ct[, 1] - 1.96 * ct[, 2]),
               hi = exp(ct[, 1] + 1.96 * ct[, 2]), p = ct[, 4], row.names = NULL)
  } else {
    ci <- suppressMessages(confint(m))
    data.frame(term = names(coef(m)), OR = exp(coef(m)), lo = exp(ci[, 1]), hi = exp(ci[, 2]),
               p = summary(m)$coefficients[, 4], row.names = NULL)
  }
}
or_firth <- function(fm) data.frame(term = names(coef(fm)), OR = exp(coef(fm)),
                                    lo = exp(fm$ci.lower), hi = exp(fm$ci.upper), p = fm$prob,
                                    row.names = NULL)
fit <- function(v, data = d) glm(reformulate(v, "displaced"), binomial, data)

specs <- list()
m0 <- fit(covs)
specs[["S0 Main model (ML)"]]               <- or_glm(m0)
specs[["S1 Firth penalised likelihood"]]     <- or_firth(logistf(reformulate(covs, "displaced"), data = d))
specs[["S2 Heteroskedasticity-robust SE"]]   <- or_glm(m0, robust = TRUE)

# S3 nonparametric bootstrap percentile intervals (B = 2000)
B <- 2000; bt <- matrix(NA, B, length(coef(m0)), dimnames = list(NULL, names(coef(m0))))
for (b in seq_len(B)) {
  db <- d[sample(nrow(d), replace = TRUE), ]
  mb <- try(suppressWarnings(glm(reformulate(covs, "displaced"), binomial, db)), silent = TRUE)
  if (!inherits(mb, "try-error") && mb$converged) {
    cf <- coef(mb); bt[b, names(cf)] <- cf
  }
}
specs[["S3 Bootstrap percentile CI"]] <- data.frame(
  term = colnames(bt), OR = exp(coef(m0)),
  lo = exp(apply(bt, 2, quantile, 0.025, na.rm = TRUE)),
  hi = exp(apply(bt, 2, quantile, 0.975, na.rm = TRUE)),
  p = NA, row.names = NULL)

specs[["S4 Without residence duration"]]      <- or_glm(fit(setdiff(covs, "res")))
specs[["S5 Without financial-support block"]] <- or_glm(fit(setdiff(covs, c("support", "provider", "autonomy"))))
specs[["S6 Without residence and support"]]   <- or_glm(fit(setdiff(covs, c("res", "support", "provider", "autonomy"))))
specs[["S7 Impacts as ordinal trends"]]       <- or_glm(fit(c(setdiff(covs, c("imp_rapid", "imp_slow")), "imp_rapid_n", "imp_slow_n")))
specs[["S8 Female respondents only"]]         <- or_glm(fit(setdiff(covs, "gender"), data = d[d$gender == "Female", ]))

sens <- bind_rows(lapply(names(specs), function(s) mutate(specs[[s]], spec = s))) %>%
  filter(term %in% key)
write.csv(sens, "outputs/tables/TableS_sensitivity_key_estimates.csv", row.names = FALSE)

# women's decision-making x respondent sex
m_ws <- fit(c(covs, "wdm:gender"))
lr_ws <- anova(m0, m_ws, test = "LRT")
ws <- data.frame(test = "wdm x gender", chisq = lr_ws$Deviance[2], df = lr_ws$Df[2], p = lr_ws$`Pr(>Chi)`[2])
write.csv(ws, "outputs/tables/TableS_wdm_by_gender.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# Item-level decomposition (Firth): replace one composite at a time by its items,
# in the pooled sample and within each district
# -----------------------------------------------------------------------------
item_sets <- list(
  NELD = list(drop = "neld", add = c("n_culture", "n_health", "n_biodiv", "n_social")),
  Exposure = list(drop = "exposure", add = c("v_flood", "v_lowland", "v_erosion", "v_drought")),
  Hazards = list(drop = c("rapid", "slow"),
                 add = c("h_flood", "h_cyclone", "h_drought", "h_heat", "h_slr", "h_erosion", "h_temp")))
# salinity, tidal surge items omitted from item models: < 25 positive responses
item_res <- list()
for (set in names(item_sets)) {
  for (k in c("Pooled", "Barishal", "Kurigram")) {
    s <- if (k == "Pooled") d else droplevels(d[d$district == k, ])
    v <- c(setdiff(covs, item_sets[[set]]$drop), item_sets[[set]]$add)
    if (k != "Pooled") v <- setdiff(v, "district")
    # drop items with no variation in the stratum
    v <- v[sapply(v, function(x) length(unique(s[[x]])) > 1)]
    fm <- logistf(reformulate(v, "displaced"), data = s)
    item_res[[paste(set, k)]] <- or_firth(fm) %>% filter(term %in% item_sets[[set]]$add) %>%
      mutate(set = set, sample = k)
  }
}
item_res <- bind_rows(item_res)
write.csv(item_res, "outputs/tables/TableS_item_level_models.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# Displacement reasons and post-displacement problems among the displaced
# -----------------------------------------------------------------------------
disp <- d[d$displaced == 1, ]
rs <- as.data.frame(prop.table(table(disp$reason, disp$district), 2) * 100)
names(rs) <- c("category", "district", "pct"); rs$question <- "Primary reason for moving"
is <- as.data.frame(prop.table(table(disp$issue, disp$district), 2) * 100)
names(is) <- c("category", "district", "pct"); is$question <- "Main problem after moving"
rsn <- as.data.frame(table(disp$reason, disp$district)); isn <- as.data.frame(table(disp$issue, disp$district))
rs$n <- rsn$Freq; is$n <- isn$Freq
ri <- bind_rows(rs, is)
p_reason <- fisher.test(table(disp$reason, disp$district), simulate.p.value = TRUE, B = 1e5)$p.value
p_issue  <- fisher.test(table(disp$issue, disp$district), simulate.p.value = TRUE, B = 1e5)$p.value
ri$p_district <- ifelse(ri$question == "Primary reason for moving", p_reason, p_issue)
write.csv(ri, "outputs/tables/FigData_reasons_issues.csv", row.names = FALSE)

cat("\n=== Sensitivity (key terms) ===\n")
print(sens %>% mutate(across(c(OR, lo, hi), ~ round(.x, 2)), p = round(p, 3)) %>%
        select(spec, term, OR, lo, hi, p), row.names = FALSE)
cat("\n=== wdm x gender ===\n"); print(ws)
cat("\n=== Item-level ===\n")
print(item_res %>% mutate(across(c(OR, lo, hi), ~ round(.x, 2)), p = round(p, 3)), row.names = FALSE)
cat("\n=== Reasons/issues ===\n"); print(ri, digits = 3); cat("p reason:", p_reason, " p issue:", p_issue, "\n")
