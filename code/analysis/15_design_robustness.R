# =============================================================================
# 15_design_robustness.R
# Analyses that the raw KoboToolbox export makes possible (v4):
#   A. how the displacement item was answered and coded
#   B. displacement prevalence by field site (settlement scale)
#   C. prevalence by interviewer (anonymised E1-E7)
#   D. variance partition: site vs interviewer (crossed random intercepts)
#   E. interviewer effect within sites (Kurigram is fully crossed)
#   F. district-specific primary-model aPRs under design-based specifications
#   G. district heterogeneity conditional on site and interviewer fixed effects
#   H. leave-one-interviewer-out stability
#   I. convergent validity: item 5.13 vs the NELD item "displacement or migration"
#   J. time since arrival among displaced respondents
#   K. transportability across districts and to a third region (Sunamganj, 2024)
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sandwich); library(lmtest); library(clubSandwich)
  library(lme4); library(logistf); library(DescTools); library(pROC)
})
set.seed(20240531)
out <- "outputs/v4"
d_all <- readRDS(file.path(out, "analysis_data_v4_all602.rds"))
d <- readRDS(file.path(out, "analysis_data_v4_primary.rds"))
d$kur <- as.integer(d$district == "Kurigram")
# interviewer-within-district contrasts: one shared baseline level for the first
# interviewer in each district, so that site and interviewer fixed effects (both
# nested in district) are not collinear
base_lv <- d %>% group_by(district) %>% summarise(b = sort(as.character(unique(enum_dist)))[1]) %>% pull(b)
d$enum_w <- relevel(factor(ifelse(as.character(d$enum_dist) %in% base_lv, "base", as.character(d$enum_dist))), ref = "base")
M1_vars <- c("gender", "age", "edu3", "district", "rapid", "slow", "imp_rapid3", "imp_slow3")
nd <- setdiff(M1_vars, "district")
fpois <- function(v, data, y = "displaced") suppressWarnings(glm(reformulate(v, y), family = poisson(link = "log"), data = data))
flogit <- function(v, data, y = "displaced") suppressWarnings(glm(reformulate(v, y), family = binomial, data = data))
keep_terms <- function(df) df %>% filter(!grepl("^site|^enum|^\\(Intercept", term))
pr_hc <- function(m) {
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC0"))
  keep_terms(data.frame(term = rownames(ct), PR = exp(ct[, 1]), lo = exp(ct[, 1] - 1.96 * ct[, 2]),
                        hi = exp(ct[, 1] + 1.96 * ct[, 2]), p = ct[, 4], row.names = NULL))
}
pr_cr2 <- function(m, cl) {
  V <- vcovCR(m, cluster = cl, type = "CR2")
  ci <- conf_int(m, vcov = V, test = "Satterthwaite"); ct <- coef_test(m, vcov = V, test = "Satterthwaite")
  keep_terms(data.frame(term = ct$Coef, PR = exp(ct$beta), lo = exp(ci$CI_L), hi = exp(ci$CI_U),
                        p = ct$p_Satt, df_satt = ct$df_Satt, row.names = NULL))
}
wil <- function(x, n) { r <- BinomCI(x, n, method = "wilson"); data.frame(prev = r[, 1], lo = r[, 2], hi = r[, 3]) }

# ---- A. outcome item: answers and coding -----------------------------------------
A <- d_all %>% mutate(answer = factor(raw_513, levels = c("Yes", "No", "Not sure", "None"))) %>%
  count(answer, district) %>% pivot_wider(names_from = district, values_from = n, values_fill = 0) %>%
  mutate(data1_coding = c("Displaced", "Not displaced", "Displaced", "Not displaced")[as.integer(answer)],
         v4_coding = c("Displaced", "Not displaced", "Excluded", "Excluded")[as.integer(answer)])
A <- bind_rows(A %>% mutate(answer = as.character(answer)),
               data.frame(answer = "Aged under 18 (any answer)", Barishal = sum(!d_all$adult & d_all$district == "Barishal"),
                          Kurigram = sum(!d_all$adult & d_all$district == "Kurigram"),
                          data1_coding = "Kept (merged into 18-35)", v4_coding = "Excluded"))
write.csv(A, file.path(out, "Table_outcome_item_coding.csv"), row.names = FALSE)

# ---- B. site-level prevalence ---------------------------------------------------
B <- d_all %>% group_by(district, upazila, site, site_name, settlement) %>%
  summarise(interviewed = n(), valid = sum(in_primary), displaced = sum(displaced_valid[in_primary]),
            interviewers = n_distinct(enum), female_pct = 100 * mean(gender == "Female"),
            res_over20_pct = 100 * mean(res_raw == "Above 20 years"),
            first = min(date), last = max(date), .groups = "drop") %>%
  bind_cols(wil(.$displaced, .$valid))
Bd <- d_all %>% group_by(district) %>%
  summarise(interviewed = n(), valid = sum(in_primary), displaced = sum(displaced_valid[in_primary]),
            interviewers = n_distinct(enum), female_pct = 100 * mean(gender == "Female"),
            res_over20_pct = 100 * mean(res_raw == "Above 20 years"), first = min(date), last = max(date)) %>%
  bind_cols(wil(.$displaced, .$valid)) %>% mutate(site = "All", site_name = paste(district, "(all sites)"))
B <- bind_rows(B, Bd) %>% arrange(district, site == "All")
# heterogeneity of prevalence across sites within each district
site_chi <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- d %>% filter(district == k); t <- table(droplevels(s$site), s$displaced)
  data.frame(district = k, chisq = suppressWarnings(chisq.test(t)$statistic), df = nrow(t) - 1,
             p_exact = fisher.test(t, simulate.p.value = TRUE, B = 1e5)$p.value,
             range_lo = min(prop.table(t, 1)[, 2]), range_hi = max(prop.table(t, 1)[, 2]))
}))
write.csv(B, file.path(out, "Table_site_prevalence.csv"), row.names = FALSE)
write.csv(site_chi, file.path(out, "TableS_site_heterogeneity_tests.csv"), row.names = FALSE)

# ---- C. interviewer-level prevalence (anonymised) ---------------------------------
C <- d %>% group_by(district, enum) %>%
  summarise(valid = n(), displaced = sum(displaced), sites = n_distinct(site),
            female_pct = 100 * mean(gender == "Female"), res_over20_pct = 100 * mean(res_raw == "Above 20 years"),
            .groups = "drop") %>% bind_cols(wil(.$displaced, .$valid))
Ck <- d %>% filter(district == "Kurigram") %>% group_by(site, enum) %>%
  summarise(valid = n(), displaced = sum(displaced), .groups = "drop") %>% mutate(prev = displaced / valid)
Cb <- d %>% filter(district == "Barishal") %>% group_by(site, enum) %>%
  summarise(valid = n(), displaced = sum(displaced), .groups = "drop") %>% mutate(prev = displaced / valid)
write.csv(C, file.path(out, "TableS_interviewer_prevalence.csv"), row.names = FALSE)
write.csv(bind_rows(Cb %>% mutate(district = "Barishal"), Ck %>% mutate(district = "Kurigram")),
          file.path(out, "FigData_site_by_interviewer.csv"), row.names = FALSE)

# ---- D. variance partition: crossed random intercepts -------------------------------
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
vp <- function(f, data, label) {
  g <- glmer(f, family = binomial, data = data, control = ctrl)
  vc <- as.data.frame(VarCorr(g)); s2 <- setNames(vc$vcov, vc$grp); tot <- sum(s2) + pi^2 / 3
  drop_one <- function(grp) {
    f2 <- update(f, as.formula(paste(". ~ . - (1 |", grp, ")")))
    g2 <- if (any(grepl("\\|", deparse(f2)))) glmer(f2, family = binomial, data = data, control = ctrl) else glm(f2, binomial, data)
    lr <- as.numeric(2 * (logLik(g) - logLik(g2))); data.frame(LR = lr, p_boundary = 0.5 * pchisq(lr, 1, lower.tail = FALSE))
  }
  bind_rows(lapply(names(s2), function(grp) cbind(data.frame(model = label, level = grp, variance = s2[[grp]],
    ICC_latent = s2[[grp]] / tot, MOR = exp(sqrt(2 * s2[[grp]]) * qnorm(0.75))), drop_one(grp))))
}
D <- bind_rows(
  vp(displaced ~ district + (1 | site) + (1 | enum_dist), d, "Null (district only)"),
  vp(reformulate(c(M1_vars, "(1 | site)", "(1 | enum_dist)"), "displaced"), d, "Primary covariates"),
  vp(displaced ~ (1 | site) + (1 | enum_dist), d %>% filter(district == "Kurigram"), "Kurigram, null"),
  vp(displaced ~ (1 | site) + (1 | enum_dist), d %>% filter(district == "Barishal"), "Barishal, null"))
write.csv(D, file.path(out, "Table_variance_partition.csv"), row.names = FALSE)

# ---- E. interviewer effect within sites (penalised LR, Firth) -------------------------
E <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- droplevels(d %>% filter(district == k))
  fa <- logistf(displaced ~ site, data = s, pl = FALSE); fb <- logistf(displaced ~ site + enum, data = s, pl = FALSE)
  fc <- logistf(reformulate(c(nd, "site"), "displaced"), data = s, pl = FALSE)
  fd <- logistf(reformulate(c(nd, "site", "enum"), "displaced"), data = s, pl = FALSE)
  lr1 <- 2 * (fb$loglik["full"] - fa$loglik["full"]); lr2 <- 2 * (fd$loglik["full"] - fc$loglik["full"])
  data.frame(district = k, n_interviewers = nlevels(s$enum), n_sites = nlevels(s$site),
             test = c("Interviewer | site", "Interviewer | site + primary covariates"),
             penLR = c(lr1, lr2), df = nlevels(s$enum) - 1,
             p = pchisq(c(lr1, lr2), nlevels(s$enum) - 1, lower.tail = FALSE))
}))
write.csv(E, file.path(out, "TableS_interviewer_effect_tests.csv"), row.names = FALSE)

# ---- F. district-specific aPRs under design-based specifications ---------------------
F_ <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- droplevels(d %>% filter(district == k)); cell <- interaction(s$site, s$enum, drop = TRUE)
  m0 <- fpois(nd, s)
  bind_rows(
    pr_hc(m0) %>% mutate(spec = "1 Primary (HC0)"),
    pr_cr2(m0, cell) %>% mutate(spec = "2 CR2, site x interviewer clusters"),
    pr_hc(fpois(c(nd, "site"), s)) %>% mutate(spec = "3 + site fixed effects"),
    pr_hc(fpois(c(nd, "enum"), s)) %>% mutate(spec = "4 + interviewer fixed effects"),
    pr_cr2(fpois(c(nd, "site", "enum"), s), cell) %>% mutate(spec = "5 + site and interviewer FE (CR2)")
  ) %>% mutate(district = k, n = nrow(s), clusters = nlevels(cell))
}))
write.csv(F_, file.path(out, "Table_design_robustness_aPR.csv"), row.names = FALSE)

# ---- F2. correlates (added singly) within sites and interviewers --------------------------
CORRV <- c("neld", "wdm", "exposure", "res", "support4", "occ2")
F2 <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- droplevels(d %>% filter(district == k)); cell <- interaction(s$site, s$enum, drop = TRUE)
  bind_rows(lapply(CORRV, function(x) bind_rows(
    pr_hc(fpois(c(nd, x), s)) %>% filter(startsWith(term, x)) %>% mutate(spec = "Primary + correlate (HC0)"),
    pr_cr2(fpois(c(nd, x, "site", "enum"), s), cell) %>% filter(startsWith(term, x)) %>%
      mutate(spec = "+ site and interviewer FE (CR2)")) %>% mutate(variable = x))) %>% mutate(district = k)
}))
write.csv(F2, file.path(out, "TableS_correlates_within_site_interviewer.csv"), row.names = FALSE)

# ---- G. district heterogeneity conditional on site and interviewer FE -----------------
# explicit Kurigram x covariate columns (contrast-coded), so the test does not rely
# on R's marginality rules when the district main effect is absorbed by site FE
kur_int <- function(data, vars) {
  X <- model.matrix(reformulate(vars), data)[, -1, drop = FALSE] * data$kur
  colnames(X) <- paste0("kx_", make.names(colnames(X))); X
}
het_test <- function(base, data, label, vars = nd) {
  X <- kur_int(data, vars); data <- cbind(data, X); ints <- colnames(X)
  a <- anova(flogit(base, data), flogit(c(base, ints), data), test = "LRT")
  fa <- logistf(reformulate(base, "displaced"), data = data, pl = FALSE)
  fb <- logistf(reformulate(c(base, ints), "displaced"), data = data, pl = FALSE)
  pen <- 2 * (fb$loglik["full"] - fa$loglik["full"]); pdf_ <- fb$df - fa$df
  data.frame(spec = label, LR = a$Deviance[2], df = a$Df[2], p = a$`Pr(>Chi)`[2],
             penLR = pen, pen_df = pdf_, pen_p = pchisq(pen, pdf_, lower.tail = FALSE))
}
G <- bind_rows(
  het_test(M1_vars, d, "Primary (district main effect)"),
  het_test(c(nd, "site"), d, "Site fixed effects"),
  het_test(c(nd, "site", "enum_w"), d, "Site + interviewer-by-district fixed effects"))
G_each <- bind_rows(lapply(nd, function(v) {
  base <- c(nd, "site", "enum_w"); X <- kur_int(d, v); dd <- cbind(d, X)
  a <- anova(flogit(base, dd), flogit(c(base, colnames(X)), dd), test = "LRT")
  data.frame(variable = v, LR = a$Deviance[2], df = a$Df[2], p = a$`Pr(>Chi)`[2])
}))
write.csv(G, file.path(out, "Table_heterogeneity_conditional_FE.csv"), row.names = FALSE)
write.csv(G_each, file.path(out, "TableS_heterogeneity_each_conditional_FE.csv"), row.names = FALSE)

# ---- H. leave-one-interviewer-out ------------------------------------------------------
H <- bind_rows(lapply(levels(d$enum), function(e) {
  s0 <- droplevels(d %>% filter(enum != e))
  g <- het_test(M1_vars, s0, paste("drop", e))
  bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
    s <- droplevels(s0 %>% filter(district == k))
    pr_hc(fpois(nd, s)) %>% mutate(district = k, n = nrow(s))
  })) %>% mutate(dropped = e, global_LR = g$LR, global_p = g$p)
}))
H_sum <- H %>% group_by(district, term) %>%
  summarise(PR_min = min(PR), PR_max = max(PR), n_sig = sum(p < 0.05), runs = n(), .groups = "drop")
write.csv(H, file.path(out, "TableS_leave_one_interviewer_out.csv"), row.names = FALSE)
write.csv(H_sum, file.path(out, "TableS_leave_one_interviewer_out_summary.csv"), row.names = FALSE)

# ---- I. convergent validity with the NELD 'displacement or migration' item -------------
I <- d %>% group_by(district) %>%
  summarise(n = n(), both = sum(displaced == 1 & neld_dm == 1), item513_only = sum(displaced == 1 & neld_dm == 0),
            neld_only = sum(displaced == 0 & neld_dm == 1), neither = sum(displaced == 0 & neld_dm == 0),
            agreement = mean(displaced == neld_dm),
            kappa = CohenKappa(table(factor(displaced, 0:1), factor(neld_dm, 0:1))), .groups = "drop")
I_enum <- d %>% group_by(district, enum) %>%
  summarise(n = n(), p513 = mean(displaced), p_neld = mean(neld_dm), agreement = mean(displaced == neld_dm), .groups = "drop")
# primary model with the NELD item as an alternative outcome
I_mod <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- droplevels(d %>% filter(district == k)); pr_hc(fpois(nd, s, "neld_dm")) %>% mutate(district = k)
}))
dd <- d %>% mutate(displaced = neld_dm)
I_het <- het_test(M1_vars, dd, "Alternative outcome: NELD displacement or migration")
write.csv(I, file.path(out, "TableS_convergent_validity.csv"), row.names = FALSE)
write.csv(I_enum, file.path(out, "TableS_convergent_validity_by_interviewer.csv"), row.names = FALSE)
write.csv(I_mod, file.path(out, "TableS_alt_outcome_neld_dm_by_district.csv"), row.names = FALSE)
write.csv(I_het, file.path(out, "TableS_alt_outcome_neld_dm_heterogeneity.csv"), row.names = FALSE)

# ---- J. time since arrival among displaced respondents ---------------------------------
J <- d %>% filter(displaced == 1) %>%
  mutate(res3 = recode(res_raw, "Less than a year" = "Up to 5 years", "1-5 years" = "Up to 5 years")) %>%
  count(district, site_name, res3) %>% group_by(district, site_name) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
Jd <- d %>% filter(displaced == 1) %>%
  mutate(res3 = recode(res_raw, "Less than a year" = "Up to 5 years", "1-5 years" = "Up to 5 years")) %>%
  count(district, res3) %>% group_by(district) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
write.csv(bind_rows(Jd %>% mutate(site_name = "All sites"), J), file.path(out, "TableS_time_since_arrival.csv"), row.names = FALSE)

# ---- K. transportability -------------------------------------------------------------------
su <- read.csv("outputs/raw/sunamganj_2024_coded.csv", stringsAsFactors = FALSE) %>% filter(in_primary %in% c(TRUE, "True")) %>%
  mutate(displaced = displaced_valid,
         gender = factor(gender, levels = levels(d$gender)), age = factor(age, levels = levels(d$age)),
         edu3 = factor(edu3, levels = levels(d$edu3)),
         imp_rapid3 = factor(imp_rapid3, levels = levels(d$imp_rapid3)),
         imp_slow3 = factor(imp_slow3, levels = levels(d$imp_slow3)))
trans_eval <- function(train, test, label) {
  fit <- logistf(reformulate(nd, "displaced"), data = train, pl = FALSE)
  lp <- predict(fit, newdata = test, type = "link"); p <- plogis(lp)
  cal_int <- coef(glm(test$displaced ~ offset(lp), family = binomial))[1]
  cal_slope <- coef(glm(test$displaced ~ lp, family = binomial))[2]
  r <- pROC::roc(test$displaced, p, quiet = TRUE, direction = "<"); ci <- pROC::ci.auc(r)
  data.frame(model = label, n_test = nrow(test), observed = mean(test$displaced), predicted = mean(p),
             OE = mean(test$displaced) / mean(p), cal_intercept = cal_int, cal_slope = cal_slope,
             AUC = as.numeric(r$auc), AUC_lo = ci[1], AUC_hi = ci[3])
}
# in-place reference: repeated 10-fold cross-validation within each district
cv_ref <- function(s, label, R = 20) {
  res <- replicate(R, {
    f <- sample(rep(1:10, length.out = nrow(s))); p <- numeric(nrow(s))
    for (i in 1:10) {
      fit <- logistf(reformulate(nd, "displaced"), data = s[f != i, ], pl = FALSE)
      p[f == i] <- predict(fit, newdata = s[f == i, ], type = "response")
    }
    c(auc = as.numeric(pROC::roc(s$displaced, p, quiet = TRUE, direction = "<")$auc), OE = mean(s$displaced) / mean(p))
  })
  data.frame(model = label, n_test = nrow(s), observed = mean(s$displaced), predicted = NA,
             OE = mean(res["OE", ]), cal_intercept = NA, cal_slope = NA, AUC = mean(res["auc", ]),
             AUC_lo = quantile(res["auc", ], .025), AUC_hi = quantile(res["auc", ], .975))
}
bar <- droplevels(d %>% filter(district == "Barishal")); kurg <- droplevels(d %>% filter(district == "Kurigram"))
bar$age <- factor(bar$age, levels = levels(d$age)); kurg$age <- factor(kurg$age, levels = levels(d$age))
K <- bind_rows(
  cv_ref(bar, "Barishal -> Barishal (10-fold CV)"),
  cv_ref(kurg, "Kurigram -> Kurigram (10-fold CV)"),
  trans_eval(bar, kurg, "Barishal -> Kurigram"),
  trans_eval(kurg, bar, "Kurigram -> Barishal"),
  trans_eval(d, su, "Pooled (both districts) -> Sunamganj 2024"),
  trans_eval(bar, su, "Barishal -> Sunamganj 2024"),
  trans_eval(kurg, su, "Kurigram -> Sunamganj 2024"))
# transport with predictors that are less interviewer-dependent (no impact-severity ratings)
nd_red <- c("gender", "age", "edu3", "rapid", "slow")
trans_red <- function(train, test, label) { nd_old <- nd; nd <<- nd_red; on.exit(nd <<- nd_old); trans_eval(train, test, label) }
cv_red <- function(s, label) { nd_old <- nd; nd <<- nd_red; on.exit(nd <<- nd_old); cv_ref(s, label) }
K_red <- bind_rows(
  cv_red(bar, "Barishal -> Barishal (10-fold CV)"), cv_red(kurg, "Kurigram -> Kurigram (10-fold CV)"),
  trans_red(bar, kurg, "Barishal -> Kurigram"), trans_red(kurg, bar, "Kurigram -> Barishal"),
  trans_red(d, su, "Pooled (both districts) -> Sunamganj 2024")) %>% mutate(predictors = "Sex, age, education, hazard counts")
write.csv(bind_rows(K %>% mutate(predictors = "Primary model"), K_red), file.path(out, "TableS_transportability_both_sets.csv"), row.names = FALSE)
su_desc <- read.csv("outputs/raw/sunamganj_2024_coded.csv", stringsAsFactors = FALSE) %>%
  count(upazila, raw_513) %>% pivot_wider(names_from = raw_513, values_from = n, values_fill = 0)
write.csv(K, file.path(out, "Table_transportability.csv"), row.names = FALSE)
write.csv(su_desc, file.path(out, "TableS_sunamganj_outcome.csv"), row.names = FALSE)

# ---- console ------------------------------------------------------------------------------
r3 <- function(x) x %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
cat("\n== A outcome coding ==\n"); print(A)
cat("\n== B sites ==\n"); print(r3(B %>% select(district, upazila, site, site_name, interviewed, valid, displaced, prev, lo, hi, interviewers, res_over20_pct)), width = 200)
print(r3(site_chi))
cat("\n== C interviewers ==\n"); print(r3(C)); print(tibble::as_tibble(r3(Ck)), n = 50)
cat("\n== D variance partition ==\n"); print(r3(D))
cat("\n== E interviewer within site ==\n"); print(r3(E))
cat("\n== F design robustness ==\n"); print(tibble::as_tibble(r3(F_ %>% select(district, spec, term, PR, lo, hi, p))), n = 200)
cat("\n== G heterogeneity | FE ==\n"); print(r3(G)); print(r3(G_each))
cat("\n== H LOIO summary ==\n"); print(tibble::as_tibble(r3(H_sum)), n = 50); print(r3(distinct(H, dropped, global_LR, global_p)))
cat("\n== I convergent validity ==\n"); print(r3(I)); print(r3(I_enum)); print(r3(I_mod)); print(r3(I_het))
cat("\n== J time since arrival ==\n"); print(r3(Jd))
cat("\n== K transport ==\n"); print(r3(K)); print(su_desc)
