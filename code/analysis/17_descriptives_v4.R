# =============================================================================
# 17_descriptives_v4.R
# Table 1 and district item profiles (01_main_models.R, sections 1-2) on the
# v4 analytic sample (adults with a valid answer to item 5.13, n = 571), plus
# destination choice and post-move problems among displaced respondents.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
set.seed(20240531)
TV <- "outputs/v4/tables"; dir.create(TV, recursive = TRUE, showWarnings = FALSE)
d <- readRDS("outputs/v4/analysis_data_v4_primary.rds")

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
write.csv(t1, file.path(TV, "Table1_characteristics_by_displacement.csv"), row.names = FALSE)

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
write.csv(t2, file.path(TV, "Table2_hazard_profiles_by_district.csv"), row.names = FALSE)


# destination choice (item 5.14) and problems faced (5.15) by district and status
dc <- d %>% mutate(status = ifelse(displaced == 1, "Displaced", "Not displaced")) %>%
  count(district, status, item = "Reason for choosing area of relocation (5.14)", level = reason) %>%
  bind_rows(d %>% mutate(status = ifelse(displaced == 1, "Displaced", "Not displaced")) %>%
              count(district, status, item = "Problem faced due to displacement (5.15)", level = issue)) %>%
  group_by(district, status, item) %>% mutate(pct = 100 * n / sum(n)) %>% ungroup()
write.csv(dc, file.path(TV, "TableS_destination_choice_and_problems.csv"), row.names = FALSE)
# Fisher tests of destination choice between districts among displaced respondents
dd <- d %>% filter(displaced == 1)
cat("work as destination reason:", tapply(dd$reason == "Job Opportunities", dd$district, mean), "
")
print(fisher.test(table(dd$district, dd$reason == "Job Opportunities")))
print(table(dd$district, dd$reason)); print(round(prop.table(table(dd$district, dd$reason), 1) * 100, 1))
print(round(prop.table(table(dd$district, dd$issue), 1) * 100, 1))
cat("reason answered by non-displaced:", sum(d$displaced == 0 & d$reason != "None"), "of", sum(d$displaced == 0), "
")
print(table(d$district, d$displaced))
print(t2 %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
cat("NELD mean by district:", tapply(d$neld, d$district, mean), wilcox.test(neld ~ district, d)$p.value, "
")
