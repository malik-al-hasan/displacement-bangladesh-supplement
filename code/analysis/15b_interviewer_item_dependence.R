suppressPackageStartupMessages({library(dplyr); library(DescTools)})
d <- readRDS("outputs/v4/analysis_data_v4_primary.rds")
items <- c(displaced = "Reported displacement (5.13)", neld_dm = "NELD: displacement or migration (5.2)",
           gender = "Sex", age = "Age group", edu3 = "Education",
           imp_rapid3 = "Rapid-onset impact severity (3.3)", imp_slow3 = "Slow-onset impact severity (4.2)",
           rapid = "Rapid-onset hazard count (3.2)", slow = "Slow-onset hazard count (4.1)",
           exposure = "Current-location exposure count (2.5)", neld = "NELD count (5.2)", wdm = "Women in response decisions (5.9)",
           res = "Residence duration (2.1)", support4 = "Support received (7.1)")
out <- bind_rows(lapply(c("Barishal", "Kurigram"), function(k) {
  s <- droplevels(d[d$district == k, ])
  bind_rows(lapply(names(items), function(v) {
    x <- factor(s[[v]])
    # interviewer association overall, and within site (Cochran-Mantel-Haenszel-type: average V across sites)
    V_all <- CramerV(table(s$enum, x))
    within <- sapply(levels(s$site), function(st) { ss <- s[s$site == st, ]; t <- table(droplevels(ss$enum), droplevels(factor(ss[[v]])))
      if (nrow(t) < 2 || ncol(t) < 2) return(NA); CramerV(t) })
    w <- table(s$site); wv <- sum(within * w[names(within)], na.rm = TRUE) / sum(w[names(within)][!is.na(within)])
    p <- suppressWarnings(chisq.test(table(s$enum, x), simulate.p.value = TRUE, B = 2000)$p.value)
    data.frame(district = k, item = items[[v]], V_interviewer = V_all, V_within_site = wv, p_sim = p)
  }))
}))
write.csv(out, "outputs/v4/TableS_interviewer_dependence_of_items.csv", row.names = FALSE)
print(out %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
