# =============================================================================
# 08b_revision_items.R
# Exploratory item-level models under the revised (parsimonious) specification,
# GVIF for the primary model, and E-values for primary-model prevalence ratios.
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(logistf); library(sandwich); library(lmtest); library(car) })
out <- "outputs/revision"
d <- readRDS(file.path(out, "analysis_data_revision.rds"))
base <- c("gender", "age", "edu3", "imp_rapid3", "imp_slow3")
pr_tab <- function(m) {
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC0"))
  data.frame(term = rownames(ct), PR = exp(ct[, 1]), lo = exp(ct[, 1] - 1.96 * ct[, 2]),
             hi = exp(ct[, 1] + 1.96 * ct[, 2]), p = ct[, 4], row.names = NULL)[-1, ]
}
sets <- list(
  Hazards = list(replace = c("rapid", "slow"), items = c("h_flood", "h_cyclone", "h_drought", "h_heat", "h_slr", "h_erosion", "h_temp"), role = "primary"),
  Exposure = list(replace = character(0), items = c("v_flood", "v_lowland", "v_erosion", "v_drought"), role = "correlate"),
  NELD = list(replace = character(0), items = c("n_culture", "n_health", "n_biodiv", "n_social"), role = "correlate"))
res <- list()
for (nm in names(sets)) for (k in c("Pooled", "Barishal", "Kurigram")) {
  s <- if (k == "Pooled") d else droplevels(d[d$district == k, ])
  v <- c(base, if (k == "Pooled") "district", setdiff(c("rapid", "slow"), sets[[nm]]$replace), sets[[nm]]$items)
  v <- v[sapply(v, function(x) length(unique(s[[x]])) > 1 && (!is.numeric(s[[x]]) || sum(s[[x]]) >= 10 || !x %in% sets[[nm]]$items))]
  m <- glm(reformulate(v, "displaced"), family = poisson(link = "log"), data = s)
  fm <- logistf(reformulate(v, "displaced"), data = s)
  r <- pr_tab(m) %>% filter(term %in% sets[[nm]]$items)
  f <- data.frame(term = names(coef(fm)), OR = exp(coef(fm)), OR_lo = exp(fm$ci.lower), OR_hi = exp(fm$ci.upper))
  res[[paste(nm, k)]] <- r %>% left_join(f, by = "term") %>% mutate(set = nm, sample = k,
                                         n_reporting = sapply(term, function(t) sum(s[[t]])))
}
res <- bind_rows(res)
write.csv(res, file.path(out, "TableS_item_level_revision.csv"), row.names = FALSE)

m1 <- glm(reformulate(c(base, "district", "rapid", "slow"), "displaced"), binomial, d)
gv <- car::vif(m1)
write.csv(data.frame(variable = rownames(gv), GVIF = gv[, 1], df = gv[, 2], GVIF_adj = gv[, 3], row.names = NULL),
          file.path(out, "TableS_GVIF_primary.csv"), row.names = FALSE)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE)
print(gv)
print(read.csv(file.path(out, "TableS_Evalues_primary.csv")) %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
