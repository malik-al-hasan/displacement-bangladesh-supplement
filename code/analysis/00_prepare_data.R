# =============================================================================
# 00_prepare_data.R
# Climate-induced displacement in Barishal (delta) and Kurigram (floodplain)
# Reads the raw survey export, recodes variables exactly as in the submitted
# analysis, verifies the composite scores, and saves an analysis-ready file.
# =============================================================================

suppressPackageStartupMessages(library(dplyr))

raw <- read.csv("data/data1.csv", check.names = FALSE, stringsAsFactors = FALSE)
names(raw) <- trimws(names(raw))

impact_levels <- c("No impacts", "Some bad impacts", "Recoverable impacts",
                   "A lot of recoverable impacts", "A lot of irrecoverable impacts")

d <- raw %>%
  transmute(
    displaced = as.integer(Displacement == "Yes"),
    gender    = factor(Gender, levels = c("Male", "Female")),
    age       = factor(Age_group, levels = c("18-35", "36-65", "65 or older")),
    edu       = factor(Educational_status,
                       levels = c("No Education", "Below Primary", "High school or less",
                                  "SSC", "HSC or above")),
    occ_raw   = Occupation,
    # Manual labour = day labourers, farmers and fishers; Other = housewives,
    # shopkeepers and other occupations (coding used in the submitted Table 1)
    occ       = factor(case_when(
                  Occupation %in% c("Day Laborer", "Farmer", "Fisherman") ~ "Manual labour",
                  Occupation == "Student" ~ "Student",
                  TRUE ~ "Other"),
                  levels = c("Manual labour", "Student", "Other")),
    res       = factor(Residence_duration,
                       levels = c("Up to 5 years", "6-20 years", "Above 20 years")),
    wdm       = factor(Women_decision, levels = c("No", "Not sure", "Yes")),
    district  = factor(District, levels = c("Barishal", "Kurigram")),
    exposure  = `Exposure score (0-5)`,
    rapid     = `Hazard rapid onset score`,
    slow      = `Hazard Slow onset score`,
    neld      = `NELD score`,
    imp_rapid = factor(Impacts_rapid_disasters, levels = impact_levels),
    imp_slow  = factor(Impacts_slow_disasters,  levels = impact_levels),
    imp_rapid_n = as.integer(imp_rapid),           # 1-5 ordinal score
    imp_slow_n  = as.integer(imp_slow),
    # three-level severity used in the district comparison, because the
    # "no impacts" cell is nearly empty in Kurigram (3 and 2 respondents)
    imp_rapid3 = factor(cut(imp_rapid_n, c(0, 2, 4, 5), labels = c("None or minor", "Recoverable", "Irrecoverable"))),
    imp_slow3  = factor(cut(imp_slow_n,  c(0, 2, 4, 5), labels = c("None or minor", "Recoverable", "Irrecoverable"))),
    support   = factor(Get_financial_support,
                       levels = c("Food assistance", "Immediate Relief", "Micro-credit",
                                  "Vocational Training", "Others", "None")),
    provider  = factor(Support_provider,
                       levels = c("Local communities", "National governments",
                                  "Non-governmental organizations (NGOs)", "Others")),
    autonomy  = factor(Autonomy_use_finance, levels = c("No", "Yes")),
    reason    = Reasons_displacement,
    issue     = Issues_face_displacement,
    # item-level indicators
    v_flood = Vulnerability_flood_area, v_lowland = Vulnerability_Low_lying_land,
    v_erosion = Vulnerability_Erosion, v_salinity = Vulnerability_Salinity_intrusion,
    v_drought = Vulnerability_Drought,
    h_flood = Hazard_flood, h_cyclone = Hazard_Cyclones, h_drought = Hazard_Drought,
    h_tidal = Hazard_Tidal_Surge, h_heat = Hazard_Heatwave,
    h_slr = Hazard_Sea_level_rise, h_erosion = Hazard_Riverbank_erosion,
    h_salinity = Hazard_Salinity_intrusion, h_temp = Hazard_Increase_temperature,
    n_culture = NELD_cultural_heritage, n_health = NELD_Health_impacts,
    n_biodiv = NELD_biodiversity, n_social = NELD_social_cohesion
  )

# --- integrity checks: composite scores must equal the sum of their items ----
stopifnot(nrow(d) == 602, !anyNA(d[, c("displaced", "gender", "age", "edu", "occ", "res",
                                        "wdm", "district", "imp_rapid", "imp_slow",
                                        "support", "provider", "autonomy")]))
stopifnot(all(d$exposure == with(d, v_flood + v_lowland + v_erosion + v_salinity + v_drought)))
stopifnot(all(d$rapid    == with(d, h_flood + h_cyclone + h_drought + h_tidal + h_heat)))
stopifnot(all(d$slow     == with(d, h_slr + h_erosion + h_salinity + h_temp)))
stopifnot(all(d$neld     == with(d, n_culture + n_health + n_biodiv + n_social)))

dir.create("outputs", showWarnings = FALSE)
saveRDS(d, "outputs/analysis_data.rds")
write.csv(d, "outputs/analysis_data.csv", row.names = FALSE)
cat("Analysis file written: n =", nrow(d), "| displaced =", sum(d$displaced), "\n")
