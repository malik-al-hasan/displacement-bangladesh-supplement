# =============================================================================
# 13_prepare_v4.R
# Adds the raw-export information recovered in 12_link_raw_kobo.py to the
# analysis file and defines the corrected analytic sample.
#
# What the raw export showed about data1.csv
#   * 5.13 "Have you been displaced from another area?": 27 item non-responses
#     had been coded "No" and 2 "Not sure" answers coded "Yes".
#   * 3 respondents aged under 18 had been merged into the 18-35 group.
#   * 1 respondent who reported a gender other than male or female was dropped.
#
# Primary analytic sample (v4): adults with a valid Yes/No answer to 5.13.
# The data1 coding (n = 602) is retained as a sensitivity analysis.
# =============================================================================
suppressPackageStartupMessages(library(dplyr))
d <- readRDS("outputs/analysis_data.rds")
L <- read.csv("outputs/raw/linked_raw_data1.csv", check.names = FALSE, stringsAsFactors = FALSE,
              na.strings = character(0))
stopifnot(nrow(L) == nrow(d), all(L$d1_row == seq_len(nrow(d))),
          all((L$Displacement == "Yes") == (d$displaced == 1)))

d <- d %>% mutate(
  edu3 = factor(case_when(edu == "No Education" ~ "No education",
                          edu == "Below Primary" ~ "Below primary",
                          TRUE ~ "Secondary or above"),
                levels = c("No education", "Below primary", "Secondary or above")),
  occ2 = factor(ifelse(occ == "Manual labour", "Manual labour", "Other"), levels = c("Manual labour", "Other")),
  support4 = factor(case_when(support %in% c("Food assistance", "Immediate Relief") ~ "In-kind relief",
                              support == "Micro-credit" ~ "Microcredit",
                              support %in% c("Vocational Training", "Others") ~ "Training or other",
                              TRUE ~ "None"),
                    levels = c("In-kind relief", "Microcredit", "Training or other", "None")),
  sudden  = h_flood + h_cyclone + h_tidal + h_heat + h_erosion,
  gradual = h_drought + h_slr + h_salinity + h_temp,
  # raw-export fields
  raw_513   = L$raw_displaced_5_13,
  displaced_data1 = displaced,
  displaced_valid = case_when(raw_513 == "Yes" ~ 1L, raw_513 == "No" ~ 0L, TRUE ~ NA_integer_),
  adult     = L$raw_age != "Under 18",
  site      = factor(L$site, levels = c("LH", "MC", "PP", "BB", "NC", "LG", "UL", "BG", "FC", "MD")),
  site_name = L$site_name, upazila = L$upazila, settlement = L$settlement_type,
  enum      = factor(L$enum_id),
  enum_dist = factor(paste(L$enum_id, L$District, sep = "_")),
  date      = as.Date(L$interview_date),
  neld_dm   = L$raw_neld_displacement_migration,
  res_raw   = L$raw_residence,
  reason_raw = L$raw_reason_5_14,
  issue_raw  = L$raw_issue_5_15,
  wdm_raw   = L$raw_women_dm_5_9,
  autonomy_raw = L$raw_autonomy_7_5,
  anxious   = L$raw_anxious_5_5, sleep = L$raw_sleep_5_6,
  in_primary = adult & !is.na(displaced_valid)
)
# the v3 strict outcome, defined on the corrected outcome
d$displaced_strict <- ifelse(d$displaced_valid == 1 & d$reason %in% c("Job Opportunities", "None"), NA,
                             d$displaced_valid)

dir.create("outputs/v4", showWarnings = FALSE, recursive = TRUE)
saveRDS(d, "outputs/v4/analysis_data_v4_all602.rds")

cat("Primary analytic sample:", sum(d$in_primary), "\n")
print(with(d, table(district, raw_513, useNA = "ifany")))
print(with(d[d$in_primary, ], table(district, displaced_valid)))
print(with(d, table(adult, in_primary)))
