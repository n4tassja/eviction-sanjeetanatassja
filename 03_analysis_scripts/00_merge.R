# =============================================================================
# 00: Merge Student A and Student B outputs
# =============================================================================
# Input:  02_clean_data/ca_county_evictions.RData  (Student A)
#         02_clean_data/ca_prop_clean.RData         (Student B)
# Output: 02_clean_data/ca_county_merged.RData
#
# Run this before any analysis script.
# =============================================================================

rm(list = ls())

library(pacman)

p_load(tidyverse, tidylog, janitor)

# -----------------------------------------------------------------------------
# 1. Load both cleaned datasets
# -----------------------------------------------------------------------------

load("02_clean_data/ca_county_evictions.RData")   # ca_county
load("02_clean_data/ca_prop_clean.RData")          # ca_prop_clean

glimpse(ca_county)
glimpse(ca_prop_clean)


# -----------------------------------------------------------------------------
# 2. Merge
# -----------------------------------------------------------------------------

# Left join: keep all 477 rows from Student A.
# Student B's proprietary data covers only 16 small rural counties,
# so most rows will have NA for filing_rate_prop and judgement_rate.
# The main analysis variables (filing_rate, coastal, post2008) all come
# from Student A and are complete for all 58 counties.

ca_county_merged <- ca_county %>%
  left_join(
    ca_prop_clean %>% select(-county),   # drop county to avoid duplication
    by = c("geoid", "year")
  )

ca_county_merged %>%
  summarise(
    total_rows      = n(),
    has_prop_rate   = sum(!is.na(filing_rate_prop)),
    has_judgement   = sum(!is.na(judgement_rate))
  )


# -----------------------------------------------------------------------------
# 3. Save
# -----------------------------------------------------------------------------

save(ca_county_merged, file = "02_clean_data/ca_county_merged.RData")

message("Done! Saved ca_county_merged.RData — ",
        nrow(ca_county_merged), " rows, ",
        ncol(ca_county_merged), " columns.")

