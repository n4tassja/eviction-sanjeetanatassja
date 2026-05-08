# =============================================================================
# Student B: Tables and Regression
# =============================================================================
# Input:  02_clean_data/ca_county_merged.RData
# Output: 05_tables/table_summary_stats.csv
#         05_tables/table_means_by_group.csv
#         05_tables/table_regression.txt
#
# Run 03_analysis_scripts/00_merge.R first.
# =============================================================================

rm(list = ls())

library(pacman)

p_load(tidyverse, tidylog, janitor, modelsummary, kableExtra)

load("02_clean_data/ca_county_merged.RData")


# -----------------------------------------------------------------------------
# 1. Summary statistics table
# -----------------------------------------------------------------------------

# Select the variables we care about and compute summary stats

summary_stats <- ca_county_merged %>%
  select(filing_rate, filings_observed, renting_hh, coastal, post2008) %>%
  summarise(across(
    everything(),
    list(
      mean   = ~ mean(.x, na.rm = TRUE),
      sd     = ~ sd(.x, na.rm = TRUE),
      min    = ~ min(.x, na.rm = TRUE),
      max    = ~ max(.x, na.rm = TRUE),
      n_miss = ~ sum(is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(summary_stats)

write_csv(summary_stats, "05_tables/table_summary_stats.csv")


# -----------------------------------------------------------------------------
# 2. Mean filing rate by group
# -----------------------------------------------------------------------------

# Compare coastal vs. inland and pre vs. post 2008

means_by_group <- ca_county_merged %>%
  mutate(
    group  = if_else(coastal == 1, "Coastal", "Inland"),
    period = if_else(post2008 == 1, "Post-2008", "Pre-2008")
  ) %>%
  group_by(group, period) %>%
  summarise(
    mean_filing_rate = mean(filing_rate, na.rm = TRUE),
    sd_filing_rate   = sd(filing_rate,   na.rm = TRUE),
    n_obs            = n(),
    .groups = "drop"
  ) %>%
  arrange(group, period)

print(means_by_group)

write_csv(means_by_group, "05_tables/table_means_by_group.csv")


# -----------------------------------------------------------------------------
# 3. T-test: are coastal and inland filing rates significantly different?
# -----------------------------------------------------------------------------

coastal_rates <- ca_county_merged %>% filter(coastal == 1) %>% pull(filing_rate)
inland_rates  <- ca_county_merged %>% filter(coastal == 0) %>% pull(filing_rate)

t_test_result <- t.test(coastal_rates, inland_rates)
print(t_test_result)


# -----------------------------------------------------------------------------
# 4. Regression: what predicts eviction filing rates?
# -----------------------------------------------------------------------------

# Model 1: just coastal
m1 <- lm(filing_rate ~ coastal, data = ca_county_merged)

# Model 2: coastal + post2008
m2 <- lm(filing_rate ~ coastal + post2008, data = ca_county_merged)

# Model 3: coastal + post2008 + interaction
# The interaction term tells us: did the 2008 crisis affect coastal and
# inland counties differently?
m3 <- lm(filing_rate ~ coastal * post2008, data = ca_county_merged)

# Quick look at each model
summary(m1)
summary(m2)
summary(m3)


# -----------------------------------------------------------------------------
# 5. Display regression table with modelsummary
# -----------------------------------------------------------------------------

models <- list(
  "Model 1" = m1,
  "Model 2" = m2,
  "Model 3" = m3
)

modelsummary(
  models,
  stars     = TRUE,
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c(
    "coastal"          = "Coastal county",
    "post2008"         = "Post-2008",
    "coastal:post2008" = "Coastal × Post-2008"
  ),
  title   = "OLS Regression: Eviction Filing Rate",
  notes   = "Source: Eviction Lab. Unit of observation: county-year."
)

# Save plain-text version
sink("05_tables/table_regression.txt")
modelsummary(models, stars = TRUE,
             coef_rename = c(
               "coastal"          = "Coastal county",
               "post2008"         = "Post-2008",
               "coastal:post2008" = "Coastal × Post-2008"
             ))
sink()

message("Done! Tables saved to 05_tables/")
