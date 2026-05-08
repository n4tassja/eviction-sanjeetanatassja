# =============================================================================
# Student A: County-Level Eviction Data Cleaning
# =============================================================================
# Input:  00_raw_data/county_court-issued_2000_2018.csv
# Output: 02_clean_data/ca_county_evictions.RData
#
# Your job: clean and prepare county-level eviction data for California.
# Your cleaned file will be used later for time series plots, summary tables,
# and regressions. Student B is working in parallel on tract-level data.
# =============================================================================

rm(list = ls())

library(pacman)

p_load(tidyverse, tidylog,janitor)

# -----------------------------------------------------------------------------
# 1. Load the raw data
# -----------------------------------------------------------------------------

county_raw <- read_csv("00_raw_data/county_court-issued_2000_2018.csv")

# Take a look at what you have
glimpse(county_raw)


# -----------------------------------------------------------------------------
# 2. Filter to California only
# -----------------------------------------------------------------------------

# TASK: Keep only rows where state == "California"
ca_county <- county_raw %>%
  filter(state == "California")

# How many unique counties?
#nrow(ca_county)
n_distinct(ca_county$county)


# -----------------------------------------------------------------------------
# 3. Clean up the county name column
# -----------------------------------------------------------------------------

# The county column contains values like "Los Angeles County".
# Remove the word " County" so it just says "Los Angeles".


ca_county <- ca_county %>%
  mutate(county = str_remove(county, " County"))

# Check it worked
unique(ca_county$county)


# -----------------------------------------------------------------------------
# 4. Fix the county FIPS code
# -----------------------------------------------------------------------------

# The variable fips_county contains values like 6001, 6037, 6075.
# For mapping later, we need a 5-character string with a leading zero: "06001".
# Hint: use str_pad(fips_county, width = 5, pad = "0")

ca_county <- ca_county %>%
  mutate(geoid = str_pad(as.character(fips_county), width = 5, pad = "0"))

# Verify: all values should start with "06"
unique(ca_county$geoid)



# -----------------------------------------------------------------------------
# 5. Compute the eviction filing rate
# -----------------------------------------------------------------------------

# filing_rate = filings_observed / renting_hh * 100
# This gives us the percentage of renting households that received a filing.
# Some rows have missing filings_observed — leave those as NA for now.

ca_county <- ca_county %>%
  mutate(filing_rate = filings_observed / renting_hh * 100)

# Sanity check: what is the range of filing_rate?
summary(ca_county$filing_rate)


# -----------------------------------------------------------------------------
# 6. Create a coastal county indicator
# -----------------------------------------------------------------------------

# California has 15 counties that border the Pacific Ocean.
# We will use this as a grouping variable later.

coastal_counties <- c(
  "Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin",
  "San Francisco", "San Mateo", "Santa Cruz", "Monterey",
  "San Luis Obispo", "Santa Barbara", "Ventura",
  "Los Angeles", "Orange", "San Diego"
)

ca_county <- ca_county %>%
  mutate(coastal = if_else(county %in% coastal_counties, 1L, 0L))


ca_county %>% count(coastal)


# -----------------------------------------------------------------------------
# 7. Create a pre/post 2008 financial crisis indicator
# -----------------------------------------------------------------------------

# The 2008 housing crisis hit renters hard. We'll compare evictions before
# and after 2008 (2008 itself is coded as "post").

ca_county <- ca_county %>%
  mutate(post2008 = if_else(year >= 2008, 1, 0))


# -----------------------------------------------------------------------------
# 8. Save to 02_clean_data, save as RData
# -----------------------------------------------------------------------------

save(ca_county, file = "02_clean_data/ca_county_evictions.RData")


