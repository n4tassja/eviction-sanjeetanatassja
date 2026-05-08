# =============================================================================
# Student A: Maps and Graphs
# =============================================================================
# Input:  02_clean_data/ca_county_merged.RData
# Output: 04_graphs/map_avg_filing_rate.png
#         04_graphs/map_filing_rate_2010.png
#         04_graphs/timeseries_coastal_inland.png
#         04_graphs/top10_counties.png
#
# Run 03_analysis_scripts/00_merge.R first.
# =============================================================================

rm(list = ls())

library(pacman)

p_load(tidyverse, tidylog, janitor, tigris, sf)

options(tigris_use_cache = TRUE)

theme_set(theme_minimal(base_size = 12) + theme(
  plot.background  = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
))

load("02_clean_data/ca_county_merged.RData")


# -----------------------------------------------------------------------------
# 1. Get California county geometries from tigris
# -----------------------------------------------------------------------------

# tigris downloads Census shapefiles. cb = TRUE gives a simplified (smaller)
# version, which is faster to plot.

ca_sf <- counties(state = "CA", cb = TRUE, year = 2018, progress_bar = FALSE)

glimpse(ca_sf)

# The GEOID column in ca_sf matches our geoid column — both are "06001" format
head(ca_sf$GEOID)


# -----------------------------------------------------------------------------
# 2. Prepare map data: average filing rate per county across all years
# -----------------------------------------------------------------------------

ca_avg <- ca_county_merged %>%
  group_by(geoid, county, coastal) %>%
  summarise(
    avg_filing_rate = mean(filing_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Join geometry + data
ca_map <- ca_sf %>%
  left_join(ca_avg, by = c("GEOID" = "geoid"))


# -----------------------------------------------------------------------------
# 3. Map 1: Average eviction filing rate across all years
# -----------------------------------------------------------------------------

map1 <- ggplot(ca_map) +
  geom_sf(aes(fill = avg_filing_rate), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    name   = "Avg filing rate\n(% renting HH)",
    option = "magma",
    direction = -1
  ) +
  labs(
    title    = "Average Eviction Filing Rate by County",
    subtitle = "California, 2000–2018",
    caption  = "Source: Eviction Lab"
  ) +
  theme_void() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 11),
    legend.position  = "right",
    plot.background  = element_rect(fill = "white", color = NA)
  )

map1

ggsave("04_graphs/map_avg_filing_rate.png", map1, width = 7, height = 8, dpi = 150)


# -----------------------------------------------------------------------------
# 4. Map 2: Filing rate in 2010 (peak post-crisis year)
# -----------------------------------------------------------------------------

ca_2010 <- ca_county_merged %>%
  filter(year == 2010) %>%
  select(geoid, filing_rate)

ca_map_2010 <- ca_sf %>%
  left_join(ca_2010, by = c("GEOID" = "geoid"))

map2 <- ggplot(ca_map_2010) +
  geom_sf(aes(fill = filing_rate), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    name   = "Filing rate\n(% renting HH)",
    option = "magma",
    direction = -1
  ) +
  labs(
    title    = "Eviction Filing Rate by County, 2010",
    subtitle = "California — one year after the housing crisis",
    caption  = "Source: Eviction Lab"
  ) +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11),
    plot.background = element_rect(fill = "white", color = NA)
  )

map2

ggsave("04_graphs/map_filing_rate_2010.png", map2, width = 7, height = 8, dpi = 150)


# -----------------------------------------------------------------------------
# 5. Time series: coastal vs. inland average filing rate by year
# -----------------------------------------------------------------------------

ts_data <- ca_county_merged %>%
  mutate(group = if_else(coastal == 1, "Coastal", "Inland")) %>%
  group_by(year, group) %>%
  summarise(mean_rate = mean(filing_rate, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  filter(n_distinct(group) == 2) %>%
  ungroup()

plot_ts <- ggplot(ts_data, aes(x = year, y = mean_rate, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "gray40") +
  annotate("text", x = 2008.3, y = max(ts_data$mean_rate) * 0.97,
           label = "2008 crisis", hjust = 0, size = 3.5, color = "gray40") +
  scale_color_manual(values = c("Coastal" = "#2166ac", "Inland" = "#d6604d")) +
  scale_x_continuous(breaks = seq(2000, 2018, by = 2)) +
  labs(
    title   = "Eviction Filing Rates: Coastal vs. Inland Counties",
    subtitle = "California, 2010–2017 (years with data for both groups)",
    x       = NULL,
    y       = "Mean filing rate (% renting HH)",
    color   = NULL,
    caption = "Source: Eviction Lab"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    legend.position = "top"
  )

plot_ts

ggsave("04_graphs/timeseries_coastal_inland.png", plot_ts, width = 8, height = 5, dpi = 150)


# -----------------------------------------------------------------------------
# 6. Bar chart: top 10 counties by average filing rate
# -----------------------------------------------------------------------------

top10 <- ca_avg %>%
  slice_max(avg_filing_rate, n = 10) %>%
  mutate(
    county = fct_reorder(county, avg_filing_rate),
    group  = if_else(coastal == 1, "Coastal", "Inland")
  )

plot_top10 <- ggplot(top10, aes(x = avg_filing_rate, y = county, fill = group)) +
  geom_col() +
  scale_fill_manual(values = c("Coastal" = "#2166ac", "Inland" = "#d6604d")) +
  labs(
    title   = "Top 10 Counties by Average Eviction Filing Rate",
    subtitle = "California, 2000–2018",
    x       = "Average filing rate (% renting HH)",
    y       = NULL,
    fill    = NULL,
    caption = "Source: Eviction Lab"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    legend.position = "top"
  )

plot_top10

ggsave("04_graphs/top10_counties.png", plot_top10, width = 7, height = 5, dpi = 150)
