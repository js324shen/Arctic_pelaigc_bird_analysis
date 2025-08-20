rm(list = ls())

library(dplyr)
library(sf)

bird_data <- readRDS("bird_data_effort_standardized.rds")

# Data preparation
if(!"count_adjusted" %in% names(bird_data)) {
  bird_data$count_adjusted <- as.numeric(bird_data$`OBSERVATION COUNT`)
  bird_data$count_adjusted[bird_data$`OBSERVATION COUNT` == "X"] <- 1
}

if(!"month" %in% names(bird_data)) {
  bird_data$month <- as.numeric(format(bird_data$`OBSERVATION DATE`, "%m"))
}

if(!"season" %in% names(bird_data)) {
  bird_data$season <- case_when(
    bird_data$month %in% c(12, 1, 2) ~ "Winter",
    bird_data$month %in% c(3, 4, 5) ~ "Spring", 
    bird_data$month %in% c(6, 7, 8) ~ "Summer",
    bird_data$month %in% c(9, 10, 11) ~ "Autumn"
  )
}

bird_data$coast_dist_cat <- cut(bird_data$shore_dist/1000, 
                                breaks = c(0, 5, 25, 100, Inf),
                                labels = c("0-5km", "5-25km", "25-100km", ">100km"),
                                include.lowest = TRUE)

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")



# 1. TOP 15 SPECIES BY OBSERVATION COUNT
# NO CHANGE - This shows encounter frequency, not abundance

species_counts <- bird_data %>%
  group_by(`COMMON NAME`) %>%
  summarise(observations = n(), .groups = 'drop') %>%
  arrange(desc(observations)) %>%
  head(15)

total_observations <- nrow(bird_data)
species_counts$percentage <- round((species_counts$observations / total_observations) * 100, 2)

for(i in 1:nrow(species_counts)) {
  cat(sprintf("%2d. %-25s: %6d (%5.2f%%)\n", 
              i, species_counts$`COMMON NAME`[i], 
              species_counts$observations[i], 
              species_counts$percentage[i]))
}

cat("Top 15 represent:", round(sum(species_counts$percentage), 2), "% of all observations\n\n")


# 2. MONTHLY ABUNDANCE PATTERNS
# CHANGED - Now uses standardized abundance

monthly_abundance <- bird_data %>%
  group_by(month) %>%
  summarise(total_abundance = sum(abundance_standardized, na.rm = TRUE), .groups = 'drop')

for(i in 1:12) {
  abundance <- monthly_abundance$total_abundance[monthly_abundance$month == i]
  if(length(abundance) == 0) abundance <- 0
  cat(sprintf("%-3s: %8.2f birds/observer-hour\n", month_names[i], abundance))
}

summer_abundance <- sum(monthly_abundance$total_abundance[monthly_abundance$month %in% c(6,7,8)])
winter_abundance <- sum(monthly_abundance$total_abundance[monthly_abundance$month %in% c(12,1,2)])
summer_winter_ratio <- round(summer_abundance / winter_abundance, 2)

cat("Summer (Jun-Aug):", round(summer_abundance, 2), "| Winter (Dec-Feb):", round(winter_abundance, 2))
cat(" | Ratio:", summer_winter_ratio, "\n\n")


# 3. PEAK ABUNDANCE MONTH
# CHANGED - Based on standardized abundance

peak_month <- monthly_abundance$month[which.max(monthly_abundance$total_abundance)]
peak_value <- max(monthly_abundance$total_abundance)
cat("Peak:", month_names[peak_month], "with", round(peak_value, 2), "birds/observer-hour\n\n")


# 4. OBSERVER EFFORT
# NO CHANGE - This measures survey effort, not bird abundance

total_unique_observers <- n_distinct(bird_data$`OBSERVER ID`)
monthly_observers <- bird_data %>%
  group_by(month) %>%
  summarise(unique_observers = n_distinct(`OBSERVER ID`), .groups = 'drop')

peak_effort_month <- monthly_observers$month[which.max(monthly_observers$unique_observers)]
peak_effort_value <- max(monthly_observers$unique_observers)

cat("Total unique observers:", total_unique_observers, "\n")
cat("Peak effort:", month_names[peak_effort_month], "with", peak_effort_value, "observers\n\n")


# 5. LATITUDINAL DISTRIBUTION
# CHANGED - Now shows abundance patterns, not just observation patterns

# Overall statistics - still use lat coordinates for range
overall_lat <- bird_data %>%
  summarise(
    min_lat = round(min(lat_wgs84, na.rm = TRUE), 2),
    max_lat = round(max(lat_wgs84, na.rm = TRUE), 2),
    mean_lat = round(mean(lat_wgs84, na.rm = TRUE), 2),
    median_lat = round(median(lat_wgs84, na.rm = TRUE), 2)
  ) %>%
  mutate(range_span = max_lat - min_lat)

cat("OVERALL RANGE:", overall_lat$min_lat, "°N to", overall_lat$max_lat, "°N")
cat(" (span:", overall_lat$range_span, "°)\n")
cat("Mean:", overall_lat$mean_lat, "°N | Median:", overall_lat$median_lat, "°N\n")

# Peak concentration - CHANGED to use abundance
bird_data$lat_bin <- round(bird_data$lat_wgs84 * 2) / 2
lat_overall <- bird_data %>%
  group_by(lat_bin) %>%
  summarise(abundance = sum(abundance_standardized, na.rm = TRUE), .groups = 'drop')
overall_peak_lat <- lat_overall$lat_bin[which.max(lat_overall$abundance)]
overall_peak_abundance <- max(lat_overall$abundance)
cat("Peak abundance:", overall_peak_lat, "°N (", round(overall_peak_abundance, 2), "birds/observer-hour)\n")

# By pelagic type - CHANGED to use abundance
cat("\nBY PELAGIC TYPE:\n")
lat_ranges <- bird_data %>%
  group_by(pelagic_type) %>%
  summarise(
    min_lat = round(min(lat_wgs84, na.rm = TRUE), 2),
    max_lat = round(max(lat_wgs84, na.rm = TRUE), 2),
    mean_lat = round(mean(lat_wgs84, na.rm = TRUE), 2),
    total_abundance = sum(abundance_standardized, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(range_span = max_lat - min_lat)

# Peak by type - CHANGED to use abundance
lat_by_type <- bird_data %>%
  group_by(lat_bin, pelagic_type) %>%
  summarise(abundance = sum(abundance_standardized, na.rm = TRUE), .groups = 'drop')

for(type in unique(lat_ranges$pelagic_type)) {
  type_data <- lat_ranges[lat_ranges$pelagic_type == type, ]
  peak_data <- lat_by_type %>% filter(pelagic_type == type)
  peak_lat <- peak_data$lat_bin[which.max(peak_data$abundance)]
  peak_abundance <- max(peak_data$abundance)
  
  cat(sprintf("%-18s: %5.2f°N to %5.2f°N (span: %4.2f°, peak: %5.2f°N, %.2f birds/obs-hr)\n",
              type, type_data$min_lat, type_data$max_lat, 
              type_data$range_span, peak_lat, peak_abundance))
}
cat("\n")


# 6. DISTANCE FROM COASTLINE
# CHANGED - Now uses standardized abundance

# Overall distribution - CHANGED
coast_overall <- bird_data %>%
  group_by(coast_dist_cat) %>%
  summarise(
    abundance = sum(abundance_standardized, na.rm = TRUE),
    observations = n(),
    .groups = 'drop'
  ) %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 2))

cat("OVERALL DISTRIBUTION:\n")
for(i in 1:nrow(coast_overall)) {
  cat(sprintf("%-10s: %6.2f birds/obs-hr (%5.2f%%) [%d observations]\n",
              coast_overall$coast_dist_cat[i],
              coast_overall$abundance[i], 
              coast_overall$percentage[i],
              coast_overall$observations[i]))
}

# By pelagic type - CHANGED
cat("\nBY PELAGIC TYPE:\n")
coast_by_type <- bird_data %>%
  group_by(pelagic_type, coast_dist_cat) %>%
  summarise(
    abundance = sum(abundance_standardized, na.rm = TRUE),
    observations = n(),
    .groups = 'drop'
  ) %>%
  group_by(pelagic_type) %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 2))

for(type in unique(coast_by_type$pelagic_type)) {
  cat(sprintf("%s:\n", type))
  type_data <- coast_by_type[coast_by_type$pelagic_type == type, ]
  for(i in 1:nrow(type_data)) {
    cat(sprintf("  %-10s: %6.2f birds/obs-hr (%5.2f%%) [%d obs]\n",
                type_data$coast_dist_cat[i],
                type_data$abundance[i], 
                type_data$percentage[i],
                type_data$observations[i]))
  }
}
cat("\n")


# 7. PERCENTAGE WITHIN 25KM OF COASTLINE
# CHANGED - Now based on abundance, not observation count

within_25km_abundance <- sum(coast_overall$abundance[coast_overall$coast_dist_cat %in% c("0-5km", "5-25km")])
pct_within_25km <- round((within_25km_abundance / sum(coast_overall$abundance)) * 100, 2)

cat("Within 25km of coast:", round(within_25km_abundance, 2), "birds/obs-hr (", pct_within_25km, "%)\n\n")


# 8. LATITUDE RANGE FOR 90% OF ABUNDANCE
# CHANGED - Now shows where 90% of abundance occurs, not 90% of observations

# Create weighted quantiles based on abundance
bird_data <- st_drop_geometry(bird_data)

lat_abundance_data <- bird_data %>%
  dplyr::select(lat_wgs84, abundance_standardized) %>%
  dplyr::filter(!is.na(lat_wgs84), !is.na(abundance_standardized)) %>%
  dplyr::arrange(lat_wgs84) %>%
  dplyr::mutate(cum_abundance = cumsum(abundance_standardized),
                cum_pct = cum_abundance / sum(abundance_standardized))

# Find 5th and 95th percentiles
lat_5th <- lat_abundance_data$lat_wgs84[which.min(abs(lat_abundance_data$cum_pct - 0.05))]
lat_95th <- lat_abundance_data$lat_wgs84[which.min(abs(lat_abundance_data$cum_pct - 0.95))]
lat_range_90pct <- round(lat_95th - lat_5th, 2)

cat("5th percentile (abundance-weighted):", round(lat_5th, 2), "°N\n")
cat("95th percentile (abundance-weighted):", round(lat_95th, 2), "°N\n") 
cat("90% of abundance spans:", lat_range_90pct, "° latitude\n\n")


# 9. SEASONAL DISTRIBUTION COVERAGE
# CHANGED - Now shows abundance patterns, not just observation frequency

# Overall seasonal distribution - CHANGED
seasonal_overall <- bird_data %>%
  group_by(season) %>%
  summarise(
    abundance = sum(abundance_standardized, na.rm = TRUE),
    observations = n(),
    .groups = 'drop'
  ) %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 2)) %>%
  arrange(desc(abundance))

cat("OVERALL SEASONAL DISTRIBUTION:\n")
for(i in 1:nrow(seasonal_overall)) {
  cat(sprintf("%-7s: %6.2f birds/obs-hr (%5.2f%%) [%d observations]\n",
              seasonal_overall$season[i],
              seasonal_overall$abundance[i],
              seasonal_overall$percentage[i],
              seasonal_overall$observations[i]))
}

# By pelagic type - CHANGED
cat("\nBY PELAGIC TYPE:\n")
seasonal_by_type <- bird_data %>%
  group_by(pelagic_type, season) %>%
  summarise(
    abundance = sum(abundance_standardized, na.rm = TRUE),
    observations = n(),
    .groups = 'drop'
  ) %>%
  group_by(pelagic_type) %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 2))

for(type in unique(seasonal_by_type$pelagic_type)) {
  cat(sprintf("%s:\n", type))
  type_data <- seasonal_by_type[seasonal_by_type$pelagic_type == type, ] %>%
    arrange(desc(abundance))
  for(i in 1:nrow(type_data)) {
    cat(sprintf("  %-7s: %6.2f birds/obs-hr (%5.2f%%) [%d obs]\n",
                type_data$season[i],
                type_data$abundance[i],
                type_data$percentage[i],
                type_data$observations[i]))
  }
}
cat("\n")


# 10. SEASONAL SPATIAL COVERAGE
# NO CHANGE - This measures sampling coverage, not bird abundance

# Overall spatial coverage
seasonal_spatial <- bird_data %>%
  mutate(location_day = paste(round(lat_wgs84, 2), round(lon_wgs84, 2), 
                              format(`OBSERVATION DATE`, "%Y-%m-%d"), sep = "_")) %>%
  group_by(season) %>%
  summarise(
    unique_locations = n_distinct(location_day),
    observations = n(),
    .groups = 'drop'
  ) %>%
  mutate(coverage_ratio = round(unique_locations / observations, 3))

cat("OVERALL SPATIAL COVERAGE (unique location-days):\n")
for(i in 1:nrow(seasonal_spatial)) {
  cat(sprintf("%-7s: %5d unique locations (%5d obs, ratio: %.3f)\n", 
              seasonal_spatial$season[i], 
              seasonal_spatial$unique_locations[i],
              seasonal_spatial$observations[i],
              seasonal_spatial$coverage_ratio[i]))
}

max_coverage_season <- seasonal_spatial$season[which.max(seasonal_spatial$unique_locations)]
min_coverage_season <- seasonal_spatial$season[which.min(seasonal_spatial$unique_locations)]
max_coverage_value <- max(seasonal_spatial$unique_locations)
min_coverage_value <- min(seasonal_spatial$unique_locations)
coverage_ratio <- round(max_coverage_value / min_coverage_value, 2)

cat("\nMaximum coverage:", max_coverage_season, "(", max_coverage_value, "unique locations)\n")
cat("Minimum coverage:", min_coverage_season, "(", min_coverage_value, "unique locations)\n")
cat("Max/Min ratio:", coverage_ratio, "\n\n")


# SUMMARY 

cat("=== SUMMARY ===\n")
cat("Total observations:", total_observations, "\n")
cat("Total species:", n_distinct(bird_data$`COMMON NAME`), "\n")
cat("Total observers:", total_unique_observers, "\n")
cat("Study period:", min(bird_data$`OBSERVATION DATE`, na.rm = TRUE), "to", max(bird_data$`OBSERVATION DATE`, na.rm = TRUE), "\n")

tp_count <- sum(bird_data$pelagic_type == "True Pelagic")
pp_count <- sum(bird_data$pelagic_type == "Partially Pelagic")
tp_pct <- round((tp_count / total_observations) * 100, 1)
pp_pct <- round((pp_count / total_observations) * 100, 1)

# ADDED - Summary of abundance by type
tp_abundance <- sum(bird_data$abundance_standardized[bird_data$pelagic_type == "True Pelagic"], na.rm = TRUE)
pp_abundance <- sum(bird_data$abundance_standardized[bird_data$pelagic_type == "Partially Pelagic"], na.rm = TRUE)
total_abundance <- tp_abundance + pp_abundance
tp_abundance_pct <- round((tp_abundance / total_abundance) * 100, 1)
pp_abundance_pct <- round((pp_abundance / total_abundance) * 100, 1)

cat("\nOBSERVATION FREQUENCY:\n")
cat("True Pelagic:", tp_count, "obs (", tp_pct, "%) | Partially Pelagic:", pp_count, "obs (", pp_pct, "%)\n")
cat("\nSTANDARDIZED ABUNDANCE:\n")
cat("True Pelagic:", round(tp_abundance, 2), "birds/obs-hr (", tp_abundance_pct, "%) | Partially Pelagic:", round(pp_abundance, 2), "birds/obs-hr (", pp_abundance_pct, "%)\n")

