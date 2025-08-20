rm(list = ls())

library(dplyr)
library(sf)
library(ggplot2)

# Load data
bird_data <- readRDS("bird_data_effort_standardized.rds")
gistemp <- read.csv("ZonAnn.Ts+dSST.csv")

# Prepare Arctic temperature data
arctic_temp <- gistemp %>%
  filter(Year >= 2003 & Year <= 2024) %>%
  mutate(arctic_temp_anomaly = (`X44N.64N` + `X64N.90N`) / 2) %>%
  select(year = Year, arctic_temp_anomaly)

# Create hexagonal grid (50km spacing)
arctic_bbox <- st_bbox(bird_data)
hex_grid <- st_make_grid(bird_data, cellsize = 50000, square = FALSE) %>%
  st_sf(grid_id = 1:length(.)) %>%
  st_intersection(st_union(bird_data))

# Function to calculate Schoener's D
calculate_schoener_d <- function(density1, density2) {
  sum(pmin(density1, density2))
}

# Prepare bird data and intersect with grid
bird_prep <- bird_data %>%
  filter(year >= 2003 & year <= 2024) %>%
  filter(!is.na(ice_dist) & !is.na(abundance_standardized)) %>%
  group_by(year, lat_wgs84, lon_wgs84, pelagic_type) %>%
  summarise(
    standardized_abundance = sum(abundance_standardized, na.rm = TRUE),
    n_checklists = n_distinct(paste(`OBSERVATION DATE`, `LOCALITY ID`)),
    ice_dist_km = mean(ice_dist/1000, na.rm = TRUE),
    geometry = first(geometry),
    .groups = 'drop'
  ) %>%
  filter(standardized_abundance > 0) %>%
  st_sf()

# Intersect bird data with hexagonal grid
bird_grid <- st_intersection(bird_prep, hex_grid)

# Calculate annual grid densities
grid_densities <- bird_grid %>%
  st_drop_geometry() %>%
  group_by(year, grid_id, pelagic_type) %>%
  summarise(
    grid_abundance = sum(standardized_abundance, na.rm = TRUE),
    grid_checklists = sum(n_checklists),
    mean_ice_dist = mean(ice_dist_km, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(grid_checklists >= 2)

# Convert to probability densities per year
annual_densities <- grid_densities %>%
  group_by(year, pelagic_type) %>%
  mutate(
    total_abundance = sum(grid_abundance, na.rm = TRUE),
    probability_density = grid_abundance / total_abundance
  ) %>%
  ungroup() %>%
  filter(total_abundance > 0)

# Calculate Schoener's D between consecutive years
years <- 2003:2024
schoener_results <- data.frame()

for (species_type in c("True Pelagic", "Partially Pelagic")) {
  for (i in 1:(length(years)-1)) {
    year1 <- years[i]
    year2 <- years[i+1]
    
    # Get densities for both years
    density1 <- annual_densities %>%
      filter(year == year1, pelagic_type == species_type) %>%
      select(grid_id, probability_density)
    
    density2 <- annual_densities %>%
      filter(year == year2, pelagic_type == species_type) %>%
      select(grid_id, probability_density)
    
    # Merge and fill missing grids with 0
    all_grids <- full_join(density1, density2, by = "grid_id", suffix = c("_y1", "_y2"))
    all_grids[is.na(all_grids)] <- 0
    
    # Calculate Schoener's D
    schoener_d <- calculate_schoener_d(all_grids$probability_density_y1, 
                                       all_grids$probability_density_y2)
    
    # Store result
    schoener_results <- rbind(schoener_results, data.frame(
      year_from = year1,
      year_to = year2,
      pelagic_type = species_type,
      schoener_d = schoener_d
    ))
  }
}

# Add environmental predictors
schoener_results <- schoener_results %>%
  left_join(arctic_temp, by = c("year_to" = "year")) %>%
  mutate(year_numeric = year_to - 2003)

# Calculate annual ice metrics
annual_ice_metrics <- bird_data %>%
  st_drop_geometry() %>%
  filter(year >= 2003 & year <= 2024) %>%
  group_by(year) %>%
  summarise(
    fast_ice_extent = mean(ice_dist/1000, na.rm = TRUE),
    .groups = 'drop'
  )

schoener_results <- schoener_results %>%
  left_join(annual_ice_metrics, by = c("year_to" = "year")) %>%
  filter(!is.na(arctic_temp_anomaly) & !is.na(fast_ice_extent))

# GLM analysis
# Transform Schoener's D for better model fit (handle exact zeros)
schoener_results$schoener_d_adj <- pmax(schoener_results$schoener_d, 0.001)
schoener_results$schoener_d_logit <- log(schoener_results$schoener_d_adj / (1 - schoener_results$schoener_d_adj + 0.001))

# Convert to factor for GLM
schoener_results$pelagic_type <- as.factor(schoener_results$pelagic_type)

# Filter complete cases
schoener_analysis <- schoener_results %>%
  filter(!is.na(arctic_temp_anomaly) & !is.na(fast_ice_extent) & 
           !is.infinite(schoener_d_logit))

# Main GLM
distribution_glm <- glm(schoener_d_logit ~ 
                          arctic_temp_anomaly + 
                          fast_ice_extent + 
                          pelagic_type + 
                          year_numeric +
                          arctic_temp_anomaly:pelagic_type +
                          fast_ice_extent:pelagic_type,
                        data = schoener_analysis,
                        family = gaussian())

# Results
summary(distribution_glm)
confint(distribution_glm)

# Diagnostic plots
par(mfrow = c(2,2))
plot(distribution_glm)

# Check for normality
hist(residuals(distribution_glm))

# Model comparison
null_model <- glm(schoener_d_logit ~ pelagic_type + year_numeric, 
                  data = schoener_analysis, family = gaussian())

climate_model <- glm(schoener_d_logit ~ 
                       arctic_temp_anomaly + 
                       fast_ice_extent + 
                       pelagic_type + 
                       year_numeric,
                     data = schoener_analysis, family = gaussian())

anova(null_model, climate_model, distribution_glm, test = "F")

# Visualization (Annual Distribution Change in Arctic Pelagic Birds)
schoener_plot <- ggplot(schoener_analysis, aes(x = year_to, y = schoener_d, 
                                               color = pelagic_type)) +
  geom_line(size = 1.0, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  labs(x = "Year", y = "Schoener's D (Distribution Similarity)",
       color = "Species Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Partially Pelagic" = "coral", 
                                "True Pelagic" = "steelblue"))

print(schoener_plot)


# Save results
write.csv(schoener_analysis, "schoener_d_results.csv", row.names = FALSE)
write.csv(summary(distribution_glm)$coefficients, "distribution_glm_coefficients.csv")

cat("\n=== HOTSPOT MIGRATION ANALYSIS ===\n")

# Get grid coordinates
grid_coords <- bird_grid %>% 
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    grid_lat = mean(lat_wgs84, na.rm = TRUE),
    grid_lon = mean(lon_wgs84, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate annual hotspot centroids using 75th percentile
hotspot_centroids <- grid_densities %>%
  left_join(grid_coords, by = "grid_id") %>%
  filter(!is.na(grid_lat) & grid_checklists >= 2) %>%
  group_by(year, pelagic_type) %>%
  # Define hotspots as top 25% (75th percentile)
  mutate(threshold = quantile(grid_abundance, 0.75, na.rm = TRUE)) %>%
  filter(grid_abundance >= threshold) %>%
  # Calculate weighted centroid
  summarise(
    weighted_lat = sum(grid_lat * grid_abundance, na.rm = TRUE) / sum(grid_abundance, na.rm = TRUE),
    weighted_lon = sum(grid_lon * grid_abundance, na.rm = TRUE) / sum(grid_abundance, na.rm = TRUE),
    n_hotspot_grids = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(weighted_lat) & n_hotspot_grids >= 3)

# Calculate migration trends
migration_analysis <- hotspot_centroids %>%
  arrange(pelagic_type, year) %>%
  group_by(pelagic_type) %>%
  mutate(lat_change_km = (weighted_lat - lag(weighted_lat)) * 111) %>%
  ungroup()

# Migration plot (Arctic Pelagic Bird Hotspot Migration)
migration_plot <- ggplot(migration_analysis, aes(x = year, y = weighted_lat, 
                                                 color = pelagic_type)) +
  geom_line(size = 1.0, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3, linetype = "dashed") +
  scale_color_manual(values = c("Partially Pelagic" = "coral", 
                                "True Pelagic" = "steelblue")) +
  labs(x = "Year", y = "Hotspot Centroid Latitude (°N)",
       color = "Species Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(migration_plot)

# Quick stats
migration_stats <- migration_analysis %>%
  group_by(pelagic_type) %>%
  summarise(
    n_years = n(),
    total_shift_km = (max(weighted_lat) - min(weighted_lat)) * 111,
    movement_direction = ifelse(cor(year, weighted_lat, use = "complete") > 0, "Northward", "Southward"),
    .groups = 'drop'
  )

print("Migration summary:")
print(migration_stats)

# === COMPLETE DATA LISTINGS FOR REPORTING ===

cat("\n=== ALL SCHOENER'S D VALUES ===\n")

# List all Schoener's D values by year
schoener_all <- schoener_analysis %>%
  select(year_to, pelagic_type, schoener_d) %>%
  mutate(schoener_d = round(schoener_d, 3)) %>%
  arrange(pelagic_type, year_to)

for(species in c("Partially Pelagic", "True Pelagic")) {
  cat(sprintf("\n%s:\n", species))
  species_data <- schoener_all %>% filter(pelagic_type == species)
  for(i in 1:nrow(species_data)) {
    cat(sprintf("  %d: %.3f\n", species_data$year_to[i], species_data$schoener_d[i]))
  }
}

cat("\n=== ALL HOTSPOT LATITUDE VALUES ===\n")

# List all hotspot centroid latitudes by year  
hotspot_all <- hotspot_centroids %>%
  select(year, pelagic_type, weighted_lat) %>%
  mutate(weighted_lat = round(weighted_lat, 2)) %>%
  arrange(pelagic_type, year)

for(species in c("Partially Pelagic", "True Pelagic")) {
  cat(sprintf("\n%s Hotspot Centroids:\n", species))
  species_data <- hotspot_all %>% filter(pelagic_type == species)
  for(i in 1:nrow(species_data)) {
    cat(sprintf("  %d: %.2f°N\n", species_data$year[i], species_data$weighted_lat[i]))
  }
}

# Save complete datasets
write.csv(schoener_all, "all_schoener_d_values.csv", row.names = FALSE)
write.csv(hotspot_all, "all_hotspot_latitudes.csv", row.names = FALSE)
