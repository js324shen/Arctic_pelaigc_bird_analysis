rm(list = ls())

library(sf)
library(dplyr)

sf_use_s2(FALSE)
ice_data <- readRDS("nsidc/processed_data/fast_ice_for_birds.rds")

cat("=== CHECKING FOR TEMPORAL ICE DATA ===\n")

# Check for the original combined file
if(file.exists("processed_data/all_fast_ice_data.rds")) {
  cat("Found: all_fast_ice_data.rds\n")
  original_ice <- readRDS("processed_data/all_fast_ice_data.rds")
  
  cat("Structure of original ice data:\n")
  str(original_ice)
  
  # Check if it has temporal columns
  if("year" %in% names(original_ice)) {
    cat("\n✓ HAS YEAR DATA!\n")
    cat("Years available:", sort(unique(original_ice$year)), "\n")
    cat("Total observations:", nrow(original_ice), "\n")
    
    # Show sample of data
    cat("\nSample data:\n")
    print(head(original_ice %>% st_drop_geometry() %>% 
                 select(any_of(c("date", "year", "month", "season")))))
  } else {
    cat("\n✗ No year column found in this file\n")
  }
  
} else if(file.exists("processed_data/all_fast_ice_data_list.rds")) {
  cat("Found: all_fast_ice_data_list.rds (list format)\n")
  original_ice_list <- readRDS("processed_data/all_fast_ice_data_list.rds")
  
  cat("Number of datasets in list:", length(original_ice_list), "\n")
  
  if(length(original_ice_list) > 0) {
    # Check first dataset
    first_dataset <- original_ice_list[[1]]
    cat("Structure of first dataset:\n")
    str(first_dataset)
    
    if("year" %in% names(first_dataset)) {
      cat("\n✓ HAS YEAR DATA!\n")
      
      # Extract years from all datasets
      all_years <- unique(unlist(lapply(original_ice_list, function(x) {
        if("year" %in% names(x)) x$year else NULL
      })))
      
      cat("Years available:", sort(all_years), "\n")
      cat("Total datasets:", length(original_ice_list), "\n")
    }
  }
  
} else {
  cat("Original combined file not found. Checking for batch files...\n")
  
  # Check for batch files
  batch_files <- list.files("processed_data", pattern = "fast_ice_batch_.*\\.rds", full.names = TRUE)
  
  if(length(batch_files) > 0) {
    cat("Found", length(batch_files), "batch files\n")
    
    # Check first batch
    first_batch <- readRDS(batch_files[1])
    if(length(first_batch) > 0 && !is.null(first_batch[[1]])) {
      cat("Structure of first dataset in first batch:\n")
      str(first_batch[[1]])
      
      if("year" %in% names(first_batch[[1]])) {
        cat("\n✓ BATCH FILES HAVE YEAR DATA!\n")
        cat("We can recreate the temporal dataset from batch files\n")
      }
    }
  } else {
    cat("No batch files found either\n")
  }
}

# Also check what you currently have loaded
cat("\n=== CURRENT ICE DATA ===\n")
if(exists("ice_data")) {
  cat("Current ice_data structure:\n")
  str(ice_data)
  
  if("seasonal" %in% names(ice_data) && "year" %in% names(ice_data$seasonal)) {
    cat("Current ice_data has year information!\n")
  } else {
    cat("Current ice_data appears to be seasonal averages only\n")
  }
} else {
  cat("No ice_data currently loaded\n")
}

# Check what files exist in processed_data directory
cat("\n=== FILES IN PROCESSED_DATA ===\n")
if(dir.exists("nsidc/processed_data")) {
  all_files <- list.files("nsidc/processed_data", full.names = TRUE)
  cat("Files found:\n")
  for(file in all_files) {
    cat(" -", basename(file), "(", round(file.size(file)/1024/1024, 1), "MB )\n")
  }
} else {
  cat("processed_data directory not found\n")
}


# Check the original temporal ice data

cat("=== CHECKING TEMPORAL ICE DATA ===\n")

# Load the full temporal ice dataset
temporal_ice <- readRDS("nsidc/processed_data/all_fast_ice_data.rds")

cat("Loaded temporal ice data successfully!\n")
cat("Object class:", class(temporal_ice), "\n")
cat("Number of observations:", nrow(temporal_ice), "\n")

# Check structure
cat("\nColumn names:\n")
print(names(temporal_ice))

# Check temporal coverage
if("year" %in% names(temporal_ice)) {
  years_available <- sort(unique(temporal_ice$year))
  cat("\n✓ YEARS AVAILABLE:", paste(years_available, collapse = ", "), "\n")
  cat("Total years:", length(years_available), "\n")
  
  # Check data per year
  yearly_counts <- temporal_ice %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    summarise(n_features = n(), .groups = 'drop')
  
  cat("\nData per year:\n")
  print(yearly_counts)
}

if("date" %in% names(temporal_ice)) {
  date_range <- range(temporal_ice$date, na.rm = TRUE)
  cat("\nDate range:", format(date_range[1]), "to", format(date_range[2]), "\n")
}

if("month" %in% names(temporal_ice)) {
  cat("Months available:", paste(sort(unique(temporal_ice$month)), collapse = ", "), "\n")
}

if("season" %in% names(temporal_ice)) {
  cat("Seasons available:", paste(unique(temporal_ice$season), collapse = ", "), "\n")
}

# Sample of the data
cat("\nSample data (first 5 rows):\n")
sample_data <- temporal_ice %>%
  st_drop_geometry() %>%
  select(any_of(c("date", "year", "month", "season", "fast", "fast_ice_present"))) %>%
  head(5)
print(sample_data)

# Check coordinate system
cat("\nCoordinate system:\n")
print(st_crs(temporal_ice))













# Load data
bird_data <- readRDS("arctic_faroe_cleaned_2025-06-10.rds")

cat("=== ANNUAL ICE DISTANCE CALCULATION ===\n")
cat("Bird observations:", nrow(bird_data), "\n")
cat("Temporal ice data:", nrow(temporal_ice), "\n")

# Check temporal coverage
years_ice <- sort(unique(temporal_ice$year))
years_birds <- sort(unique(bird_data$year))
overlapping_years <- intersect(years_ice, years_birds)

cat("Ice data years:", paste(range(years_ice), collapse = "-"), "\n")
cat("Bird data years:", paste(range(years_birds), collapse = "-"), "\n") 
cat("Overlapping years:", length(overlapping_years), "years\n")

# Initialize ice distance column
bird_data$ice_dist <- NA

# Calculate distances year by year
for(year_val in overlapping_years) {
  cat("Processing year:", year_val, "\n")
  
  # Get birds for this year
  birds_year <- bird_data[bird_data$year == year_val, ]
  
  # Get ice for this year
  ice_year <- temporal_ice[temporal_ice$year == year_val, ]
  
  if(nrow(ice_year) == 0) {
    cat("  No ice data for", year_val, "\n")
    next
  }
  
  if(nrow(birds_year) == 0) {
    cat("  No bird data for", year_val, "\n") 
    next
  }
  
  # Get ice edge (boundary of ice polygons)
  ice_edge <- st_boundary(st_make_valid(ice_year))
  
  # Ensure same CRS
  if(st_crs(birds_year) != st_crs(ice_edge)) {
    birds_year <- st_transform(birds_year, st_crs(ice_edge))
  }
  
  # Calculate distances
  cat("  Calculating distances for", nrow(birds_year), "birds...\n")
  distances <- st_distance(birds_year, ice_edge)
  min_distances <- as.numeric(apply(distances, 1, min))
  
  # Update bird_data
  bird_data$ice_dist[bird_data$year == year_val] <- min_distances
  
  cat("  Completed - assigned", length(min_distances), "distances\n")
  
  # Clean up memory
  rm(distances, min_distances, ice_edge, ice_year, birds_year)
  gc()
}

# Check results
n_with_ice <- sum(!is.na(bird_data$ice_dist))
n_total <- nrow(bird_data)
cat("\nResults:\n")
cat("Ice distances calculated:", n_with_ice, "of", n_total, "observations\n")
cat("Coverage:", round(n_with_ice/n_total*100, 1), "%\n")

if(n_with_ice > 0) {
  # Summary statistics
  cat("\nIce distance summary (meters):\n")
  print(summary(bird_data$ice_dist))
  
  cat("\nIce distance summary (km):\n")
  ice_dist_km <- bird_data$ice_dist / 1000
  print(summary(ice_dist_km))
  
  # Create distance categories  
  bird_data$ice_dist_cat <- cut(bird_data$ice_dist/1000, 
                                breaks = c(0, 25, 50, 100, 200, Inf),
                                labels = c("0-25km", "25-50km", "50-100km", "100-200km", ">200km"),
                                include.lowest = TRUE)
  
  cat("\nIce distance categories:\n")
  print(table(bird_data$ice_dist_cat, useNA = "always"))
  
  # Compare with coastal distances
  if("shore_dist" %in% names(bird_data)) {
    cat("\nDistance comparison:\n")
    cat("Mean coastal distance:", round(mean(bird_data$shore_dist/1000, na.rm = TRUE), 1), "km\n")
    cat("Mean ice distance:", round(mean(bird_data$ice_dist/1000, na.rm = TRUE), 1), "km\n")
  }
}

# Save updated dataset
saveRDS(bird_data, "bird_data_with_annual_ice_distance.rds")

# Show years with ice distance data
if(n_with_ice > 0) {
  ice_coverage_by_year <- bird_data %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    summarise(
      total_birds = n(),
      with_ice_dist = sum(!is.na(ice_dist)),
      coverage_pct = round(with_ice_dist/total_birds*100, 1),
      .groups = 'drop'
    ) %>%
    filter(with_ice_dist > 0)
  
  cat("\nYearly coverage:\n")
  print(ice_coverage_by_year)
}




# Descriptive data calculation
# Overall ice distance distribution
rm(list = ls())

bird_data <- readRDS("bird_data_effort_standardized.rds")

# 1. OVERALL ICE DISTANCE DISTRIBUTION
# CHANGED - Now uses abundance for percentages, keeps observations for reference

ice_overall <- bird_data %>%
  filter(!is.na(ice_dist_cat)) %>%
  group_by(ice_dist_cat) %>%
  summarise(abundance = sum(abundance_standardized, na.rm = TRUE),
            observations = n(), .groups = 'drop') %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 2))  # CHANGED: Now based on abundance

cat("ICE EDGE DISTANCE DISTRIBUTION:\n")
for(i in 1:nrow(ice_overall)) {
  cat(sprintf("%-12s: %6.2f birds/obs-hr (%5.2f%%) [%d observations]\n",  # CHANGED: Show abundance first
              ice_overall$ice_dist_cat[i],
              ice_overall$abundance[i], 
              ice_overall$percentage[i],
              ice_overall$observations[i]))
}

# 2. BY PELAGIC TYPE  
# CHANGED - Now uses abundance for percentages
cat("\nBY PELAGIC TYPE:\n")
ice_by_type <- bird_data %>%
  filter(!is.na(ice_dist_cat)) %>%
  group_by(pelagic_type, ice_dist_cat) %>%
  summarise(abundance = sum(abundance_standardized, na.rm = TRUE),
            observations = n(), .groups = 'drop') %>%
  group_by(pelagic_type) %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 2))  # CHANGED: Now based on abundance

for(type in unique(ice_by_type$pelagic_type)) {
  cat(sprintf("%s:\n", type))
  type_data <- ice_by_type[ice_by_type$pelagic_type == type, ]
  for(i in 1:nrow(type_data)) {
    cat(sprintf("  %-12s: %6.2f birds/obs-hr (%5.2f%%) [%d obs]\n",  # CHANGED: Show abundance first
                type_data$ice_dist_cat[i],
                type_data$abundance[i], 
                type_data$percentage[i],
                type_data$observations[i]))
  }
}

# 3. SEASONAL CHECK
# NO CHANGE - This is about data coverage, not abundance patterns
cat("\n=== SEASONAL CHECK ===\n")
bird_seasons <- table(bird_data$season)
print("Bird observations by season:")
print(bird_seasons)

# 4. YEARLY COVERAGE
# NO CHANGE - This is about data quality/coverage
cat("\n=== YEARLY COVERAGE ===\n")
yearly_ice_coverage <- bird_data %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    total_birds = n(),
    with_ice_dist = sum(!is.na(ice_dist)),
    coverage_pct = round(with_ice_dist/total_birds*100, 1),
    .groups = 'drop'
  ) %>%
  filter(with_ice_dist > 0)

print("Years with ice distance data:")
print(yearly_ice_coverage)

# 5. DISTANCE COMPARISON
# NO CHANGE - This is about data coverage and distance relationships
cat("\n=== DISTANCE COMPARISON ===\n")
both_distances <- bird_data %>%
  st_drop_geometry() %>%
  filter(!is.na(shore_dist) & !is.na(ice_dist)) %>%
  mutate(
    coast_km = shore_dist/1000,
    ice_km = ice_dist/1000,
    ice_closer = ice_km < coast_km
  )

cat("Birds with both distances:", nrow(both_distances), "\n")
cat("Ice closer than coast:", sum(both_distances$ice_closer), "birds (", 
    round(sum(both_distances$ice_closer)/nrow(both_distances)*100, 1), "%)\n")

cat("\nDistance summaries:\n")
cat("Coast - Mean:", round(mean(both_distances$coast_km), 1), "km, Median:", round(median(both_distances$coast_km), 1), "km\n")
cat("Ice - Mean:", round(mean(both_distances$ice_km), 1), "km, Median:", round(median(both_distances$ice_km), 1), "km\n")

# 6. SAMPLE COMPARISONS
# NO CHANGE - This is just showing example data
cat("\n=== SAMPLE COMPARISONS ===\n")
sample_data <- both_distances %>%
  select(year, season, lat_wgs84, lon_wgs84, coast_km, ice_km) %>%
  sample_n(10)
print(sample_data)

# 7. GEOGRAPHIC CHECK
# NO CHANGE - This is about data coverage
cat("\n=== GEOGRAPHIC CHECK ===\n")
cat("Latitude range of birds:", round(min(bird_data$lat_wgs84, na.rm = TRUE), 1), "to", round(max(bird_data$lat_wgs84, na.rm = TRUE), 1), "°N\n")

# Check if ice distances vary by latitude
lat_ice_summary <- bird_data %>%
  st_drop_geometry() %>%
  filter(!is.na(ice_dist)) %>%
  mutate(lat_group = cut(lat_wgs84, breaks = 5)) %>%
  group_by(lat_group) %>%
  summarise(
    mean_ice_km = round(mean(ice_dist/1000), 1),
    n_birds = n(),
    .groups = 'drop'
  )

cat("\nIce distance by latitude:\n")
print(lat_ice_summary)

# 8. SUMMARY WITHIN 100KM OF ICE EDGE
# CHANGED - Now uses abundance instead of observations
within_100km_ice_abundance <- sum(ice_overall$abundance[ice_overall$ice_dist_cat %in% c("0-25km", "25-50km", "50-100km")])
pct_within_100km_ice <- round((within_100km_ice_abundance / sum(ice_overall$abundance)) * 100, 2)

cat("\nWithin 100km of ice edge:", round(within_100km_ice_abundance, 2), "birds/obs-hr (", pct_within_100km_ice, "%)\n")

# 9. DISTANCE SUMMARIES
# NO CHANGE - These are about actual distance values, not abundance patterns
cat("\nICE DISTANCE SUMMARY:\n")
cat("Mean ice distance:", round(mean(bird_data$ice_dist/1000, na.rm = TRUE), 1), "km\n")
cat("Median ice distance:", round(median(bird_data$ice_dist/1000, na.rm = TRUE), 1), "km\n")

# Species composition within 100km of ice
ice_species <- bird_data %>%
  filter(!is.na(ice_dist_cat), ice_dist_cat %in% c("0-25km", "25-50km", "50-100km")) %>%
  group_by(`COMMON NAME`) %>%
  summarise(
    abundance_near_ice = sum(abundance_standardized, na.rm = TRUE),
    observations = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(abundance_near_ice)) %>%
  head(15)

# Calculate what % of each species' total abundance is near ice
species_ice_dependency <- bird_data %>%
  filter(!is.na(ice_dist_cat)) %>%
  group_by(`COMMON NAME`) %>%
  summarise(
    total_abundance = sum(abundance_standardized, na.rm = TRUE),
    near_ice_abundance = sum(abundance_standardized[ice_dist_cat %in% c("0-25km", "25-50km", "50-100km")], na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    ice_dependency = round((near_ice_abundance / total_abundance) * 100, 1)
  ) %>%
  filter(total_abundance > 50) %>%  # Only species with decent sample size
  arrange(desc(ice_dependency)) %>%
  head(15)

cat("TOP SPECIES NEAR ICE (by abundance):\n")
print(ice_species)
cat("\nMOST ICE-DEPENDENT SPECIES (% of their abundance near ice):\n") 
print(species_ice_dependency)

# How do True vs Partially Pelagic species use ice edges?
pelagic_ice <- bird_data %>%
  filter(!is.na(ice_dist_cat)) %>%
  mutate(near_ice = ice_dist_cat %in% c("0-25km", "25-50km", "50-100km")) %>%
  group_by(pelagic_type, near_ice) %>%
  summarise(abundance = sum(abundance_standardized, na.rm = TRUE), .groups = 'drop') %>%
  group_by(pelagic_type) %>%
  mutate(percentage = round((abundance / sum(abundance)) * 100, 1))

cat("ICE ASSOCIATION BY PELAGIC TYPE:\n")
print(pelagic_ice)









# 10. BY PELAGIC TYPE SUMMARY
# NO CHANGE - These are distance summaries, not abundance patterns
ice_summary_by_type <- bird_data %>%
  filter(!is.na(ice_dist)) %>%
  group_by(pelagic_type) %>%
  summarise(
    mean_ice_dist = round(mean(ice_dist/1000), 1),
    median_ice_dist = round(median(ice_dist/1000), 1),
    observations = n(),
    .groups = 'drop'
  )

cat("\nBY PELAGIC TYPE SUMMARY:\n")
for(i in 1:nrow(ice_summary_by_type)) {
  cat(sprintf("%-18s: Mean = %5.1f km, Median = %5.1f km (%d obs)\n",
              ice_summary_by_type$pelagic_type[i],
              ice_summary_by_type$mean_ice_dist[i],
              ice_summary_by_type$median_ice_dist[i],
              ice_summary_by_type$observations[i]))
}

# 11. ADDITIONAL ABUNDANCE-BASED SUMMARY
# ADDED - Show abundance patterns by pelagic type
cat("\nABUNDANCE BY ICE DISTANCE & PELAGIC TYPE:\n")
abundance_by_type <- bird_data %>%
  filter(!is.na(ice_dist_cat)) %>%
  group_by(pelagic_type) %>%
  summarise(
    total_abundance = sum(abundance_standardized, na.rm = TRUE),
    observations = n(),
    .groups = 'drop'
  ) %>%
  mutate(abundance_pct = round((total_abundance / sum(total_abundance)) * 100, 1))

for(i in 1:nrow(abundance_by_type)) {
  cat(sprintf("%-18s: %6.2f birds/obs-hr (%5.1f%%) [%d observations]\n",
              abundance_by_type$pelagic_type[i],
              abundance_by_type$total_abundance[i],
              abundance_by_type$abundance_pct[i],
              abundance_by_type$observations[i]))
}
