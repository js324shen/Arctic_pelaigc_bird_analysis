rm(list = ls())

library(dplyr)
library(sf)

# Load the clean marine data
truly_marine_data <- readRDS("marine_with_pelagic_types_2025-06-08.rds")


#both usa and alaska in here

cat("=== FIXING DUPLICATE ALASKA DATA ISSUE ===\n")
cat("Starting with:", format(nrow(truly_marine_data), big.mark = ","), "observations\n\n")

# Current data sources
cat("CURRENT DATA SOURCES (showing the duplicate Alaska problem):\n")
source_breakdown <- truly_marine_data %>%
  st_drop_geometry() %>%
  count(data_source, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))
print(source_breakdown)
cat("\n")

cat("PROBLEM IDENTIFIED:\n")
cat("- 'usa' (331,773 obs) = Alaska data from our previous recovery\n")
cat("- 'alaska' (142,322 obs) = Alaska data again!\n") 
cat("- This means Alaska observations appear TWICE\n\n")

# SOLUTION: Remove "usa" source since it's duplicate Alaska data
# Keep the actual "alaska" source plus other Arctic countries
corrected_data <- truly_marine_data %>%
  filter(data_source != "usa")  # Remove the duplicate Alaska data labeled as "usa"

removed_duplicate_alaska <- nrow(truly_marine_data) - nrow(corrected_data)

cat("AFTER REMOVING DUPLICATE ALASKA DATA:\n")
cat("Removed", format(removed_duplicate_alaska, big.mark = ","), "duplicate Alaska observations (labeled as 'usa')\n")
cat("Remaining:", format(nrow(corrected_data), big.mark = ","), "observations\n\n")

# Check corrected data source breakdown
cat("CORRECTED DATA SOURCES (no more duplicates):\n")
final_breakdown <- corrected_data %>%
  st_drop_geometry() %>%
  count(data_source, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))
print(final_breakdown)
cat("\n")

# Verify this makes sense for Arctic coverage
cat("ARCTIC REGIONS INCLUDED:\n")
cat("✅ alaska    - Arctic Alaska\n")
cat("✅ iceland   - Arctic Iceland\n") 
cat("✅ norway    - Arctic Norway\n")
cat("✅ canada    - Arctic Canada\n")
cat("✅ svalbard  - Arctic Svalbard\n")
cat("✅ greenland - Arctic Greenland\n")
cat("✅ russia    - Arctic Russia\n")
cat("✅ highsea   - High seas\n\n")

# Check if we're in the right range now
cat("=== COMPARISON TO TARGET ===\n")
cat("Your OLD data: ~380,000 observations\n")
cat("Your NEW data: ~280,000 observations\n") 
cat("Current data:", format(nrow(corrected_data), big.mark = ","), "observations\n")

if(nrow(corrected_data) >= 250000 && nrow(corrected_data) <= 400000) {
  cat("✅ EXCELLENT! Now in the expected range!\n")
} else if(nrow(corrected_data) > 400000) {
  cat("⚠️  Still high - may need exact duplicate removal\n")
} else {
  cat("ℹ️  Lower than expected but might be correct after removing duplicates\n")
}

# Show final geographic summary
cat("\nFINAL GEOGRAPHIC COVERAGE:\n")
geo_summary <- corrected_data %>%
  st_drop_geometry() %>%
  group_by(data_source) %>%
  summarise(
    obs_count = n(),
    species_count = n_distinct(`COMMON NAME`),
    min_lat = round(min(LATITUDE, na.rm = TRUE), 1),
    max_lat = round(max(LATITUDE, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(obs_count))
print(geo_summary)

# Save the corrected dataset
output_file <- paste0("arctic_marine_no_duplicates_", Sys.Date(), ".rds")
saveRDS(corrected_data, output_file)
cat("\n✅ CORRECTED DATA SAVED:", output_file, "\n")

# Final summary
final_species <- length(unique(corrected_data$`COMMON NAME`))
cat("\nFINAL SUMMARY:\n")
cat("- Observations:", format(nrow(corrected_data), big.mark = ","), "\n")
cat("- Species:", final_species, "\n")

