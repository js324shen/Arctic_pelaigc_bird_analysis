rm(list = ls())
library(dplyr)
library(sf)

cat("=== ARCTIC + FAROE ISLANDS DATASET MERGE ===\n\n")

# Load the main Arctic dataset (corrected, no duplicates)
cat("Loading main Arctic dataset...\n")
arctic_files <- list.files(pattern = "arctic_marine_no_duplicates_2025-06-08.rds$")
if(length(arctic_files) == 0) {
  stop("No Arctic dataset found! Looking for: arctic_marine_no_duplicates_*.rds")
}
arctic_data <- readRDS(arctic_files[1])
cat("✅ Loaded Arctic data:", arctic_files[1], "\n")
cat("   Arctic observations:", format(nrow(arctic_data), big.mark = ","), "\n")
cat("   Arctic species:", length(unique(arctic_data$`COMMON NAME`)), "\n\n")

# Load the Faroe Islands dataset (final pelagic only)
cat("Loading Faroe Islands dataset...\n")
faroe_files <- list.files(pattern = "faroe_pelagic_final_2025-06-09.rds$")
if(length(faroe_files) == 0) {
  stop("No Faroe dataset found! Looking for: faroe_pelagic_final_*.rds")
}
faroe_data <- readRDS(faroe_files[1])
cat("✅ Loaded Faroe data:", faroe_files[1], "\n")
cat("   Faroe observations:", format(nrow(faroe_data), big.mark = ","), "\n")
cat("   Faroe species:", length(unique(faroe_data$`COMMON NAME`)), "\n\n")

# Check data source assignments
cat("=== DATA SOURCE VERIFICATION ===\n")
cat("Arctic data sources:\n")
print(table(arctic_data$data_source))

cat("\nFaroe data sources:\n")
print(table(faroe_data$data_source))

# Ensure Faroe data is properly labeled
if(!"faroeislands" %in% faroe_data$data_source) {
  cat("⚠️  Setting Faroe data_source to 'faroeislands'\n")
  faroe_data$data_source <- "faroeislands"
}

# Check column compatibility before merge
cat("\n=== COLUMN COMPATIBILITY CHECK ===\n")
arctic_cols <- names(arctic_data)
faroe_cols <- names(faroe_data)

common_cols <- intersect(arctic_cols, faroe_cols)
arctic_only <- setdiff(arctic_cols, faroe_cols)
faroe_only <- setdiff(faroe_cols, arctic_cols)

cat("Common columns:", length(common_cols), "\n")
cat("Arctic-only columns:", length(arctic_only), "\n")
cat("Faroe-only columns:", length(faroe_only), "\n")

if(length(arctic_only) > 0) {
  cat("\nArctic-only columns:\n")
  print(arctic_only)
}

if(length(faroe_only) > 0) {
  cat("\nFaroe-only columns:\n")
  print(faroe_only)
}

# Remove unnecessary columns instead of adding them
columns_to_remove <- c("X53", "pelagic_category", "recovery_method", "region")

cat("\n=== REMOVING UNNECESSARY COLUMNS ===\n")
removed_arctic <- intersect(names(arctic_data), columns_to_remove)
removed_faroe <- intersect(names(faroe_data), columns_to_remove)

if(length(removed_arctic) > 0) {
  cat("Removing from Arctic data:", paste(removed_arctic, collapse = ", "), "\n")
  arctic_data <- arctic_data %>% select(-all_of(removed_arctic))
}

if(length(removed_faroe) > 0) {
  cat("Removing from Faroe data:", paste(removed_faroe, collapse = ", "), "\n")
  faroe_data <- faroe_data %>% select(-all_of(removed_faroe))
}

# Check remaining column differences
arctic_cols_clean <- names(arctic_data)
faroe_cols_clean <- names(faroe_data)
remaining_arctic_only <- setdiff(arctic_cols_clean, faroe_cols_clean)
remaining_faroe_only <- setdiff(faroe_cols_clean, arctic_cols_clean)

if(length(remaining_arctic_only) > 0) {
  cat("Remaining Arctic-only columns:", paste(remaining_arctic_only, collapse = ", "), "\n")
  for(col in remaining_arctic_only) {
    faroe_data[[col]] <- NA
  }
}

if(length(remaining_faroe_only) > 0) {
  cat("Remaining Faroe-only columns:", paste(remaining_faroe_only, collapse = ", "), "\n")
  for(col in remaining_faroe_only) {
    arctic_data[[col]] <- NA
  }
}


# Verify pelagic_type column exists in both
if(!"pelagic_type" %in% names(arctic_data)) {
  cat("⚠️  Adding pelagic_type to Arctic data\n")
  # Add based on your classification system
  true_pelagic_species <- c(
    # ALBATROSSES
    "Black-footed Albatross", "Laysan Albatross", "Short-tailed Albatross", 
    "Black-browed Albatross", "Atlantic Yellow-nosed Albatross", "Salvin's Albatross",
    "albatross sp.", "Laysan x Black-footed Albatross (hybrid)",
    
    # PETRELS & SHEARWATERS
    "Northern Fulmar", "Sooty Shearwater", "Great Shearwater", "Manx Shearwater",
    "Short-tailed Shearwater", "Buller's Shearwater", "Pink-footed Shearwater",
    "Flesh-footed Shearwater", "Mottled Petrel", "Providence Petrel",
    "shearwater sp.", "small shearwater sp.", "Sooty/Short-tailed Shearwater", 
    "procellariid sp.",
    
    # STORM-PETRELS
    "Wilson's Storm-Petrel", "Leach's Storm-Petrel", "Fork-tailed Storm-Petrel", 
    "European Storm-Petrel", "storm-petrel sp.", "storm-petrel sp. (white-rumped)",
    "Oceanites sp.", "Hydrobates sp.",
    
    # JAEGERS & SKUAS
    "Pomarine Jaeger", "Parasitic Jaeger", "Long-tailed Jaeger", "Great Skua", 
    "South Polar Skua", "jaeger sp.", "skua sp.", "jaeger/skua sp.",
    "Long-tailed/Parasitic Jaeger", "Parasitic/Pomarine Jaeger",
    
    # ALCIDS
    "Thick-billed Murre", "Common Murre", "Razorbill", "Dovekie",
    "Atlantic Puffin", "Horned Puffin", "Tufted Puffin",
    "Ancient Murrelet", "Marbled Murrelet", "Kittlitz's Murrelet", "Long-billed Murrelet",
    "Cassin's Auklet", "Parakeet Auklet", "Least Auklet", "Crested Auklet",
    "Whiskered Auklet", "Rhinoceros Auklet", "Black Guillemot", "Pigeon Guillemot",
    "alcid sp.", "large alcid sp.", "murrelet sp.", "auklet sp.", "puffin sp.",
    "Thick-billed/Common Murre", "Black/Pigeon Guillemot",
    
    # TRULY PELAGIC GULLS
    "Black-legged Kittiwake", "Red-legged Kittiwake", "Ivory Gull", "Ross's Gull", 
    "Sabine's Gull", "Black-legged/Red-legged Kittiwake",
    
    # NORTHERN GANNET
    "Northern Gannet",
    
    # RED PHALAROPE
    "Red Phalarope"
  )
  
  arctic_data$pelagic_type <- ifelse(arctic_data$`COMMON NAME` %in% true_pelagic_species, 
                                     "True Pelagic", "Partially Pelagic")
}

# Perform the merge
cat("\n=== MERGING DATASETS ===\n")
cat("Before merge:\n")
cat("- Arctic:", format(nrow(arctic_data), big.mark = ","), "observations\n")
cat("- Faroe:", format(nrow(faroe_data), big.mark = ","), "observations\n")

# Combine datasets
combined_data <- bind_rows(arctic_data, faroe_data)

cat("After merge:\n")
cat("- Combined:", format(nrow(combined_data), big.mark = ","), "observations\n")
cat("- Total species:", length(unique(combined_data$`COMMON NAME`)), "\n\n")

# Data source breakdown
cat("=== COMBINED DATA SOURCE BREAKDOWN ===\n")
source_summary <- combined_data %>%
  st_drop_geometry() %>%
  count(data_source, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))
print(source_summary)






cat("\n=== PHASE 2: DUPLICATE DETECTION ===\n")
cat("Checking for potential duplicates using OBSERVER_ID and CHECKLIST_DURATION...\n\n")

# Remove observations with missing critical data
cat("Removing observations with missing critical data...\n")
before_clean <- nrow(combined_data)

names(combined_data)

clean_data <- combined_data %>%
  filter(!is.na(`OBSERVER ID`) & 
           !is.na(`DURATION MINUTES`) & 
           !is.na(`OBSERVATION DATE`) &
           !st_is_empty(geometry))  # Check for valid geometry instead of lat/lon

after_clean <- nrow(clean_data)
cat("Removed", format(before_clean - after_clean, big.mark = ","), "observations with missing data\n")
cat("Remaining:", format(after_clean, big.mark = ","), "observations\n\n")

# Extract coordinates for duplicate detection
coords <- st_coordinates(clean_data)
clean_data$longitude <- coords[,1]
clean_data$latitude <- coords[,2]

# Duplicate detection
cat("=== DUPLICATE DETECTION ===\n")
cat("Looking for exact duplicates based on:\n")
cat("- Same observer, duration, date, species, and location\n\n")

# Find duplicates
duplicates <- clean_data %>%
  st_drop_geometry() %>%
  group_by(`OBSERVER ID`, `DURATION MINUTES`, `OBSERVATION DATE`, 
           `COMMON NAME`, round(latitude, 4), round(longitude, 4)) %>%
  mutate(
    duplicate_count = n(),
    is_duplicate = duplicate_count > 1,
    keep_record = row_number() == 1  # Keep first occurrence
  ) %>%
  ungroup()

# Summary of duplicates
duplicate_summary <- duplicates %>%
  filter(is_duplicate) %>%
  summarise(
    total_duplicate_obs = n(),
    unique_duplicate_groups = sum(keep_record),
    obs_to_remove = sum(!keep_record)
  )

cat("Duplicate detection results:\n")
cat("- Total duplicate observations:", duplicate_summary$total_duplicate_obs, "\n")
cat("- Unique groups:", duplicate_summary$unique_duplicate_groups, "\n") 
cat("- Observations to remove:", duplicate_summary$obs_to_remove, "\n\n")

# Remove duplicates
if(duplicate_summary$obs_to_remove > 0) {
  cat("Removing duplicates...\n")
  final_data <- clean_data[duplicates$keep_record, ]
} else {
  cat("No duplicates found!\n")
  final_data <- clean_data
}

cat("Final observations:", format(nrow(final_data), big.mark = ","), "\n\n")

# Final summary
cat("=== FINAL DATASET SUMMARY ===\n")
final_summary <- final_data %>%
  st_drop_geometry() %>%
  group_by(data_source) %>%
  summarise(
    observations = n(),
    species = n_distinct(`COMMON NAME`),
    .groups = 'drop'
  ) %>%
  arrange(desc(observations))

print(final_summary)

cat("\nTotal species:", length(unique(final_data$`COMMON NAME`)), "\n")




#system error/multiple upload duplicates removed
#now look at group duplicates from training sessions



# STEP 1: Extract coordinates (needed for grouping by location)
cat("Step 1: Extracting coordinates...\n")
# Keep data in original projection, just convert coordinates for grouping
final_data_temp <- st_transform(final_data, crs = 4326)  # Temporary conversion
coords_wgs84 <- st_coordinates(final_data_temp)
final_data$lat_wgs84 <- round(coords_wgs84[,2], 4)  # True lat for grouping
final_data$lon_wgs84 <- round(coords_wgs84[,1], 4)  # True lon for grouping

# STEP 2: Create a "session identifier" for each potential group session
# Logic: Same time + same place + same duration = same birding session
cat("Step 2: Creating session identifiers...\n")

# Redo duplicate detection with proper coordinates
final_data$session_id <- paste(
  final_data$`OBSERVATION DATE`,
  final_data$`DURATION MINUTES`, 
  final_data$lat_wgs84,  # Use proper lat
  final_data$lon_wgs84,  # Use proper lon
  sep = "_"
)

# STEP 3: For each observer in each session, create their species list
cat("Step 3: Creating species lists per observer per session...\n")
observer_sessions <- final_data %>%
  st_drop_geometry() %>%
  group_by(session_id, `OBSERVER ID`) %>%
  summarise(
    species_list = paste(sort(`COMMON NAME`), collapse = "|"),
    n_species = n_distinct(`COMMON NAME`),
    n_observations = n(),
    .groups = 'drop'
  )

cat("Found", nrow(observer_sessions), "unique observer-session combinations\n")

# STEP 4: Find sessions where multiple observers have IDENTICAL species lists
cat("Step 4: Finding identical species lists...\n")
duplicate_sessions <- observer_sessions %>%
  group_by(session_id, species_list) %>%
  summarise(
    n_observers = n(),
    observers = paste(`OBSERVER ID`, collapse = "; "),
    total_obs = sum(n_observations),
    .groups = 'drop'
  ) %>%
  filter(n_observers >= 2)  # Multiple observers with same species list

cat("Found", nrow(duplicate_sessions), "sessions with identical observer lists\n")

if(nrow(duplicate_sessions) > 0) {
  cat("\nExamples of group sessions:\n")
  for(i in 1:min(3, nrow(duplicate_sessions))) {
    cat("Session", i, ":", duplicate_sessions$n_observers[i], "observers recorded:")
    species <- strsplit(duplicate_sessions$species_list[i], "\\|")[[1]]
    cat(paste(species, collapse = ", "), "\n")
  }
  
  # STEP 5: For each duplicate session, keep only ONE observer's records
  cat("\nStep 5: Removing duplicate observers...\n")
  cat("Strategy: For each session with identical lists, keep first observer only\n")
  
  # Mark which observer-sessions to keep
  observers_to_keep <- observer_sessions %>%
    group_by(session_id, species_list) %>%
    slice(1) %>%  # Keep first observer for each identical species list
    ungroup() %>%
    select(session_id, `OBSERVER ID`, species_list)
  
  # Mark original observations to keep
  final_data$keep_observation <- final_data %>%
    st_drop_geometry() %>%
    left_join(observers_to_keep, by = c("session_id", "OBSERVER ID")) %>%
    mutate(keep = !is.na(species_list)) %>%
    pull(keep)
  
  # Count removals
  before_removal <- nrow(final_data)
  to_remove <- sum(!final_data$keep_observation)
  
  cat("Observations to remove:", format(to_remove, big.mark = ","), "\n")
  
  # Apply removal
  cleaned_data <- final_data[final_data$keep_observation, ]
  
  cat("✅ Removed", format(to_remove, big.mark = ","), "group duplicate observations\n")
  
} else {
  cat("No group birding sessions found!\n")
  cleaned_data <- final_data
}




#6418 sessions with identical lists is quite a lot
#Check the distribution of duplicate group sizes
cat("=== DIAGNOSTIC: GROUP SIZE DISTRIBUTION ===\n")
group_sizes <- duplicate_sessions %>%
  count(n_observers, sort = TRUE)
print(group_sizes)

cat("\n=== DIAGNOSTIC: LARGEST GROUPS ===\n")
largest_groups <- duplicate_sessions %>%
  arrange(desc(n_observers)) %>%
  head(10)
print(largest_groups)

cat("\n=== DIAGNOSTIC: DATA SOURCE OF DUPLICATES ===\n")
# Check which data sources are involved in duplicates
duplicate_data_sources <- final_data %>%
  st_drop_geometry() %>%
  filter(session_id %in% duplicate_sessions$session_id) %>%
  count(data_source, sort = TRUE)
print(duplicate_data_sources)

cat("\n=== DIAGNOSTIC: SAMPLE OF IDENTICAL SESSIONS ===\n")
# Look at a few specific examples
sample_session <- duplicate_sessions$session_id[1]
sample_details <- final_data %>%
  st_drop_geometry() %>%
  filter(session_id == sample_session) %>%
  select(`OBSERVER ID`, `COMMON NAME`, data_source, `OBSERVATION DATE`, `DURATION MINUTES`) %>%
  arrange(`OBSERVER ID`, `COMMON NAME`)
print(sample_details)



# Clean up temporary columns
cleaned_data$lat <- NULL
cleaned_data$lon <- NULL
cleaned_data$session_id <- NULL
cleaned_data$keep_observation <- NULL


# Final summary
cat("\n=== SUMMARY ===\n")
cat("Original:", format(nrow(final_data), big.mark = ","), "observations\n")
cat("Final:", format(nrow(cleaned_data), big.mark = ","), "observations\n")
cat("Removed:", format(nrow(final_data) - nrow(cleaned_data), big.mark = ","), "\n")

# Save
output_file <- paste0("arctic_faroe_cleaned_", Sys.Date(), ".rds")
saveRDS(cleaned_data, output_file)
cat("\n✅ Cleaned dataset saved:", output_file, "\n")






#need to change projection of coordinates to WGS84

#checking coordinate precision
sample_coords <- final_data %>%
  filter(session_id %in% duplicate_sessions$session_id[1:10]) %>%
  st_drop_geometry() %>%
  select(session_id, lat, lon, `OBSERVER ID`) %>%
  arrange(session_id)

print(sample_coords)

# Check what coordinate system data is in
st_crs(final_data)

# Convert to proper lat/lon for duplicate detection
final_data_latlon <- st_transform(final_data, crs = 4326)  # WGS84
coords_latlon <- st_coordinates(final_data_latlon)
final_data$lat_proper <- round(coords_latlon[,2], 4)  # Latitude
final_data$lon_proper <- round(coords_latlon[,1], 4)  # Longitude

# Check the difference
head(data.frame(
  old_lat = final_data$lat[1:5],
  new_lat = final_data$lat_proper[1:5],
  old_lon = final_data$lon[1:5], 
  new_lon = final_data$lon_proper[1:5]
))




#Data quality check 
cleaned_data <- readRDS("arctic_faroe_cleaned_2025-06-10.rds")

summary(cleaned_data)
table(cleaned_data$data_source)
table(cleaned_data$pelagic_type)

#Temporal coverage
table(cleaned_data$year)
table(cleaned_data$month)

library(ggplot2)

#Quick distribution check
ggplot(cleaned_data) + geom_sf(size=0.1)


species_summary <- cleaned_data %>% 
  count(`COMMON NAME`, sort=TRUE)

print(species_summary)
