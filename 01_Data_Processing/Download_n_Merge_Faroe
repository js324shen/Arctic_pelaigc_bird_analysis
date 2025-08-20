rm(list = ls())

# Load required libraries
library(sf)
library(dplyr)
library(lubridate)
library(readr)
library(rnaturalearth)

# Disable s2 processing for sf
sf_use_s2(FALSE)

# Settings (same as your original processing)
arctic_projection <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
pelagic_distance <- 3218

# Load spatial reference data
cat("Loading spatial reference data...\n")
amap <- st_read("AMAP-area/amaplim_lam_poly.shp", quiet = TRUE)
amap <- st_transform(amap, arctic_projection)
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, arctic_projection) 
coastline <- ne_coastline(scale = "medium", returnclass = "sf")
coastline <- st_transform(coastline, arctic_projection)
cat("Spatial data loaded successfully\n\n")

# Function to process a country dataset (based on your reredo.R approach)
process_country_dataset <- function(file_path, country_name) {
  cat("=== Processing", country_name, "===\n")
  
  if (!file.exists(file_path)) {
    cat("ERROR: File not found:", file_path, "\n")
    return(NULL)
  }
  
  # Read ALL data first (like your reredo.R approach)
  cat("Reading raw data...\n")
  raw_data <- read_delim(file_path, delim = "\t", show_col_types = FALSE)
  cat("Read", format(nrow(raw_data), big.mark = ","), "raw observations\n")
  
  # Check for required columns
  if (!all(c("LONGITUDE", "LATITUDE") %in% names(raw_data))) {
    cat("ERROR: Missing coordinate columns\n")
    return(NULL)
  }
  
  # Remove rows with missing/invalid coordinates
  raw_data <- raw_data[!is.na(raw_data$LONGITUDE) & !is.na(raw_data$LATITUDE), ]
  raw_data <- raw_data[abs(raw_data$LONGITUDE) <= 180 & abs(raw_data$LATITUDE) <= 90, ]
  
  if (nrow(raw_data) == 0) {
    cat("ERROR: No valid coordinates\n")
    return(NULL)
  }
  
  # Convert to spatial
  cat("Converting to spatial data...\n")
  data_sf <- st_as_sf(raw_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  data_sf <- st_transform(data_sf, arctic_projection)
  rm(raw_data); gc()
  
  # FILTER 1: Apply Arctic boundary filter
  cat("FILTER 1: Applying Arctic filter...\n")
  in_arctic <- lengths(st_intersects(data_sf, amap)) > 0
  arctic_data <- data_sf[in_arctic, ]
  cat("After Arctic filter:", format(nrow(arctic_data), big.mark = ","), "observations\n")
  rm(data_sf, in_arctic); gc()
  
  if (nrow(arctic_data) == 0) {
    cat("No Arctic observations found\n")
    return(NULL)
  }
  
  # FILTER 2: Apply land filter (exclude land observations)
  cat("FILTER 2: Applying land filter...\n")
  on_land <- lengths(st_intersects(arctic_data, land)) > 0
  sea_data <- arctic_data[!on_land, ]
  cat("After land filter:", format(nrow(sea_data), big.mark = ","), "observations\n")
  rm(arctic_data, on_land); gc()
  
  if (nrow(sea_data) == 0) {
    cat("No sea observations found\n")
    return(NULL)
  }
  
  # FILTER 3: Calculate shore distances and apply pelagic filter
  cat("FILTER 3: Calculating shore distances...\n")
  # Calculate shore distance (using batch processing for large datasets)
  n_obs <- nrow(sea_data)
  if (n_obs > 10000) {
    # Process in batches for large datasets
    batch_size <- 5000
    n_batches <- ceiling(n_obs / batch_size)
    shore_distances <- numeric(n_obs)
    
    for (b in 1:n_batches) {
      start_idx <- (b - 1) * batch_size + 1
      end_idx <- min(b * batch_size, n_obs)
      
      batch_data <- sea_data[start_idx:end_idx, ]
      distances <- st_distance(batch_data, coastline)
      min_distances <- apply(distances, 1, min)
      shore_distances[start_idx:end_idx] <- as.numeric(min_distances)
      
      if (b %% 5 == 0 || b == n_batches) {
        cat("  Distance batch", b, "of", n_batches, "completed\n")
      }
      
      rm(batch_data, distances, min_distances); gc()
    }
    
    sea_data$shore_dist <- shore_distances
    rm(shore_distances)
  } else {
    # Process normally for smaller datasets
    distances <- st_distance(sea_data, coastline)
    min_distances <- apply(distances, 1, min)
    sea_data$shore_dist <- as.numeric(min_distances)
    rm(distances, min_distances)
  }
  
  gc()
  
  cat("Applying pelagic filter (>=", pelagic_distance, "m from shore)...\n")
  pelagic_data <- sea_data %>% filter(shore_dist >= pelagic_distance)
  cat("After pelagic filter:", format(nrow(pelagic_data), big.mark = ","), "observations\n")
  rm(sea_data); gc()
  
  if (nrow(pelagic_data) == 0) {
    cat("No pelagic observations found\n")
    return(NULL)
  }
  
  # FILTER 4: Date filter (2003-2025)
  cat("FILTER 4: Applying date filter (2003-2025)...\n")
  pelagic_data$`OBSERVATION DATE` <- as.Date(pelagic_data$`OBSERVATION DATE`)
  final_data <- pelagic_data %>%
    filter(`OBSERVATION DATE` >= as.Date("2003-01-01") & 
             `OBSERVATION DATE` <= as.Date("2025-03-31"))
  
  cat("After date filter:", format(nrow(final_data), big.mark = ","), "observations\n")
  rm(pelagic_data); gc()
  
  if (nrow(final_data) == 0) {
    cat("No observations after date filter\n")
    return(NULL)
  }
  
  # Add metadata (like your reredo.R approach)
  final_data$data_source <- tolower(gsub(" ", "", country_name))
  final_data$count_numeric <- as.numeric(final_data$`OBSERVATION COUNT`)
  final_data$count_numeric[is.na(final_data$count_numeric)] <- 1
  
  # Add temporal variables
  final_data$year <- year(final_data$`OBSERVATION DATE`)
  final_data$month <- month(final_data$`OBSERVATION DATE`)
  
  # FILTER 5: Check species present
  cat("FILTER 5: Checking species composition...\n")
  unique_species <- unique(final_data$`COMMON NAME`)
  cat("Species found in", country_name, "data:\n")
  print(sort(unique_species))
  cat("Total unique species:", length(unique_species), "\n")
  cat("Date range:", min(final_data$`OBSERVATION DATE`), "to", max(final_data$`OBSERVATION DATE`), "\n\n")
  
  return(final_data)
}



cat("📍 STARTING FAROE ISLANDS PROCESSING\n")
faroe_data <- process_country_dataset("ebd_FO_smp_relApr-2025.txt", "Faroe Islands")

if (!is.null(faroe_data)) {
  # Add region identifier
  faroe_data$region <- "Faroe Islands"
  
  # Save processed data
  faroe_output <- paste0("faroe_marine_clean_", Sys.Date(), ".rds")
  saveRDS(faroe_data, faroe_output)
  cat("✅ Faroe Islands processing complete - saved", faroe_output, "\n\n")
} else {
  cat("❌ Faroe Islands processing failed\n\n")
}


unique(faroe_data$`COMMON NAME`)


# Get unique species list
faroe_species <- unique(faroe_data$`COMMON NAME`)
cat("Total unique species:", length(faroe_species), "\n\n")

# Define marine pelagic species categories (based on your existing classification)
true_pelagic_species <- c(
  "Northern Fulmar", 
  "Pomarine Jaeger", 
  "Sooty Shearwater", 
  "Great Skua", 
  "Northern Gannet", 
  "Atlantic Puffin", 
  "Great Shearwater", 
  "Long-tailed Jaeger", 
  "Dovekie", 
  "Leach's Storm-Petrel", 
  "European Storm-Petrel",
  "Wilson's Storm-Petrel",
  "Parasitic Jaeger", 
  "Thick-billed Murre", 
  "Black Guillemot", 
  "Common Murre", 
  "Manx Shearwater", 
  "Razorbill",
  "large alcid sp.",
  "alcid sp.",
  "Cory's/Scopoli's Shearwater",
  "Fea's Petrel",
  "procellariid sp.",
  "shearwater sp."
)

partially_pelagic_species <- c(
  "Black-legged Kittiwake", 
  "Glaucous Gull", 
  "Red Phalarope", 
  "Red-necked Phalarope",
  "Great Black-backed Gull", 
  "Arctic Tern",
  "Iceland Gull", 
  "Lesser Black-backed Gull", 
  "European Herring Gull",
  "gull sp.",
  "Larus sp.",
  "Sabine's Gull"
)

# Additional marine species that might be valid in pelagic zones
marine_coastal_species <- c(
  "European Shag",
  "Common Eider",
  "Common Gull",
  "Black-headed Gull",
  "Great Cormorant",
  "Red-breasted Merganser",
  "Red-throated Loon",
  "Common Loon",
  "loon sp.",
  "Long-tailed Duck",
  "Common Scoter",
  "Surf Scoter"
)

# Clearly terrestrial/inappropriate species for pelagic study
terrestrial_species <- c(
  "Common Raven",
  "European Golden-Plover",
  "Barn Swallow",
  "Common Snipe",
  "Eurasian Blackbird",
  "European Starling",
  "House Sparrow",
  "Meadow Pipit",
  "Northern Wheatear",
  "Rock Pigeon",
  "Rock Pipit",
  "White Wagtail",
  "Eurasian Wren",
  "pipit sp.",
  "Tufted Duck",
  "Barred Warbler",
  "Brambling",
  "Fieldfare",
  "Lesser Whitethroat",
  "Common Cuckoo",
  "Eurasian Blackcap",
  "Blyth's Reed Warbler",
  "Common Chiffchaff",
  "European Robin",
  "Hooded Crow",
  "Merlin",
  "Redpoll",
  "Willow Warbler",
  "Graylag Goose",
  "Western House-Martin",
  "Mallard",
  "Redwing",
  "Common Chaffinch",
  "Common Wood-Pigeon",
  "Eurasian Siskin",
  "Goldcrest",
  "Reed Bunting",
  "Eurasian Skylark",
  "Eurasian Linnet",
  "Pink-footed Goose",
  "White-tailed Eagle",
  "Eurasian Collared-Dove",
  "Green-winged Teal",
  "Domestic goose sp. (Domestic type)",
  "Phylloscopus sp.",
  "Barnacle Goose",
  "Snow Bunting",
  "Eurasian Tree Sparrow",
  "swallow sp.",
  "Rosy Starling",
  "Yellow-browed Warbler",
  "Bohemian Waxwing",
  "Dunnock",
  "Eurasian Bullfinch",
  "Garden Warbler",
  "Gray Heron",
  "Greater Whitethroat",
  "Greater White-fronted Goose",
  "Paddyfield Warbler",
  "Water Rail",
  "Eurasian Wigeon",
  "Anser sp.",
  "Short-eared Owl",
  "Tree Pipit",
  "Wood Sandpiper",
  "Common Swift",
  "Eurasian Thick-knee",
  "Red-backed Shrike",
  "duck sp.",
  "curlew sp.",
  "Wood Warbler",
  "Bank Swallow",
  "Greenish Warbler",
  "Northern Shoveler",
  "Hawfinch",
  "passerine sp.",
  "Eurasian Jackdaw",
  "Peregrine Falcon",
  "Red-breasted Flycatcher",
  "American Wigeon",
  "Common Quail",
  "Eurasian Hobby",
  "Eurasian Kestrel",
  "Icterine Warbler",
  "Spotted Flycatcher",
  "European Turtle-Dove",
  "Whinchat",
  "Song Thrush"
)

# Shorebirds that might be questionable for pelagic zones (review case by case)
questionable_shorebirds <- c(
  "Purple Sandpiper",
  "Eurasian Oystercatcher",
  "Common Ringed Plover",
  "Black-tailed Godwit",
  "Common Redshank",
  "Dunlin",
  "Northern Lapwing",
  "Red Knot",
  "Ruddy Turnstone",
  "Whimbrel",
  "Whooper Swan",
  "Sanderling",
  "Eurasian Curlew",
  "Common Greenshank",
  "Calidris sp.",
  "Ruff",
  "Bar-tailed Godwit"
)

# Create comprehensive species classification
create_species_classification <- function(species_list) {
  classification <- data.frame(
    species = species_list,
    category = case_when(
      species_list %in% true_pelagic_species ~ "True Pelagic",
      species_list %in% partially_pelagic_species ~ "Partially Pelagic", 
      species_list %in% marine_coastal_species ~ "Marine Coastal",
      species_list %in% questionable_shorebirds ~ "Questionable Shorebird",
      species_list %in% terrestrial_species ~ "Terrestrial - REMOVE",
      TRUE ~ "Unclassified - REVIEW"
    ),
    keep_for_pelagic = case_when(
      species_list %in% true_pelagic_species ~ "YES",
      species_list %in% partially_pelagic_species ~ "YES",
      species_list %in% marine_coastal_species ~ "MAYBE - REVIEW",
      species_list %in% questionable_shorebirds ~ "MAYBE - REVIEW", 
      species_list %in% terrestrial_species ~ "NO",
      TRUE ~ "REVIEW"
    ),
    stringsAsFactors = FALSE
  )
  return(classification)
}

# Classify all Faroe species
faroe_classification <- create_species_classification(faroe_species)

# Print classification results
cat("=== FAROE ISLANDS SPECIES CLASSIFICATION ===\n\n")

# Summary by category
category_summary <- faroe_classification %>%
  group_by(category, keep_for_pelagic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

print(category_summary)

cat("\n=== DETAILED SPECIES CLASSIFICATION ===\n")
print(faroe_classification)

# Apply initial filtering (remove definite terrestrial species)
cat("\n=== APPLYING INITIAL FILTER ===\n")
cat("Original observations:", nrow(faroe_data), "\n")

faroe_filtered <- faroe_data %>%
  filter(!`COMMON NAME` %in% remove_species)

cat("After removing terrestrial species:", nrow(faroe_filtered), "\n")
cat("Observations removed:", nrow(faroe_data) - nrow(faroe_filtered), "\n")


unique(faroe_filtered$`COMMON NAME`)




remaining_species <- unique(faroe_filtered$`COMMON NAME`)
cat("Remaining species after first filter:", length(remaining_species), "\n\n")

# Print the list for reference
cat("=== REMAINING SPECIES LIST ===\n")
print(sort(remaining_species))

# Define TRUE PELAGIC species (from your reredo_manual_selection.R)
true_pelagic_species <- c(
  # PETRELS & SHEARWATERS - highly pelagic, feed far offshore
  "Northern Fulmar", "Sooty Shearwater", "Great Shearwater", "Manx Shearwater",
  "Cory's/Scopoli's Shearwater", "Fea's Petrel",
  "shearwater sp.", "procellariid sp.",
  
  # STORM-PETRELS - extremely pelagic, only seen from shore during storms
  "Wilson's Storm-Petrel", "Leach's Storm-Petrel", "European Storm-Petrel", 
  
  # JAEGERS & SKUAS - pursue other seabirds far offshore
  "Pomarine Jaeger", "Parasitic Jaeger", "Long-tailed Jaeger", "Great Skua", 
  
  # ALCIDS - deep water specialists, dive in offshore waters
  "Thick-billed Murre", "Common Murre", "Razorbill", "Dovekie",
  "Atlantic Puffin", "Black Guillemot",
  "alcid sp.", "large alcid sp.",
  
  # TRULY PELAGIC GULLS - spend most time far offshore
  "Black-legged Kittiwake", "Sabine's Gull",
  
  # NORTHERN GANNET - dives in deep offshore waters
  "Northern Gannet",
  
  # RED PHALAROPE - highly pelagic during non-breeding season
  "Red Phalarope"
)

# Define PARTIALLY PELAGIC species (from your reredo_manual_selection.R)
partially_pelagic_species <- c(
  # LARGE GULLS - forage both offshore and coastal
  "Great Black-backed Gull", "Glaucous Gull", "Lesser Black-backed Gull",
  "Iceland Gull", "European Herring Gull", "Common Gull",
  
  # SMALLER GULLS - more coastal but can be offshore
  "Black-headed Gull",
  
  # TERNS - mostly coastal but venture offshore
  "Arctic Tern",
  
  # CORMORANTS - dive in both coastal and offshore waters
  "Great Cormorant", "European Shag",
  
  # SEA DUCKS - found in both nearshore and offshore waters
  "Common Eider", "Long-tailed Duck", "Surf Scoter", "Common Scoter",
  
  # LOONS - use both coastal and offshore marine waters
  "Common Loon", "Red-throated Loon",
  
  # OTHER PHALAROPES - less pelagic than Red Phalarope
  "Red-necked Phalarope",
  
  # RED-BREASTED MERGANSER - sea duck that ventures offshore
  "Red-breasted Merganser",
  
  # GULL/TERN/DUCK SPECIES GROUPS
  "gull sp.", "Larus sp.", "loon sp."
)

# Create final classification - ONLY True Pelagic and Partially Pelagic
final_classification <- data.frame(
  species = remaining_species,
  category = case_when(
    remaining_species %in% true_pelagic_species ~ "True Pelagic",
    remaining_species %in% partially_pelagic_species ~ "Partially Pelagic",
    TRUE ~ "REMOVE - Not Pelagic"
  ),
  final_decision = case_when(
    remaining_species %in% true_pelagic_species ~ "KEEP",
    remaining_species %in% partially_pelagic_species ~ "KEEP",
    TRUE ~ "REMOVE"
  ),
  reason = case_when(
    remaining_species %in% true_pelagic_species ~ "Core pelagic species",
    remaining_species %in% partially_pelagic_species ~ "Semi-pelagic marine species",
    TRUE ~ "Not classified as pelagic in main dataset"
  ),
  stringsAsFactors = FALSE
) %>%
  arrange(final_decision, category, species)

# Print final classification
cat("\n=== FINAL SPECIES CLASSIFICATION ===\n")
print(final_classification)

# Summary by decision
decision_summary <- final_classification %>%
  group_by(final_decision, category) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(final_decision, desc(count))

cat("\n=== SUMMARY BY DECISION ===\n")
print(decision_summary)



# Apply final filtering - ONLY True Pelagic and Partially Pelagic
cat("\n=== APPLYING FINAL FILTER ===\n")
cat("Before final filter:", nrow(faroe_filtered), "observations\n")

faroe_final <- faroe_filtered %>%
  filter(`COMMON NAME` %in% keep_species)

cat("After final filter:", nrow(faroe_final), "observations\n")
cat("Observations removed:", nrow(faroe_filtered) - nrow(faroe_final), "\n")
cat("Final species count:", length(unique(faroe_final$`COMMON NAME`)), "\n")

# Add pelagic category classification (matching your system)
faroe_final$pelagic_type <- case_when(
  faroe_final$`COMMON NAME` %in% true_pelagic_species ~ "True Pelagic",
  faroe_final$`COMMON NAME` %in% partially_pelagic_species ~ "Partially Pelagic",
  TRUE ~ "ERROR - Should not happen"
)

# Final species breakdown by pelagic type
final_species_summary <- faroe_final %>%
  st_drop_geometry() %>%
  group_by(pelagic_type, `COMMON NAME`) %>%
  summarise(observations = n(), .groups = 'drop') %>%
  arrange(pelagic_type, desc(observations))

cat("\n=== FINAL SPECIES BREAKDOWN BY PELAGIC TYPE ===\n")
print(final_species_summary)

# Summary by pelagic type
type_summary <- faroe_final %>%
  st_drop_geometry() %>%
  group_by(pelagic_type) %>%
  summarise(
    species_count = n_distinct(`COMMON NAME`),
    observations = n(),
    .groups = 'drop'
  )

cat("\n=== FINAL SUMMARY BY PELAGIC TYPE ===\n")
print(type_summary)

# Verify no errors in classification
error_check <- faroe_final %>%
  st_drop_geometry() %>%
  filter(pelagic_type == "ERROR - Should not happen") %>%
  nrow()

if(error_check > 0) {
  cat("\n❌ ERROR: Some species not properly classified!\n")
} else {
  cat("\n✅ All species properly classified as True Pelagic or Partially Pelagic\n")
}

# Save final cleaned dataset
output_file <- paste0("faroe_pelagic_final_", Sys.Date(), ".rds")
saveRDS(faroe_final, output_file)
cat("\nFinal Faroe Islands dataset saved as:", output_file, "\n")


cat("\n=== FINAL SUMMARY ===\n")
cat("✅ Species kept:", length(keep_species), "out of", length(remaining_species), "\n")
cat("✅ Observations kept:", nrow(faroe_final), "out of", nrow(faroe_filtered), "\n")

# Show final species list
cat("\n=== FINAL FAROE ISLANDS PELAGIC SPECIES ===\n")
print(sort(unique(faroe_final$`COMMON NAME`)))

