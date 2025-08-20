rm(list = ls())

library(sf)
library(dplyr)
library(readr)
library(rnaturalearth)
library(lubridate)

cat("=== TARGETED RECOVERY FOR 4 COUNTRIES ===\n")
cat("Recovering: Iceland, USA (Alaska), Norway, Svalbard\n\n")

# Settings (same as your original processing)
sf_use_s2(FALSE)
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

# Function to reprocess a country using OLD METHOD (merge-first approach)
reprocess_country_old_method <- function(file_path, country_name) {
  cat("=== Reprocessing", country_name, "using OLD merge-first method ===\n")
  
  if (!file.exists(file_path)) {
    cat("ERROR: File not found:", file_path, "\n")
    return(NULL)
  }
  
  # Read ALL data first (merge-first approach like your old method)
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
  
  # Apply filters to COMPLETE dataset (OLD merge-first approach)
  cat("Applying Arctic filter...\n")
  in_arctic <- lengths(st_intersects(data_sf, amap)) > 0
  arctic_data <- data_sf[in_arctic, ]
  cat("After Arctic filter:", format(nrow(arctic_data), big.mark = ","), "observations\n")
  rm(data_sf, in_arctic); gc()
  
  if (nrow(arctic_data) == 0) {
    cat("No Arctic observations found\n")
    return(NULL)
  }
  
  cat("Applying land filter...\n")
  on_land <- lengths(st_intersects(arctic_data, land)) > 0
  sea_data <- arctic_data[!on_land, ]
  cat("After land filter:", format(nrow(sea_data), big.mark = ","), "observations\n")
  rm(arctic_data, on_land); gc()
  
  if (nrow(sea_data) == 0) {
    cat("No sea observations found\n")
    return(NULL)
  }
  
  cat("Calculating shore distances...\n")
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
  
  # Add metadata
  pelagic_data$data_source <- tolower(gsub(" ", "", country_name))
  pelagic_data$count_numeric <- as.numeric(pelagic_data$`OBSERVATION COUNT`)
  pelagic_data$count_numeric[is.na(pelagic_data$count_numeric)] <- 1
  
  # Add temporal variables
  pelagic_data$`OBSERVATION DATE` <- as.Date(pelagic_data$`OBSERVATION DATE`)
  pelagic_data$year <- year(pelagic_data$`OBSERVATION DATE`)
  pelagic_data$month <- month(pelagic_data$`OBSERVATION DATE`)
  
  # Date filter (2003-2025)
  cat("Applying date filter (2003-2025)...\n")
  final_data <- pelagic_data %>%
    filter(`OBSERVATION DATE` >= as.Date("2003-01-01") & 
             `OBSERVATION DATE` <= as.Date("2025-03-31"))
  
  cat("Final", country_name, "observations:", format(nrow(final_data), big.mark = ","), "\n")
  cat("Date range:", min(final_data$`OBSERVATION DATE`), "to", max(final_data$`OBSERVATION DATE`), "\n")
  cat("Unique species:", length(unique(final_data$`COMMON NAME`)), "\n\n")
  
  return(final_data)
}

# RECOVER ICELAND
cat("📍 STARTING ICELAND RECOVERY\n")
iceland_recovered <- reprocess_country_old_method("ebd/ebd_IS_smp_relMar-2025.txt", "Iceland")
if (!is.null(iceland_recovered)) {
  saveRDS(iceland_recovered, "iceland_recovered.rds")
  cat("✅ Iceland recovery complete - saved iceland_recovered.rds\n\n")
} else {
  cat("❌ Iceland recovery failed\n\n")
}

# RECOVER USA (ALASKA)
cat("📍 STARTING USA (ALASKA) RECOVERY\n")
usa_recovered <- reprocess_country_old_method("ebd/ebd_US-AK_smp_relMar-2025.txt", "United States")
if (!is.null(usa_recovered)) {
  usa_recovered$data_source <- "usa"  # Standardize naming
  saveRDS(usa_recovered, "usa_recovered.rds")
  cat("✅ USA (Alaska) recovery complete - saved usa_recovered.rds\n\n")
} else {
  cat("❌ USA (Alaska) recovery failed\n\n")
}

# RECOVER NORWAY
cat("📍 STARTING NORWAY RECOVERY\n")
norway_recovered <- reprocess_country_old_method("ebd/ebd_NO_smp_relMar-2025.txt", "Norway")
if (!is.null(norway_recovered)) {
  saveRDS(norway_recovered, "norway_recovered.rds")
  cat("✅ Norway recovery complete - saved norway_recovered.rds\n\n")
} else {
  cat("❌ Norway recovery failed\n\n")
}

# RECOVER SVALBARD
cat("📍 STARTING SVALBARD RECOVERY\n")
svalbard_recovered <- reprocess_country_old_method("ebd_SJ_relMar-2025.txt", "Svalbard")
if (!is.null(svalbard_recovered)) {
  saveRDS(svalbard_recovered, "svalbard_recovered.rds")
  cat("✅ Svalbard recovery complete - saved svalbard_recovered.rds\n\n")
} else {
  cat("❌ Svalbard recovery failed\n\n")
}

# MERGE RECOVERED DATA WITH EXISTING CLEAN DATASET
cat("=== MERGING RECOVERED DATA WITH EXISTING DATASET ===\n")

# Load your current clean dataset
current_clean <- readRDS("arctic_pelagic_cleaned_v2.rds")
cat("Current clean dataset:", format(nrow(current_clean), big.mark = ","), "observations\n")

# Remove the problematic countries from current dataset
countries_to_replace <- c("iceland", "usa", "norway", "svalbard")
current_clean_filtered <- current_clean %>%
  filter(!data_source %in% countries_to_replace)

cat("After removing problematic countries:", format(nrow(current_clean_filtered), big.mark = ","), "observations\n")

# Combine with recovered data
recovered_datasets <- list()

if (exists("iceland_recovered") && !is.null(iceland_recovered)) {
  recovered_datasets[["iceland"]] <- iceland_recovered
  cat("Adding Iceland:", format(nrow(iceland_recovered), big.mark = ","), "observations\n")
}

if (exists("usa_recovered") && !is.null(usa_recovered)) {
  recovered_datasets[["usa"]] <- usa_recovered
  cat("Adding USA:", format(nrow(usa_recovered), big.mark = ","), "observations\n")
}

if (exists("norway_recovered") && !is.null(norway_recovered)) {
  recovered_datasets[["norway"]] <- norway_recovered
  cat("Adding Norway:", format(nrow(norway_recovered), big.mark = ","), "observations\n")
}

if (exists("svalbard_recovered") && !is.null(svalbard_recovered)) {
  recovered_datasets[["svalbard"]] <- svalbard_recovered
  cat("Adding Svalbard:", format(nrow(svalbard_recovered), big.mark = ","), "observations\n")
}

# Combine all datasets #code updated below
if (length(recovered_datasets) > 0) {
  all_recovered <- do.call(rbind, recovered_datasets)
  final_combined <- rbind(current_clean_filtered, all_recovered)
} else {
  cat("Warning: No datasets were successfully recovered\n")
  final_combined <- current_clean_filtered
}


#checking error
cat("Columns in current_clean_filtered:\n")
print(names(current_clean_filtered))

cat("\nColumns in recovered datasets:\n")
if (exists("iceland_recovered") && !is.null(iceland_recovered)) {
  cat("Iceland columns:\n")
  print(names(iceland_recovered))
}

if (exists("usa_recovered") && !is.null(usa_recovered)) {
  cat("USA columns:\n") 
  print(names(usa_recovered))
}

# Find missing columns
if (exists("iceland_recovered") && !is.null(iceland_recovered)) {
  missing_in_iceland <- setdiff(names(current_clean_filtered), names(iceland_recovered))
  missing_in_current <- setdiff(names(iceland_recovered), names(current_clean_filtered))
  
  cat("\nColumns missing in Iceland:\n")
  print(missing_in_iceland)
  cat("\nColumns missing in current dataset:\n")
  print(missing_in_current)
}


if (length(recovered_datasets) > 0) {
  
  # Add missing columns to each recovered dataset
  for (i in 1:length(recovered_datasets)) {
    # Add X53 column (probably not important)
    if (!"X53" %in% names(recovered_datasets[[i]])) {
      recovered_datasets[[i]]$X53 <- NA
    }
    
    # Add pelagic_category - this is IMPORTANT!
    if (!"pelagic_category" %in% names(recovered_datasets[[i]])) {
      recovered_datasets[[i]]$pelagic_category <- "Unclassified"  # Will classify later
    }
  }
  
  # Now combine
  all_recovered <- do.call(rbind, recovered_datasets)
  final_combined <- rbind(current_clean_filtered, all_recovered)
  
} else {
  cat("Warning: No datasets were successfully recovered\n")
  final_combined <- current_clean_filtered
}


unique(final_combined$`COMMON NAME`)




cat("\nFinal combined dataset:", format(nrow(final_combined), big.mark = ","), "observations\n")

# Add recovery metadata
final_combined$recovery_method <- ifelse(
  final_combined$data_source %in% countries_to_replace, 
  "recovered_merge_first", 
  "original_country_by_country"
)

# Save final recovered dataset
output_file <- paste0("arctic_pelagic_recovered_", Sys.Date(), ".rds")
saveRDS(final_combined, output_file)

# SUMMARY COMPARISON
cat("=== RECOVERY SUMMARY ===\n")
cat("Original NEW dataset:", format(nrow(current_clean), big.mark = ","), "observations\n")
cat("Recovered dataset:   ", format(nrow(final_combined), big.mark = ","), "observations\n")
cat("Net change:          ", format(nrow(final_combined) - nrow(current_clean), big.mark = ","), "observations\n")

# Breakdown by country and method
recovery_summary <- final_combined %>%
  st_drop_geometry() %>%
  group_by(data_source, recovery_method) %>%
  summarise(observations = n(), .groups = "drop") %>%
  arrange(desc(observations))

cat("\nBreakdown by country and method:\n")
print(recovery_summary)

