rm(list = ls())
gc()

library(sf)
library(dplyr)
library(lubridate)
library(rnaturalearth)
library(readr)

sf_use_s2(FALSE)

# SETTINGS
chunk_size <- 200000  # Large chunks for efficiency
pelagic_distance <- 3218  # 3.218 km minimum distance from shore
arctic_projection <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# COUNTRIES TO PROCESS (ONLY RUSSIA)
countries <- list(
  list(name = "russia", code = "RU", file = "ebd/ebd_RU_smp_relMar-2025.txt")
)

cat("=== RUSSIA PROCESSING ONLY ===\n")
cat("Processing: Russia (501MB)\n")
cat("Note: Iceland & Alaska already completed, Canada (63GB) will be processed separately\n")
cat("Chunk size:", format(chunk_size, big.mark = ","), "rows\n\n")

# Load spatial reference data once
cat("Loading spatial reference data...\n")
amap <- st_read("AMAP-area/amaplim_lam_poly.shp", quiet = TRUE)
amap <- st_transform(amap, arctic_projection)

land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, arctic_projection)

coastline <- ne_coastline(scale = "medium", returnclass = "sf")
coastline <- st_transform(coastline, arctic_projection)

cat("Spatial data loaded successfully\n\n")

# Check which countries are already completed
completed_countries <- c()
for (country in countries) {
  output_file <- paste0("pelagic_", country$name, ".rds")
  if (file.exists(output_file)) {
    completed_countries <- c(completed_countries, country$name)
    data <- readRDS(output_file)
    cat("✓", toupper(country$name), "already completed:", format(nrow(data), big.mark = ","), "observations\n")
  }
}

if (length(completed_countries) > 0) {
  cat("Skipping completed countries:", paste(completed_countries, collapse = ", "), "\n\n")
}

# Function to process a single chunk
process_country_chunk <- function(country_name, file_path, chunk_num, total_chunks, col_names) {
  
  cat("  Processing chunk", chunk_num, "of", total_chunks, "...")
  
  temp_file <- paste0("temp_", country_name, "_large_", chunk_num, ".rds")
  
  if (file.exists(temp_file)) {
    cat(" already exists, skipping\n")
    return(TRUE)
  }
  
  tryCatch({
    
    skip_rows <- 1 + (chunk_num - 1) * chunk_size
    
    chunk_data <- read_delim(file_path, 
                             delim = "\t", 
                             skip = skip_rows,
                             n_max = chunk_size,
                             col_names = col_names,
                             show_col_types = FALSE)
    
    if (nrow(chunk_data) == 0) {
      saveRDS(data.frame(), temp_file)
      cat(" empty\n")
      return(TRUE)
    }
    
    # Validate coordinates
    if (!all(c("LONGITUDE", "LATITUDE") %in% names(chunk_data))) {
      saveRDS(data.frame(), temp_file)
      cat(" no coordinates\n")
      return(TRUE)
    }
    
    chunk_data <- chunk_data[!is.na(chunk_data$LONGITUDE) & !is.na(chunk_data$LATITUDE), ]
    chunk_data <- chunk_data[abs(chunk_data$LONGITUDE) <= 180 & abs(chunk_data$LATITUDE) <= 90, ]
    
    if (nrow(chunk_data) == 0) {
      saveRDS(data.frame(), temp_file)
      cat(" no valid coords\n")
      return(TRUE)
    }
    
    # Convert to spatial
    data_sf <- st_as_sf(chunk_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    rm(chunk_data); gc()
    
    data_sf <- st_transform(data_sf, arctic_projection)
    
    # Arctic filter
    in_arctic <- lengths(st_intersects(data_sf, amap)) > 0
    arctic_count <- sum(in_arctic)
    
    if (arctic_count == 0) {
      saveRDS(data.frame(), temp_file)
      cat(" no Arctic\n")
      rm(data_sf); gc()
      return(TRUE)
    }
    
    arctic_data <- data_sf[in_arctic, ]
    rm(data_sf, in_arctic); gc()
    
    # Land filter
    on_land <- lengths(st_intersects(arctic_data, land)) > 0
    sea_count <- sum(!on_land)
    
    if (sea_count == 0) {
      saveRDS(data.frame(), temp_file)
      cat(" no sea\n")
      rm(arctic_data); gc()
      return(TRUE)
    }
    
    sea_data <- arctic_data[!on_land, ]
    rm(arctic_data, on_land); gc()
    
    # Distance calculation
    batch_size <- 5000
    n_obs <- nrow(sea_data)
    n_batches <- ceiling(n_obs / batch_size)
    shore_distances <- numeric(n_obs)
    
    for (b in 1:n_batches) {
      start_idx <- (b - 1) * batch_size + 1
      end_idx <- min(b * batch_size, n_obs)
      
      batch_data <- sea_data[start_idx:end_idx, ]
      distances <- st_distance(batch_data, coastline)
      min_distances <- apply(distances, 1, min)
      shore_distances[start_idx:end_idx] <- as.numeric(min_distances)
      
      rm(batch_data, distances, min_distances)
      gc()
    }
    
    sea_data$shore_dist <- shore_distances
    rm(shore_distances); gc()
    
    # Pelagic filter
    pelagic_data <- sea_data %>% 
      filter(shore_dist >= pelagic_distance)
    
    pelagic_count <- nrow(pelagic_data)
    rm(sea_data); gc()
    
    if (pelagic_count > 0) {
      # Add metadata
      pelagic_data$data_source <- country_name
      pelagic_data$count_numeric <- as.numeric(pelagic_data$`OBSERVATION COUNT`)
      pelagic_data$count_numeric[is.na(pelagic_data$count_numeric)] <- 1
      
      # Temporal variables
      pelagic_data$`OBSERVATION DATE` <- as.Date(pelagic_data$`OBSERVATION DATE`)
      pelagic_data$year <- year(pelagic_data$`OBSERVATION DATE`)
      pelagic_data$month <- month(pelagic_data$`OBSERVATION DATE`)
      
      # Date filter
      pelagic_data <- pelagic_data %>%
        filter(`OBSERVATION DATE` >= as.Date("2003-01-01") & 
                 `OBSERVATION DATE` <= as.Date("2025-03-31"))
      
      final_count <- nrow(pelagic_data)
      
      if (final_count > 0) {
        saveRDS(pelagic_data, temp_file)
        cat(" SUCCESS:", final_count, "obs\n")
      } else {
        saveRDS(data.frame(), temp_file)
        cat(" no date range\n")
      }
    } else {
      saveRDS(data.frame(), temp_file)
      cat(" no pelagic\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat(" ERROR:", e$message, "\n")
    saveRDS(data.frame(error = e$message), temp_file)
    gc()
    return(FALSE)
  })
}

# Function to process a complete country
process_country <- function(country) {
  
  country_name <- country$name
  file_path <- country$file
  
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESSING", toupper(country_name), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Check if already completed
  output_file <- paste0("pelagic_", country_name, ".rds")
  if (file.exists(output_file)) {
    cat("✓", toupper(country_name), "already completed, skipping\n\n")
    return(TRUE)
  }
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat("✗ File not found:", file_path, "\n\n")
    return(FALSE)
  }
  
  cat("File:", file_path, "\n")
  cat("Size:", round(file.size(file_path) / 1024^2, 1), "MB\n")
  
  # Get file structure
  header <- read_lines(file_path, n_max = 1)
  col_names <- strsplit(header, "\t")[[1]]
  total_lines <- length(read_lines(file_path)) - 1
  total_chunks <- ceiling(total_lines / chunk_size)
  
  cat("Total lines:", format(total_lines, big.mark = ","), "\n")
  cat("Total chunks:", total_chunks, "\n\n")
  
  # Check existing progress
  existing_files <- list.files(pattern = paste0("^temp_", country_name, "_large_.*\\.rds$"))
  completed_chunks <- as.numeric(gsub(paste0("temp_", country_name, "_large_(\\d+)\\.rds"), "\\1", existing_files))
  remaining_chunks <- setdiff(1:total_chunks, completed_chunks)
  
  if (length(completed_chunks) > 0) {
    cat("Already completed:", length(completed_chunks), "chunks\n")
  }
  cat("Processing:", length(remaining_chunks), "remaining chunks\n\n")
  
  # Process chunks
  for (chunk_num in remaining_chunks) {
    success <- process_country_chunk(country_name, file_path, chunk_num, total_chunks, col_names)
    
    if (!success) {
      cat("Failed on chunk", chunk_num, "for", country_name, "\n")
      cat("Restart R and run this script again to continue\n")
      return(FALSE)
    }
    
    # Progress update every 5 chunks for Russia (smaller file)
    if (chunk_num %% 5 == 0) {
      gc()
      cat("  Completed", chunk_num, "of", total_chunks, "chunks\n")
    }
  }
  
  # Combine results
  cat("\nCombining", country_name, "results...\n")
  final_files <- list.files(pattern = paste0("^temp_", country_name, "_large_.*\\.rds$"))
  
  if (length(final_files) == total_chunks) {
    all_chunks <- list()
    total_obs <- 0
    
    for (file in final_files) {
      data <- readRDS(file)
      if (nrow(data) > 0 && !"error" %in% names(data)) {
        all_chunks[[length(all_chunks) + 1]] <- data
        total_obs <- total_obs + nrow(data)
      }
    }
    
    if (length(all_chunks) > 0) {
      final_data <- do.call(rbind, all_chunks)
      saveRDS(final_data, output_file)
      cat("✓ SUCCESS! Saved", output_file, "with", format(total_obs, big.mark = ","), "observations\n")
      
      # Summary
      cat("Date range:", min(final_data$`OBSERVATION DATE`), "to", max(final_data$`OBSERVATION DATE`), "\n")
      cat("Unique species:", length(unique(final_data$`COMMON NAME`)), "\n")
      
    } else {
      cat("No pelagic data found in", country_name, "\n")
    }
    
    # Clean up
    file.remove(final_files)
    cat("Cleaned up", length(final_files), "temporary files\n\n")
    
    return(TRUE)
    
  } else {
    cat("Processing incomplete for", country_name, "\n")
    cat("Progress:", length(final_files), "of", total_chunks, "chunks\n\n")
    return(FALSE)
  }
}

# MAIN PROCESSING LOOP
cat("Starting processing (Russia focus)...\n\n")

for (country in countries) {
  if (!country$name %in% completed_countries) {
    success <- process_country(country)
    
    if (!success) {
      cat("Processing failed for", country$name, "\n")
      cat("Restart R and run this script again to continue\n")
      break
    }
  }
}

# CURRENT STATUS SUMMARY
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("CURRENT STATUS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

all_countries <- c("iceland", "alaska", "russia")  # Excluding Canada for now
final_outputs <- c()

for (country in all_countries) {
  output_file <- paste0("pelagic_", country, ".rds")
  if (file.exists(output_file)) {
    data <- readRDS(output_file)
    final_outputs <- c(final_outputs, output_file)
    cat("✓", toupper(country), ":", format(nrow(data), big.mark = ","), "observations\n")
  } else {
    cat("✗", toupper(country), ": Not completed\n")
  }
}

