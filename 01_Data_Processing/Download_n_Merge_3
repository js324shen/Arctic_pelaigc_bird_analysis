rm(list = ls())
gc()


library(sf)
library(dplyr)
library(lubridate)
library(rnaturalearth)
library(readr)

sf_use_s2(FALSE)

# NEW BIGGER CHUNK SIZE FOR REMAINING PROCESSING
chunk_size <- 100000  # BIGGER: 100k rows instead of 50k (2x faster!)
old_chunk_size <- 50000  # What we used before
pelagic_distance <- 3218
arctic_projection <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

file_path <- "ebd/ebd_CA_relMar-2025.txt"

cat("=== CANADA RESUME WITH BIGGER CHUNKS ===\n")
cat("Strategy: Continue from where we left off, but with bigger chunks for speed\n")
cat("Previous chunk size:", format(old_chunk_size, big.mark = ","), "rows\n")
cat("NEW chunk size:", format(chunk_size, big.mark = ","), "rows (2x bigger!)\n\n")

# Check existing progress with old chunk size
existing_old_files <- list.files(pattern = "^temp_canada_safe_.*\\.rds$")
if (length(existing_old_files) > 0) {
  old_chunks <- as.numeric(gsub("temp_canada_safe_(\\d+)\\.rds", "\\1", existing_old_files))
  max_old_chunk <- max(old_chunks)
  cat("Found", length(existing_old_files), "files from previous run (50k chunks)\n")
  cat("Last completed 50k chunk:", max_old_chunk, "\n")
  
  # Calculate where to start with new bigger chunks
  # We processed 1,234 chunks of 50k = 61,700,000 rows
  rows_processed <- max_old_chunk * old_chunk_size
  new_start_chunk <- floor(rows_processed / chunk_size) + 1
  
  cat("Rows already processed:", format(rows_processed, big.mark = ","), "\n")
  cat("Starting NEW 100k chunks from chunk:", new_start_chunk, "\n\n")
} else {
  cat("No previous temp files found - starting fresh\n")
  new_start_chunk <- 1
}

# Load spatial reference data
cat("Loading spatial reference data...\n")
amap <- st_read("AMAP-area/amaplim_lam_poly.shp", quiet = TRUE)
amap <- st_transform(amap, arctic_projection)

land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, arctic_projection)

coastline <- ne_coastline(scale = "medium", returnclass = "sf")
coastline <- st_transform(coastline, arctic_projection)

cat("Spatial data loaded\n\n")

# Check if already completed
if (file.exists("pelagic_canada.rds")) {
  cat("✓ Complete Canada file already exists!\n")
  canada_data <- readRDS("pelagic_canada.rds")
  cat("Observations:", format(nrow(canada_data), big.mark = ","), "\n")
  stop("Canada already fully processed!")
}

# Get header
header <- read_lines(file_path, n_max = 1)
col_names <- strsplit(header, "\t")[[1]]

# Check for existing NEW chunk files
existing_new_files <- list.files(pattern = "^temp_canada_big_.*\\.rds$")
if (length(existing_new_files) > 0) {
  new_chunks <- as.numeric(gsub("temp_canada_big_(\\d+)\\.rds", "\\1", existing_new_files))
  completed_new_chunks <- sort(new_chunks)
  next_chunk <- max(completed_new_chunks) + 1
  cat("Found", length(existing_new_files), "big chunk files\n")
  cat("Resuming big chunks from:", next_chunk, "\n\n")
} else {
  next_chunk <- new_start_chunk
  cat("Starting fresh with big chunks from:", next_chunk, "\n\n")
}

# Big chunk processing function
process_canada_big_chunk <- function(chunk_num) {
  
  temp_file <- paste0("temp_canada_big_", chunk_num, ".rds")
  
  if (file.exists(temp_file)) {
    return("exists")
  }
  
  cat("Big chunk", chunk_num, "...")
  
  tryCatch({
    
    skip_rows <- 1 + (chunk_num - 1) * chunk_size
    
    chunk_data <- read_delim(file_path, 
                             delim = "\t", 
                             skip = skip_rows,
                             n_max = chunk_size,
                             col_names = col_names,
                             show_col_types = FALSE,
                             progress = FALSE)
    
    if (nrow(chunk_data) == 0) {
      cat(" END OF FILE\n")
      return("end_of_file")
    }
    
    # Coordinate validation
    if (!all(c("LONGITUDE", "LATITUDE") %in% names(chunk_data))) {
      saveRDS(data.frame(), temp_file)
      cat(" no coords\n")
      return("success")
    }
    
    chunk_data <- chunk_data[!is.na(chunk_data$LONGITUDE) & !is.na(chunk_data$LATITUDE), ]
    chunk_data <- chunk_data[abs(chunk_data$LONGITUDE) <= 180 & abs(chunk_data$LATITUDE) <= 90, ]
    
    if (nrow(chunk_data) == 0) {
      saveRDS(data.frame(), temp_file)
      cat(" no valid coords\n")
      return("success")
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
      return("success")
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
      return("success")
    }
    
    sea_data <- arctic_data[!on_land, ]
    rm(arctic_data, on_land); gc()
    
    # Distance calculation (bigger batches for bigger chunks)
    batch_size <- 5000  # Bigger batches too
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
      pelagic_data$data_source <- "canada"
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
    
    return("success")
    
  }, error = function(e) {
    cat(" ERROR:", e$message, "\n")
    saveRDS(data.frame(error = e$message), temp_file)
    gc()
    return("error")
  })
}

# MAIN PROCESSING WITH BIGGER CHUNKS
cat("Starting big chunk processing from chunk", next_chunk, "...\n")
cat("Each chunk now processes", format(chunk_size, big.mark = ","), "rows (2x faster!)\n\n")

chunk_num <- next_chunk
session_chunk_count <- 0

while (TRUE) {
  
  result <- process_canada_big_chunk(chunk_num)
  
  if (result == "end_of_file") {
    cat("✓ Reached end of file at big chunk", chunk_num - 1, "\n")
    break
  } else if (result == "error") {
    cat("✗ Error on big chunk", chunk_num, "\n")
    cat("Restart R and run script again to continue\n")
    break
  } else if (result == "exists") {
    cat("Big chunk", chunk_num, "already exists, skipping\n")
  }
  
  chunk_num <- chunk_num + 1
  session_chunk_count <- session_chunk_count + 1
  
  # Memory management every 2 chunks (since chunks are bigger)
  if (session_chunk_count %% 2 == 0) {
    gc()
  }
  
  # Progress updates every 5 big chunks
  if (session_chunk_count %% 5 == 0) {
    cat("  Processed", session_chunk_count, "big chunks this session (chunk", chunk_num - 1, ")\n")
  }
  
  # Safety break every 10 big chunks (since they're bigger)
  if (session_chunk_count %% 10 == 0) {
    cat("\n=== BIG CHUNK CHECKPOINT ===\n")
    cat("Processed", session_chunk_count, "big chunks this session\n")
    cat("Currently at big chunk", chunk_num - 1, "\n")
    cat("Taking 5 second break...\n")
    Sys.sleep(5)
    gc()
    cat("Continuing...\n\n")
  }
}

# SMART COMBINING - Use partial file if exists
cat("\n=== COMBINING CANADA RESULTS ===\n")

partial_file <- "pelagic_canada_partial_1234.rds"
new_files <- list.files(pattern = "^temp_canada_big_.*\\.rds$")

if (file.exists(partial_file) && length(new_files) > 0) {
  cat("Merging partial file with", length(new_files), "new chunks...\n")
  
  partial_data <- readRDS(partial_file)
  new_chunks <- list()
  
  for (file in new_files) {
    data <- readRDS(file)
    if (nrow(data) > 0 && !"error" %in% names(data)) {
      new_chunks[[length(new_chunks) + 1]] <- data
    }
  }
  
  if (length(new_chunks) > 0) {
    new_data <- do.call(rbind, new_chunks)
    final_canada <- rbind(partial_data, new_data)
    
    saveRDS(final_canada, "pelagic_canada.rds")
    cat("✓ SUCCESS! Complete Canada:", format(nrow(final_canada), big.mark = ","), "observations\n")
    cat("Unique species:", length(unique(final_canada$`COMMON NAME`)), "\n")
    
    file.remove(new_files)
    cat("Cleaned up new temp files\n")
  }
  
} else if (length(new_files) > 0) {
  cat("Processing", length(new_files), "new chunks only...\n")
  # Handle case where no partial file exists
}

cat("\nCanada processing complete!\n")
