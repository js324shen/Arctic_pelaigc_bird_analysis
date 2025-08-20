install.packages("rvest")
install.packages("purrr")

library(sf)      
library(dplyr)    
library(rvest)   
library(purrr)    
library(stringr)  
library(lubridate) 


# Create folders to organize data
dir.create("raw_data", showWarnings = FALSE)
dir.create("processed_data", showWarnings = FALSE)

# Function to download and process files for a given year
process_year <- function(year, max_files = 12) {
  # Construct base URL to the directory for that year
  base_url <- "https://noaadata.apps.nsidc.org/NOAA/G10033/north/weekly/shapefile/"
  
  # Read the directory page
  page <- read_html(base_url)
  
  # Extract all file links
  all_files <- page %>% 
    html_nodes("a") %>% 
    html_attr("href") %>%
    # Filter for files matching the year pattern
    str_subset(paste0("nh_", year))
  
  # Select a subset of files (e.g., one per month or specific weeks)
  # This helps manage storage by downloading fewer files
  # Here we're selecting files approximately monthly
  
  if(length(all_files) > max_files) {
    # Sort files to ensure chronological order
    all_files <- sort(all_files)
    
    # Select evenly spaced files throughout the year
    indices <- round(seq(1, length(all_files), length.out = max_files))
    selected_files <- all_files[indices]
  } else {
    selected_files <- all_files
  }
  
  # Download and process each selected file
  downloaded_files <- c()
  
  for(file in selected_files) {
    # Construct full URL
    file_url <- paste0(base_url, file)
    
    # Define local filename
    local_file <- file.path("raw_data", file)
    
    # Download file
    cat("Downloading", file, "...\n")
    download.file(file_url, local_file, mode = "wb")
    
    # Add to list of downloaded files
    downloaded_files <- c(downloaded_files, local_file)
  }
  
  return(downloaded_files)
}

# Function to extract date from filename
extract_date <- function(filename) {
  # Filenames are in format nh_YYYYMMDD.zip
  date_str <- str_extract(basename(filename), "\\d{8}")
  as.Date(date_str, format = "%Y%m%d")
}

# Function to process a shapefile and extract fast ice
process_shapefile <- function(zip_file) {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Extract date from filename
  file_date <- extract_date(zip_file)
  year_month <- format(file_date, "%Y_%m")
  
  # Unzip the file to the temporary directory
  unzip(zip_file, exdir = temp_dir)
  
  # Find the shapefile in the extracted files
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  
  if(is.na(shp_file)) {
    warning("No shapefile found in ", zip_file)
    return(NULL)
  }
  
  # Read the shapefile
  ice_data <- sf::st_read(shp_file)
  
  # Extract fast ice (you'll need to adjust this based on actual file structure)
  # The exact filter depends on how fast ice is coded in the data
  # This is a placeholder - you'll need to inspect your actual data structure
  fast_ice <- ice_data %>%
    filter(grepl("fast", ICETYPE, ignore.case = TRUE) | ICETYPE == "8") %>%
    mutate(date = file_date)
  
  # Save processed data
  output_file <- file.path("processed_data", paste0("fast_ice_", format(file_date, "%Y%m%d"), ".rds"))
  saveRDS(fast_ice, output_file)
  
  # Clean up temporary files
  unlink(temp_dir, recursive = TRUE)
  
  return(output_file)
}

# Main execution logic
# Download files for selected years
years_to_process <- 2003:2025 

# Process each year
all_downloaded <- list()
for(year in years_to_process) {
  cat("Processing year", year, "\n")
  year_files <- process_year(year, max_files = 12) # Get ~monthly data
  all_downloaded[[as.character(year)]] <- year_files
}

# Process all downloaded shapefiles
all_processed <- list()
for(year in names(all_downloaded)) {
  year_processed <- map(all_downloaded[[year]], process_shapefile)
  all_processed[[year]] <- year_processed
}



#Download interrupted at 2012

# Function to download with retry capability
download_with_retry <- function(url, destfile, max_tries = 3) {
  for(i in 1:max_tries) {
    tryCatch({
      download.file(url, destfile, mode = "wb", timeout = 120)  # Increase timeout
      return(TRUE)  # Success
    }, error = function(e) {
      cat("Attempt", i, "failed:", conditionMessage(e), "\n")
      if(i == max_tries) {
        cat("All attempts failed for", url, "\n")
        return(FALSE)
      }
      Sys.sleep(2)  # Wait before retry
    })
  }
}

# Continue processing from the point of failure (adjust starting year as needed)
remaining_years <- 2012:2025
for(year in remaining_years) {
  cat("Processing year", year, "\n")
  
  # Get list of files for this year from the website
  base_url <- "https://noaadata.apps.nsidc.org/NOAA/G10033/north/weekly/shapefile/"
  page <- read_html(base_url)
  
  all_files <- page %>% 
    html_nodes("a") %>% 
    html_attr("href") %>%
    str_subset(paste0("nh_", substr(as.character(year), 1, 4)))
  
  # Sort files to ensure chronological order
  all_files <- sort(all_files)
  
  # Select approximately monthly files (12 per year)
  if(length(all_files) > 12) {
    indices <- round(seq(1, length(all_files), length.out = 12))
    selected_files <- all_files[indices]
  } else {
    selected_files <- all_files
  }
  
  # Download each selected file with retry
  for(file in selected_files) {
    file_url <- paste0(base_url, file)
    local_file <- file.path("raw_data", file)
    
    # Skip if file already exists
    if(file.exists(local_file)) {
      cat("File", file, "already exists, skipping\n")
      next
    }
    
    cat("Downloading", file, "...\n")
    success <- download_with_retry(file_url, local_file)
    
    if(success) {
      all_downloaded[[as.character(year)]] <- c(all_downloaded[[as.character(year)]], local_file)
    }
    
    # Add a small delay between downloads to be kind to the server
    Sys.sleep(1)
  }
}




library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Create directories for processed data if they don't exist
dir.create("processed_data", showWarnings = FALSE)

# Function to examine a sample shapefile's structure
examine_shapefile <- function(zip_file) {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Extract date from filename
  file_date <- str_extract(basename(zip_file), "\\d{8}")
  file_date <- as.Date(file_date, format = "%Y%m%d")
  
  cat("Examining file from", format(file_date, "%Y-%m-%d"), "\n")
  
  # Unzip the file to the temporary directory
  unzip(zip_file, exdir = temp_dir)
  
  # Find the shapefile in the extracted files
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if(length(shp_files) == 0) {
    warning("No shapefile found in ", zip_file)
    return(NULL)
  }
  
  # Read the first shapefile
  ice_data <- sf::st_read(shp_files[1], quiet = TRUE)
  
  # Print column names to understand structure
  cat("Column names:", paste(names(ice_data), collapse = ", "), "\n")
  
  # Look for columns that might indicate ice type
  ice_type_cols <- names(ice_data)[grepl("type|ice|CT|STAGE|CODE", names(ice_data), ignore.case = TRUE)]
  if(length(ice_type_cols) > 0) {
    cat("Potential ice type columns:", paste(ice_type_cols, collapse = ", "), "\n")
    
    # Sample unique values from each potential column
    for(col in ice_type_cols) {
      if(col %in% names(ice_data)) {
        unique_vals <- unique(ice_data[[col]])
        cat("Unique values in", col, ":", paste(head(unique_vals, 10), collapse = ", "), "\n")
      }
    }
  }
  
  # Clean up
  unlink(temp_dir, recursive = TRUE)
  
  return(list(
    date = file_date,
    structure = names(ice_data),
    ice_type_cols = ice_type_cols
  ))
}

sample_files <- list.files(pattern = "nh_.*\\.zip", full.names = TRUE)
set.seed(123)  # For reproducibility
sample_indices <- sample(length(sample_files), min(5, length(sample_files)))
sample_results <- lapply(sample_files[sample_indices], examine_shapefile)




#Trouble-shooting
all_files <- list.files(pattern = "nh_.*\\.zip", full.names = TRUE)
print(length(all_files))
print(head(all_files, 3))  # Show first 3 files

# Check current working directory
print(getwd())

# List all files in the current directory
print(list.files())

# Check if we have a raw_data subdirectory
print(dir.exists("raw_data"))

# If raw_data exists, list files there
if(dir.exists("raw_data")) {
  print(list.files("raw_data", pattern = "nh_.*\\.zip"))
}

# Try looking for files in the raw_data directory
all_files <- list.files("raw_data", pattern = "nh_.*\\.zip", full.names = TRUE)
print(length(all_files))
print(head(all_files, 3))  # Show first 3 files


# Sample a few files from different years to understand the structure
set.seed(123)  # For reproducibility
sample_indices <- sample(length(all_files), min(5, length(all_files)))
sample_files <- all_files[sample_indices]
print(sample_files)  # Show which files we'll examine

# Examine the first sample file to understand its structure
result <- examine_shapefile(sample_files[1])
print(result)






# Function to extract fast ice data
extract_fast_ice <- function(zip_file) {
  # Create a temporary directory
  temp_dir <- tempdir()
  
  # Extract date from filename
  file_date <- str_extract(basename(zip_file), "\\d{8}")
  file_date <- as.Date(file_date, format = "%Y%m%d")
  
  cat("Processing file from", format(file_date, "%Y-%m-%d"), "\n")
  
  # Unzip the file to the temporary directory
  tryCatch({
    unzip(zip_file, exdir = temp_dir)
  }, error = function(e) {
    cat("Error unzipping", zip_file, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Find the shapefile in the extracted files
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if(length(shp_files) == 0) {
    cat("No shapefile found in", zip_file, "\n")
    return(NULL)
  }
  
  # Read the shapefile
  ice_data <- tryCatch({
    sf::st_read(shp_files[1], quiet = TRUE)
  }, error = function(e) {
    cat("Error reading shapefile from", zip_file, ":", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if(is.null(ice_data) || nrow(ice_data) == 0) {
    cat("Empty or null shapefile in", zip_file, "\n")
    return(NULL)
  }
  
  # Extract fast ice based on the 'fast' column if it exists
  fast_ice <- NULL
  
  if("fast" %in% names(ice_data)) {
    # Filter for polygons where fast > 0 (assuming fast column contains proportion/presence)
    fast_ice <- ice_data %>%
      filter(fast > 0)
  } else if("POLY_TYPE" %in% names(ice_data)) {
    # Try alternative approach if no fast column
    # This is a fallback in case structure differs
    fast_ice <- ice_data %>%
      filter(grepl("fast|shore|F", POLY_TYPE, ignore.case = TRUE))
  } else {
    cat("No suitable column found to identify fast ice in", zip_file, "\n")
    return(NULL)
  }
  
  # Check if we found any fast ice
  if(is.null(fast_ice) || nrow(fast_ice) == 0) {
    cat("No fast ice features found in", zip_file, "\n")
    # Return empty sf object with date information
    return(st_sf(
      date = file_date,
      year = year(file_date),
      month = month(file_date),
      season = case_when(
        month(file_date) %in% c(12, 1, 2) ~ "Winter",
        month(file_date) %in% c(3, 4, 5) ~ "Spring",
        month(file_date) %in% c(6, 7, 8) ~ "Summer",
        month(file_date) %in% c(9, 10, 11) ~ "Fall"
      ),
      fast_ice_present = FALSE,
      geometry = st_sfc(crs = st_crs(ice_data))
    ))
  }
  
  # Add date and season information
  fast_ice$date <- file_date
  fast_ice$year <- year(file_date)
  fast_ice$month <- month(file_date)
  fast_ice$season <- case_when(
    fast_ice$month %in% c(12, 1, 2) ~ "Winter",
    fast_ice$month %in% c(3, 4, 5) ~ "Spring",
    fast_ice$month %in% c(6, 7, 8) ~ "Summer",
    fast_ice$month %in% c(9, 10, 11) ~ "Fall"
  )
  fast_ice$fast_ice_present <- TRUE
  
  # Clean up
  unlink(temp_dir, recursive = TRUE)
  
  return(fast_ice)
}

# Process files in batches to manage memory
process_batch <- function(files, batch_size = 10) {
  num_batches <- ceiling(length(files) / batch_size)
  
  for(i in 1:num_batches) {
    cat("\nProcessing batch", i, "of", num_batches, "\n")
    
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, length(files))
    
    batch_files <- files[start_idx:end_idx]
    batch_results <- list()
    
    for(j in 1:length(batch_files)) {
      file_path <- batch_files[j]
      cat("Processing file", j, "of", length(batch_files), ":", basename(file_path), "\n")
      
      # Extract fast ice data
      fast_ice <- extract_fast_ice(file_path)
      
      # Store result if not NULL
      if(!is.null(fast_ice)) {
        batch_results[[j]] <- fast_ice
      }
    }
    
    # Save this batch
    batch_file <- file.path("processed_data", paste0("fast_ice_batch_", i, ".rds"))
    saveRDS(batch_results, batch_file)
    
    # Clean up to free memory
    rm(batch_results)
    gc()
  }
}

# Process all files
process_batch(all_files, batch_size = 10)

# Combine processed batches
combine_batches <- function() {
  # List all batch files
  batch_files <- list.files("processed_data", pattern = "fast_ice_batch_.*\\.rds", full.names = TRUE)
  
  cat("Found", length(batch_files), "batch files to combine\n")
  
  # Initialize empty list to store all data
  all_data <- list()
  
  # Load each batch and add non-null elements
  for(i in 1:length(batch_files)) {
    cat("Loading batch file", i, ":", basename(batch_files[i]), "\n")
    batch_data <- readRDS(batch_files[i])
    valid_indices <- which(!sapply(batch_data, is.null))
    
    if(length(valid_indices) > 0) {
      all_data <- c(all_data, batch_data[valid_indices])
    }
  }
  
  cat("Loaded", length(all_data), "valid datasets\n")
  
  # Combine all data into a single sf object if possible
  if(length(all_data) > 0) {
    # Try to combine all into one object
    tryCatch({
      cat("Attempting to combine all datasets into a single object...\n")
      combined_data <- do.call(rbind, all_data)
      saveRDS(combined_data, file.path("processed_data", "all_fast_ice_data.rds"))
      cat("Successfully created and saved combined dataset\n")
      return(combined_data)
    }, error = function(e) {
      cat("Error combining all data:", conditionMessage(e), "\n")
      cat("Saving as list instead\n")
      saveRDS(all_data, file.path("processed_data", "all_fast_ice_data_list.rds"))
      return(all_data)
    })
  } else {
    cat("No valid data found in any batch\n")
    return(NULL)
  }
}

# Run this to combine all batches
all_fast_ice <- combine_batches()
