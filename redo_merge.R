rm(list = ls())
gc()

library(dplyr)
library(lubridate)

cat("=== ARCTIC PELAGIC MASTER DATASET MERGER ===\n")
cat("Combining all countries into final comprehensive dataset\n\n")

# Define all possible country/region files
country_files <- c(
  "pelagic_greenland.rds",
  "pelagic_svalbard.rds",
  "pelagic_highsea.rds",
  "pelagic_norway.rds",
  "pelagic_iceland.rds",
  "pelagic_alaska.rds",        # Alaska from original script
  "pelagic_russia.rds",
  "pelagic_canada.rds"
)

# Check which files exist
existing_files <- country_files[file.exists(country_files)]
missing_files <- country_files[!file.exists(country_files)]

cat("=== FILE STATUS ===\n")
if (length(existing_files) > 0) {
  cat("Found files:\n")
  for (file in existing_files) {
    size_mb <- round(file.size(file) / 1024 / 1024, 1)
    cat("  ✓", file, "(", size_mb, "MB )\n")
  }
}

if (length(missing_files) > 0) {
  cat("Missing files:\n")
  for (file in missing_files) {
    cat("  ✗", file, "\n")
  }
}

if (length(existing_files) == 0) {
  stop("No country files found! Please ensure you have processed at least one country.")
}

cat("\n=== LOADING DATASETS ===\n")

# Load all existing datasets
datasets <- list()
total_observations <- 0

for (file in existing_files) {
  country_name <- gsub("pelagic_|\\.rds", "", file)
  cat("Loading", country_name, "...")
  
  data <- readRDS(file)
  
  if (nrow(data) > 0) {
    datasets[[country_name]] <- data
    cat(" SUCCESS:", format(nrow(data), big.mark = ","), "observations\n")
    total_observations <- total_observations + nrow(data)
  } else {
    cat(" EMPTY - skipping\n")
  }
}

cat("\nTotal observations across all countries:", format(total_observations, big.mark = ","), "\n\n")

if (length(datasets) == 0) {
  stop("No valid datasets found!")
}

# Standardize datasets
cat("=== STANDARDIZING DATASETS ===\n")
all_columns <- unique(unlist(lapply(datasets, names)))
cat("Total unique columns:", length(all_columns), "\n")

standardized_datasets <- list()

for (country in names(datasets)) {
  cat("Standardizing", country, "...")
  data <- datasets[[country]]
  
  # Add missing columns with NA
  for (col in all_columns) {
    if (!col %in% names(data)) {
      data[[col]] <- NA
    }
  }
  
  # Fix date formatting - handle numeric dates
  if (is.numeric(data$`OBSERVATION DATE`)) {
    data$`OBSERVATION DATE` <- as.Date(data$`OBSERVATION DATE`, origin = "1970-01-01")
  } else {
    data$`OBSERVATION DATE` <- as.Date(data$`OBSERVATION DATE`)
  }
  
  # Ensure consistent data types
  data$year <- as.numeric(data$year)
  data$month <- as.numeric(data$month)
  data$count_numeric <- as.numeric(data$count_numeric)
  data$shore_dist <- as.numeric(data$shore_dist)
  
  standardized_datasets[[country]] <- data
  cat(" ✓\n")
}

# MERGE ALL DATASETS
cat("\n=== MERGING DATASETS ===\n")
cat("Combining", length(standardized_datasets), "datasets...\n")

arctic_master <- do.call(rbind, standardized_datasets)
rownames(arctic_master) <- NULL

cat("✓ MERGE COMPLETE!\n")
cat("Master dataset size:", format(nrow(arctic_master), big.mark = ","), "observations\n\n")

# SUMMARY
cat("=== ARCTIC PELAGIC MASTER DATASET SUMMARY ===\n")
cat("Total observations:", format(nrow(arctic_master), big.mark = ","), "\n")
cat("Total columns:", ncol(arctic_master), "\n")

# Temporal coverage (with proper date formatting)
cat("Date range:", as.character(min(arctic_master$`OBSERVATION DATE`, na.rm = TRUE)), 
    "to", as.character(max(arctic_master$`OBSERVATION DATE`, na.rm = TRUE)), "\n")
cat("Year range:", min(arctic_master$year, na.rm = TRUE), 
    "to", max(arctic_master$year, na.rm = TRUE), "\n")

# Species diversity
total_species <- length(unique(arctic_master$`COMMON NAME`))
cat("Total unique species:", total_species, "\n")

# Country breakdown
cat("\nObservations by country:\n")
country_summary <- arctic_master %>%
  group_by(data_source) %>%
  summarise(
    observations = n(),
    species = length(unique(`COMMON NAME`)),
    .groups = 'drop'
  ) %>%
  arrange(desc(observations))

for (i in 1:nrow(country_summary)) {
  cat("  ", country_summary$data_source[i], ":", 
      format(country_summary$observations[i], big.mark = ","), "obs,",
      country_summary$species[i], "species\n")
}


# Save the master dataset
output_file <- paste0("arctic_pelagic_master_", Sys.Date(), ".rds")


cat("\nSaving master dataset...\n")
saveRDS(arctic_master, output_file)
cat("✓ SAVED:", output_file, "\n")

cat("\n" , rep("=", 60), "\n")
cat(rep("=", 60), "\n")
cat("Your comprehensive Arctic pelagic bird dataset is ready!\n")
cat("File:", output_file, "\n")
cat("Size:", format(nrow(arctic_master), big.mark = ","), "observations\n")
cat("Species:", total_species, "\n")
cat("Countries:", length(unique(arctic_master$data_source)), "\n")
cat(rep("=", 60), "\n")
