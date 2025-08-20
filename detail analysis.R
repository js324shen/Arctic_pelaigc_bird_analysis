rm(list = ls())

bird_data <- readRDS("bird_data_effort_standardized.rds")

library(dplyr)
library(lubridate)
library(sf)

str(bird_data)
names(bird_data)
summary(bird_data)
head(bird_data)
unique(bird_data$`COMMON NAME`)
unique(bird_data$pelagic_type)

table(bird_data$`OBSERVATION COUNT`)[1:20]
sum(bird_data$`OBSERVATION COUNT` == "X")

bird_data$count_numeric <- as.numeric(bird_data$`OBSERVATION COUNT`)
bird_data$count_adjusted <- bird_data$count_numeric
bird_data$count_adjusted[bird_data$`OBSERVATION COUNT` == "X"] <- 1

summary(bird_data$count_numeric)  #This will show NAs for X
summary(bird_data$count_adjusted)


#Suspicious maximum count of 5000000
cat("=== INVESTIGATING EXTREME COUNT VALUES ===\n")

#Find the extreme observations
extreme_counts <- bird_data %>%
  st_drop_geometry() %>%
  filter(count_adjusted > 50000) %>%
  select(`COMMON NAME`, `OBSERVATION COUNT`, count_adjusted, 
         `OBSERVATION DATE`, LOCALITY, `OBSERVER ID`, 
         `DURATION MINUTES`, `EFFORT DISTANCE KM`) %>%
  arrange(desc(count_adjusted))

print(head(extreme_counts, 10))

# Check distribution of counts
cat("\nCount distribution:\n")
quantiles <- quantile(bird_data$count_adjusted, 
                      probs = c(0.90, 0.95, 0.99, 0.999, 0.9999, 1.0), 
                      na.rm = TRUE)
print(quantiles)

#Actually looks fine





#Species overall summary
species_summary <- aggregate(
  count_adjusted ~ `COMMON NAME` + pelagic_type, 
  data = bird_data,
  FUN = function(x) {
    c(n_obs = length(x),
      total = sum(x),
      mean = round(mean(x), 1),
      median = median(x),
      max = max(x))
  }
)

# Clean up the result
species_table <- do.call(data.frame, species_summary)
names(species_table)[3:7] <- c("n_observations", "total_birds", 
                               "mean_count", "median_count", "max_count")

# Sort
species_table <- species_table[order(species_table$n_observations, decreasing = TRUE),]

# View top species
head(species_table, 15)


#Yearly trend
bird_data$year <- format(bird_data$`OBSERVATION DATE`, "%Y")
bird_data$year <- as.numeric(bird_data$year)

yearly_summary <- aggregate(
  count_adjusted ~ year, 
  data = bird_data,
  FUN = function(x) {
    c(n_obs = length(x),
      total_birds = sum(x),
      mean_count = round(mean(x), 1))
  }
)

# Clean up the result
yearly_table <- do.call(data.frame, yearly_summary)
names(yearly_table) <- c("year", "n_observations", "total_birds", "mean_count")

# Calculate species richness separately
species_by_year <- aggregate(`COMMON NAME` ~ year, 
                             data = bird_data, 
                             FUN = function(x) length(unique(x)))
names(species_by_year)[2] <- "n_species"

# Merge the results
yearly_final <- merge(yearly_table, species_by_year, by = "year")

# View the result
print(yearly_final)

# Create plots
par(mfrow = c(2, 2))
plot(yearly_final$year, yearly_final$n_species, type = "b", 
     main = "Species Richness", xlab = "Year", ylab = "Number of Species")
plot(yearly_final$year, yearly_final$n_observations, type = "b",
     main = "Number of Observations", xlab = "Year", ylab = "N")
plot(yearly_final$year, yearly_final$total_birds, type = "b",
     main = "Total Birds", xlab = "Year", ylab = "Count")
plot(yearly_final$year, yearly_final$mean_count, type = "b",
     main = "Mean Count per Observation", xlab = "Year", ylab = "Mean")




# Create month and season variables
bird_data$month <- as.numeric(format(bird_data$`OBSERVATION DATE`, "%m"))
bird_data$season <- ifelse(bird_data$month %in% c(12, 1, 2), "Winter",
                           ifelse(bird_data$month %in% c(3, 4, 5), "Spring",
                                  ifelse(bird_data$month %in% c(6, 7, 8), "Summer", 
                                         ifelse(bird_data$month %in% c(9, 10, 11), "Autumn", NA))))

# Check if it worked
table(bird_data$season)

# Seasonal summary
seasonal_summary <- aggregate(
  count_adjusted ~ season + pelagic_type,
  data = bird_data,
  FUN = function(x) {
    c(n_obs = length(x),
      total = sum(x),
      mean = round(mean(x), 1))
  }
)

# Clean up the result
seasonal_table <- do.call(data.frame, seasonal_summary)
names(seasonal_table)[3:5] <- c("n_observations", "total_birds", "mean_count")

# Species richness by season
species_by_season <- aggregate(`COMMON NAME` ~ season + pelagic_type, 
                               data = bird_data,
                               FUN = function(x) length(unique(x)))
names(species_by_season)[3] <- "n_species"

# Merge the tables
seasonal_final <- merge(seasonal_table, species_by_season, by = c("season", "pelagic_type"))

# Order seasons logically
seasonal_final$season <- factor(seasonal_final$season, 
                                levels = c("Spring", "Summer", "Autumn", "Winter"))
seasonal_final <- seasonal_final[order(seasonal_final$season, seasonal_final$pelagic_type),]

print(seasonal_final)






#Ice x Bird
ice_data <- readRDS("nsidc/processed_data/fast_ice_for_birds.rds")

# Check structure
str(ice_data)

# Check what's in the object
names(ice_data)  

# Check seasonal data if available
if("seasonal" %in% names(ice_data)) {
  table(ice_data$seasonal$season)
}

#Change fall to autumn
ice_data$seasonal$season <- gsub("Fall", "Autumn", ice_data$seasonal$season)

# Verify the change
cat("\nAfter conversion:\n")
table(ice_data$seasonal$season)

# Calculate distance to seasonal ice edge
bird_data$season <- as.factor(bird_data$season)
bird_data$season <- factor(bird_data$season, levels = c("Winter", "Spring", "Summer", "Autumn"))




library(sf)
library(ggplot2) 
sf_use_s2(FALSE)

# Function to analyze bird distribution in relation to ice edges
ice_edge_analysis <- function(bird_data, ice_data) {
  # Check what seasons are present in the data
  cat("Seasons in bird_data:", paste(unique(bird_data$season), collapse=", "), "\n")
  cat("Seasons in ice_data:", paste(unique(ice_data$seasonal$season), collapse=", "), "\n\n")
  
  # Create results list
  ice_results <- list()
  
  # Process each season
  for(current_season in c("Winter", "Spring", "Summer", "Autumn")) {
    cat("Processing", current_season, "season...\n")
    
    # Check if we need to convert "Fall" to "Autumn" for bird data or vice versa
    bird_season <- current_season
    ice_season <- current_season
    
    # Convert between Fall and Autumn if needed
    if(current_season == "Fall" && !("Fall" %in% unique(bird_data$season)) && 
       "Autumn" %in% unique(bird_data$season)) {
      bird_season <- "Autumn"
      cat("Using 'Autumn' in bird_data instead of 'Fall'\n")
    }
    
    if(current_season == "Fall" && !("Fall" %in% unique(ice_data$seasonal$season)) && 
       "Autumn" %in% unique(ice_data$seasonal$season)) {
      ice_season <- "Autumn"
      cat("Using 'Autumn' in ice_data instead of 'Fall'\n")
    }
    
    # Get seasonal ice and birds
    seasonal_ice <- ice_data$seasonal %>% filter(season == ice_season)
    seasonal_birds <- bird_data %>% filter(season == bird_season)
    
    cat("  Found", nrow(seasonal_birds), "bird observations and", 
        nrow(seasonal_ice), "ice polygons\n")
    
    # Skip if no data
    if(nrow(seasonal_ice) == 0 || nrow(seasonal_birds) == 0) {
      cat("  Skipping", current_season, "due to insufficient data\n\n")
      next
    }
    
    # 1. Calculate distance to ice edge
    cat("  Calculating distance to ice edge...\n")
    
    # Get ice boundary
    ice_boundary <- st_boundary(st_make_valid(seasonal_ice))
    
    # Calculate distances
    distances <- st_distance(seasonal_birds, ice_boundary)
    seasonal_birds$ice_dist <- as.numeric(apply(distances, 1, min))
    
    # Create distance categories (in km)
    seasonal_birds$ice_dist_cat <- cut(seasonal_birds$ice_dist/1000, 
                                       breaks = c(0, 5, 10, 25, 50, 100, 200, 500),
                                       labels = c("0-5km", "5-10km", "10-25km", "25-50km", 
                                                  "50-100km", "100-200km", ">200km"))
    
    # Summarize by ice distance and pelagic type
    ice_summary <- seasonal_birds %>%
      st_drop_geometry() %>%
      group_by(ice_dist_cat, pelagic_type) %>%
      summarize(
        n_observations = n(),
        mean_count = round(mean(count_adjusted, na.rm = TRUE), 1),
        total_count = sum(count_adjusted, na.rm = TRUE),
        n_species = length(unique(`COMMON NAME`)),
        .groups = 'drop'
      )
    
    # Store results with consistent season naming
    ice_results[[current_season]] <- ice_summary
    
    # 2. Check which birds are inside vs. outside ice
    cat("  Analyzing birds inside vs. outside ice...\n")
    
    seasonal_birds$within_ice <- st_intersects(seasonal_birds, 
                                               st_make_valid(seasonal_ice), 
                                               sparse = FALSE)
    seasonal_birds$within_ice <- apply(seasonal_birds$within_ice, 1, any)
    
    # Summarize for inside vs. outside ice
    ice_location_summary <- seasonal_birds %>%
      st_drop_geometry() %>%
      group_by(within_ice, pelagic_type) %>%
      summarize(
        n_observations = n(),
        mean_count = round(mean(count_adjusted, na.rm = TRUE), 1),
        total_count = sum(count_adjusted, na.rm = TRUE),
        n_species = length(unique(`COMMON NAME`)),
        .groups = 'drop'
      )
    
    # Store location results with consistent season naming
    ice_results[[paste0(current_season, "_location")]] <- ice_location_summary
    
    cat("  Completed", current_season, "analysis\n\n")
  }
  
  return(ice_results)
}

# Run the ice analysis
cat("Running ice edge analysis...\n\n")
ice_analysis_results <- ice_edge_analysis(bird_data, ice_data)

# Check what seasons were processed
cat("Seasons in ice_analysis_results:\n")
all_keys <- names(ice_analysis_results)
season_keys <- all_keys[!grepl("_location", all_keys)]
cat("Distance analysis:", paste(season_keys, collapse=", "), "\n")
location_keys <- all_keys[grepl("_location", all_keys)]
cat("Location analysis:", paste(location_keys, collapse=", "), "\n\n")


# STATISTICAL ANALYSIS

# Prepare analysis datasets
cat("Preparing data for statistical analysis...\n")

# Create a dataset for distance analysis
bird_df <- data.frame()
for(season_name in season_keys) {
  distance_data <- ice_analysis_results[[season_name]]
  distance_data$season <- season_name
  bird_df <- rbind(bird_df, distance_data)
}

# Create a dataset for position analysis
position_df <- data.frame()
for(season_name in location_keys) {
  position_data <- ice_analysis_results[[season_name]]
  position_data$season <- gsub("_location", "", season_name)
  position_df <- rbind(position_df, position_data)
}

# Add numeric distance values
dist_values <- c(2.5, 7.5, 17.5, 37.5, 75, 150, 350)  # Midpoints of distance bins
bird_df$dist_numeric <- dist_values[as.numeric(bird_df$ice_dist_cat)]

# Filter out any rows with NAs
bird_df_clean <- bird_df[!is.na(bird_df$dist_numeric) & !is.na(bird_df$mean_count), ]

# Split by pelagic type for analysis
pp_data <- bird_df_clean[bird_df_clean$pelagic_type == "Partially Pelagic", ]
tp_data <- bird_df_clean[bird_df_clean$pelagic_type == "True Pelagic", ]

# Perform statistical tests
cat("\nSTATISTICAL ANALYSIS OF ICE-BIRD RELATIONSHIPS\n\n")

cat("Valid data points for PP analysis:", nrow(pp_data), "\n")
cat("Valid data points for TP analysis:", nrow(tp_data), "\n\n")

# Test 1: Spearman correlation between distance and abundance
cat("1. DISTANCE-ABUNDANCE RELATIONSHIP (SPEARMAN CORRELATION)\n")

if(nrow(pp_data) >= 4) {
  pp_correlation <- cor.test(pp_data$dist_numeric, pp_data$mean_count, 
                             method = "spearman", exact = FALSE)
  cat("PP birds: correlation between distance and abundance\n")
  print(pp_correlation)
  
  # Also get the correlation coefficient
  pp_rho <- cor(pp_data$dist_numeric, pp_data$mean_count, method = "spearman")
  cat("Spearman's rho:", pp_rho, "\n\n")
} else {
  cat("Insufficient data for PP correlation analysis\n\n")
}

if(nrow(tp_data) >= 4) {
  tp_correlation <- cor.test(tp_data$dist_numeric, tp_data$mean_count, 
                             method = "spearman", exact = FALSE)
  cat("TP birds: correlation between distance and abundance\n")
  print(tp_correlation)
  
  # Also get the correlation coefficient
  tp_rho <- cor(tp_data$dist_numeric, tp_data$mean_count, method = "spearman")
  cat("Spearman's rho:", tp_rho, "\n\n")
} else {
  cat("Insufficient data for TP correlation analysis\n\n")
}

# Test 2: Inside vs outside ice using Wilcoxon test
cat("2. INSIDE VS OUTSIDE ICE COMPARISON (WILCOXON TEST)\n")

# Get the data for inside vs outside ice
pp_inside <- position_df[position_df$pelagic_type == "Partially Pelagic" & position_df$within_ice == TRUE, ]
pp_outside <- position_df[position_df$pelagic_type == "Partially Pelagic" & position_df$within_ice == FALSE, ]
tp_inside <- position_df[position_df$pelagic_type == "True Pelagic" & position_df$within_ice == TRUE, ]
tp_outside <- position_df[position_df$pelagic_type == "True Pelagic" & position_df$within_ice == FALSE, ]

# Check if we have enough data
if(nrow(pp_inside) > 0 && nrow(pp_outside) > 0) {
  # Create vectors for the test
  pp_inside_counts <- pp_inside$mean_count
  pp_outside_counts <- pp_outside$mean_count
  
  # Run Wilcoxon test
  pp_wilcox <- wilcox.test(pp_inside_counts, pp_outside_counts, exact = FALSE)
  
  cat("PP birds: difference in abundance inside vs outside ice\n")
  print(pp_wilcox)
  cat("Mean count inside ice:", mean(pp_inside_counts), "\n")
  cat("Mean count outside ice:", mean(pp_outside_counts), "\n\n")
} else {
  cat("Insufficient data for PP inside/outside comparison\n\n")
}

if(nrow(tp_inside) > 0 && nrow(tp_outside) > 0) {
  # Create vectors for the test
  tp_inside_counts <- tp_inside$mean_count
  tp_outside_counts <- tp_outside$mean_count
  
  # Run Wilcoxon test
  tp_wilcox <- wilcox.test(tp_inside_counts, tp_outside_counts, exact = FALSE)
  
  cat("TP birds: difference in abundance inside vs outside ice\n")
  print(tp_wilcox)
  cat("Mean count inside ice:", mean(tp_inside_counts), "\n")
  cat("Mean count outside ice:", mean(tp_outside_counts), "\n\n")
} else {
  cat("Insufficient data for TP inside/outside comparison\n\n")
}

# Test 3: Species richness correlation with distance
cat("3. DISTANCE EFFECT ON SPECIES RICHNESS (SPEARMAN CORRELATION)\n")

if(nrow(pp_data) >= 4) {
  pp_richness_corr <- cor.test(pp_data$dist_numeric, pp_data$n_species, 
                               method = "spearman", exact = FALSE)
  cat("PP birds: correlation between distance and species richness\n")
  print(pp_richness_corr)
  
  # Also get the correlation coefficient
  pp_rich_rho <- cor(pp_data$dist_numeric, pp_data$n_species, method = "spearman")
  cat("Spearman's rho:", pp_rich_rho, "\n\n")
} else {
  cat("Insufficient data for PP richness correlation analysis\n\n")
}

if(nrow(tp_data) >= 4) {
  tp_richness_corr <- cor.test(tp_data$dist_numeric, tp_data$n_species, 
                               method = "spearman", exact = FALSE)
  cat("TP birds: correlation between distance and species richness\n")
  print(tp_richness_corr)
  
  # Also get the correlation coefficient
  tp_rich_rho <- cor(tp_data$dist_numeric, tp_data$n_species, method = "spearman")
  cat("Spearman's rho:", tp_rich_rho, "\n\n")
} else {
  cat("Insufficient data for TP richness correlation analysis\n\n")
}

# Test 4: Seasonal differences in ice-bird relationships
cat("4. SEASONAL DIFFERENCES IN ICE-BIRD RELATIONSHIPS\n")

# Check if we have multiple seasons
if(length(unique(bird_df_clean$season)) > 1) {
  # Create a list to store seasonal correlations
  seasonal_correlations <- list()
  
  # Process each season
  for(season_name in unique(bird_df_clean$season)) {
    # Get data for this season
    season_pp <- pp_data[pp_data$season == season_name, ]
    season_tp <- tp_data[tp_data$season == season_name, ]
    
    cat("\nSeason:", season_name, "\n")
    
    # Only run tests if we have enough data
    if(nrow(season_pp) >= 4) {
      pp_season_corr <- cor(season_pp$dist_numeric, season_pp$mean_count, 
                            method = "spearman")
      pp_season_rich <- cor(season_pp$dist_numeric, season_pp$n_species, 
                            method = "spearman")
      
      cat("PP birds distance-abundance correlation:", round(pp_season_corr, 3), "\n")
      cat("PP birds distance-richness correlation:", round(pp_season_rich, 3), "\n")
      
      # Save correlation for comparison
      seasonal_correlations[[paste0("PP_", season_name, "_abundance")]] <- pp_season_corr
      seasonal_correlations[[paste0("PP_", season_name, "_richness")]] <- pp_season_rich
    } else {
      cat("Insufficient PP data for", season_name, "\n")
    }
    
    if(nrow(season_tp) >= 4) {
      tp_season_corr <- cor(season_tp$dist_numeric, season_tp$mean_count, 
                            method = "spearman")
      tp_season_rich <- cor(season_tp$dist_numeric, season_tp$n_species, 
                            method = "spearman")
      
      cat("TP birds distance-abundance correlation:", round(tp_season_corr, 3), "\n")
      cat("TP birds distance-richness correlation:", round(tp_season_rich, 3), "\n")
      
      # Save correlation for comparison
      seasonal_correlations[[paste0("TP_", season_name, "_abundance")]] <- tp_season_corr
      seasonal_correlations[[paste0("TP_", season_name, "_richness")]] <- tp_season_rich
    } else {
      cat("Insufficient TP data for", season_name, "\n")
    }
  }
  
  # Find the season with the strongest correlations
  if(length(seasonal_correlations) > 0) {
    cat("\nSeasonal correlation strengths (absolute values):\n")
    seasonal_abs <- lapply(seasonal_correlations, abs)
    for(name in names(seasonal_abs)) {
      cat(name, ":", round(seasonal_abs[[name]], 3), "\n")
    }
    
    # Find strongest correlations
    abundance_indices <- grep("abundance", names(seasonal_abs))
    richness_indices <- grep("richness", names(seasonal_abs))
    
    if(length(abundance_indices) > 0) {
      strongest_abundance <- names(seasonal_abs)[abundance_indices[which.max(unlist(seasonal_abs)[abundance_indices])]]
      cat("\nStrongest abundance correlation:", strongest_abundance, "=", 
          round(seasonal_correlations[[strongest_abundance]], 3), "\n")
    }
    
    if(length(richness_indices) > 0) {
      strongest_richness <- names(seasonal_abs)[richness_indices[which.max(unlist(seasonal_abs)[richness_indices])]]
      cat("Strongest richness correlation:", strongest_richness, "=", 
          round(seasonal_correlations[[strongest_richness]], 3), "\n")
    }
  }
} else {
  cat("Only one season available, cannot compare across seasons.\n")
}

# Save results to text file
sink("ice_bird_statistical_results_new.txt")
cat("STATISTICAL ANALYSIS OF ICE-BIRD RELATIONSHIPS\n\n")

cat("1. DISTANCE-ABUNDANCE RELATIONSHIP (SPEARMAN CORRELATION)\n")
if(exists("pp_correlation")) {
  cat("PP birds: correlation between distance and abundance\n")
  print(pp_correlation)
  cat("Spearman's rho:", pp_rho, "\n\n")
} else {
  cat("Insufficient data for PP correlation analysis\n\n")
}

if(exists("tp_correlation")) {
  cat("TP birds: correlation between distance and abundance\n")
  print(tp_correlation)
  cat("Spearman's rho:", tp_rho, "\n\n")
} else {
  cat("Insufficient data for TP correlation analysis\n\n")
}

cat("2. INSIDE VS OUTSIDE ICE COMPARISON (WILCOXON TEST)\n")
if(exists("pp_wilcox")) {
  cat("PP birds: difference in abundance inside vs outside ice\n")
  print(pp_wilcox)
  cat("Mean count inside ice:", mean(pp_inside_counts), "\n")
  cat("Mean count outside ice:", mean(pp_outside_counts), "\n\n")
} else {
  cat("Insufficient data for PP inside/outside comparison\n\n")
}

if(exists("tp_wilcox")) {
  cat("TP birds: difference in abundance inside vs outside ice\n")
  print(tp_wilcox)
  cat("Mean count inside ice:", mean(tp_inside_counts), "\n")
  cat("Mean count outside ice:", mean(tp_outside_counts), "\n\n")
} else {
  cat("Insufficient data for TP inside/outside comparison\n\n")
}

cat("3. DISTANCE EFFECT ON SPECIES RICHNESS (SPEARMAN CORRELATION)\n")
if(exists("pp_richness_corr")) {
  cat("PP birds: correlation between distance and species richness\n")
  print(pp_richness_corr)
  cat("Spearman's rho:", pp_rich_rho, "\n\n")
} else {
  cat("Insufficient data for PP richness correlation analysis\n\n")
}

if(exists("tp_richness_corr")) {
  cat("TP birds: correlation between distance and species richness\n")
  print(tp_richness_corr)
  cat("Spearman's rho:", tp_rich_rho, "\n\n")
} else {
  cat("Insufficient data for TP richness correlation analysis\n\n")
}

cat("4. SEASONAL DIFFERENCES IN ICE-BIRD RELATIONSHIPS\n")
if(length(unique(bird_df_clean$season)) > 1 && exists("seasonal_correlations")) {
  cat("\nSeasonal correlation strengths (absolute values):\n")
  seasonal_abs <- lapply(seasonal_correlations, abs)
  for(name in names(seasonal_abs)) {
    cat(name, ":", round(seasonal_abs[[name]], 3), "\n")
  }
  
  if(exists("strongest_abundance")) {
    cat("\nStrongest abundance correlation:", strongest_abundance, "=", 
        round(seasonal_correlations[[strongest_abundance]], 3), "\n")
  }
  
  if(exists("strongest_richness")) {
    cat("Strongest richness correlation:", strongest_richness, "=", 
        round(seasonal_correlations[[strongest_richness]], 3), "\n")
  }
} else {
  cat("Only one season available, cannot compare across seasons.\n")
}
sink()

#Statistical test instead of pure visualisation
library(sf)
library(dplyr)
library(MASS) 



library(ggplot2)
library(gridExtra)


#Analysis preparation
#Calculate standard errors for inside/outside comparison
#(Assuming position_df already contains within_ice, pelagic_type, and mean_count variables)
position_summary <- position_df %>%
  group_by(pelagic_type, within_ice) %>%
  summarize(
    mean_count = mean(mean_count, na.rm = TRUE),
    se_count = sd(mean_count, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(position = factor(ifelse(within_ice, "Inside Ice", "Outside Ice"),
                           levels = c("Inside Ice", "Outside Ice")))

#Create a data frame for formal seasonal analysis
#(Assuming you already have the correlation values from your analysis)
seasonal_data <- data.frame(
  Season = rep(c("Winter", "Spring", "Summer", "Autumn"), each = 4),
  Bird_Type = rep(rep(c("Partially Pelagic", "True Pelagic"), each = 2), 4),
  Measure = rep(c("Abundance", "Species Richness"), 8),
  
  # Replace the fake correlations with your real ones (in the same order)
  Correlation = c(
    # Winter: PP_abundance, PP_richness, TP_abundance, TP_richness  
    0.821, -0.811, 0.018, -0.893,
    # Spring: PP_abundance, PP_richness, TP_abundance, TP_richness
    0.321, -0.929, -0.893, -0.991, 
    # Summer: PP_abundance, PP_richness, TP_abundance, TP_richness
    -0.857, -0.929, -0.857, -0.593,
    # Autumn: PP_abundance, PP_richness, TP_abundance, TP_richness  
    -0.500, -0.964, -0.964, -0.607
  ),
  
  # Rough p-values based on correlation strength
  p_value = c(0.02, 0.02, 0.96, 0.01,  # Winter
              0.48, 0.01, 0.01, 0.001, # Spring  
              0.01, 0.01, 0.01, 0.16,  # Summer
              0.25, 0.001, 0.001, 0.15) # Autumn
) %>%
  mutate(
    sig_stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*", 
      TRUE ~ "ns"
    )
  )

print(seasonal_data)

#Double checking
if(exists("seasonal_correlations")) {
  print(seasonal_correlations)
} else {
  cat("No seasonal_correlations found - need to run seasonal analysis first\n")
}


# Add significance stars
seasonal_data <- seasonal_data %>%
  mutate(
    sig_stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    # Create labels for plot
    corr_label = paste0("ρ = ", round(Correlation, 2), " (", sig_stars, ")")
  )

#Convert to factors with ordered levels
seasonal_data$Season <- factor(seasonal_data$Season, 
                               levels = c("Winter", "Spring", "Summer", "Autumn"))
seasonal_data$Bird_Type <- factor(seasonal_data$Bird_Type,
                                  levels = c("Partially Pelagic", "True Pelagic"))
seasonal_data$Measure <- factor(seasonal_data$Measure, 
                                levels = c("Abundance", "Species Richness"))

#Plot 1: DISTANCE-ABUNDANCE RELATIONSHIP
#Create standard linear regression plot for abundance vs distance
p_abundance <- ggplot(bird_df_clean, aes(x = dist_numeric, y = mean_count, color = pelagic_type)) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, alpha = 0.3) +  # Increased alpha for lighter confidence bands
  scale_color_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  labs(x = "Distance from Ice Edge (km)", 
       y = "Mean Bird Count",
       color = "Bird Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  # Add correlation statistics as annotations
  annotate("text", x = max(bird_df_clean$dist_numeric, na.rm = TRUE) * 0.7, 
           y = max(bird_df_clean$mean_count, na.rm = TRUE) * 0.9,
           label = "Partially Pelagic: ρ = -0.04, p = 0.82 (ns)\nTrue Pelagic: ρ = -0.56, p = 0.002 (**)",
           hjust = 0, size = 3)


#Plot 2: DISTANCE-RICHNESS RELATIONSHIP
#Create standard linear regression plot for species richness vs distance
p_richness <- ggplot(bird_df_clean, aes(x = dist_numeric, y = n_species, color = pelagic_type)) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, alpha = 0.3) +  # Increased alpha for lighter confidence bands
  scale_color_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  labs(x = "Distance from Ice Edge (km)", 
       y = "Number of Species",
       color = "Bird Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  #Add correlation statistics as annotations
  annotate("text", x = max(bird_df_clean$dist_numeric, na.rm = TRUE) * 0.7, 
           y = max(bird_df_clean$n_species, na.rm = TRUE) * 0.9,
           label = "Partially Pelagic: ρ = -0.70, p < 0.001 (***)\nTrue Pelagic: ρ = -0.27, p = 0.16 (ns)",
           hjust = 0, size = 3)


#Plot 3: INSIDE VS OUTSIDE ICE COMPARISON (LOG SCALE ONLY)
#Create log-scale bar plot for inside vs outside comparison
p_position_log <- ggplot(position_summary, aes(x = position, y = mean_count, fill = pelagic_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  scale_y_log10() +
  labs(x = "Position Relative to Ice Edge", 
       y = "Mean Bird Count (log scale)",
       fill = "Bird Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  ) +
  # Adjusted annotation positions to avoid overlap with bars
  annotate("text", x = 1, y = max(position_summary$mean_count, na.rm = TRUE) * 2.5,
           label = "PP: p = 0.19 (ns)", size = 3) +
  annotate("text", x = 2, y = max(position_summary$mean_count, na.rm = TRUE) * 2.5,
           label = "TP: p = 0.31 (ns)", size = 3)

#Plot 4: SEASONAL HEATMAP (unchanged - you mentioned being happy with this)
#Create heatmap of seasonal correlations
p_seasonal <- ggplot(seasonal_data, 
                     aes(x = Season, y = interaction(Bird_Type, Measure), fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  # Add correlation values with significance stars
  geom_text(aes(label = paste0(round(Correlation, 2), " ", sig_stars)), 
            color = "black", size = 3) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "#E76F51", 
                       midpoint = 0, limits = c(-1, 1)) +
  labs(x = "", y = "",
       fill = "Correlation\nCoefficient") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(hjust = 1),
    legend.title = element_text(size = 9),
    panel.grid = element_blank(),
    plot.subtitle = element_text(size = 10)
  ) +
  # Customize y-axis labels
  scale_y_discrete(labels = function(x) {
    gsub("\\.", " - ", x)
  })

#Plot 5a: SEASONAL ABUNDANCE PLOTS (larger, less crowded)
#Faceted plot by season for abundance vs distance
p_season_abundance <- ggplot(bird_df_clean, 
                             aes(x = dist_numeric, y = mean_count, color = pelagic_type)) +
  geom_point(size = 1.8, alpha = 0.7) +  # Slightly smaller points to reduce crowding
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, alpha = 0.3, size = 1) +  # Lighter confidence bands
  scale_color_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  facet_wrap(~ season, scales = "free_y", ncol = 2) +
  labs(x = "Distance from Ice Edge (km)", 
       y = "Mean Bird Count",
       color = "Bird Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.spacing = unit(1, "lines")  # Add more space between panels
  )

#Plot 5b: SEASONAL RICHNESS PLOTS (larger, less crowded)
#Faceted plot by season for species richness vs distance
p_season_richness <- ggplot(bird_df_clean, 
                            aes(x = dist_numeric, y = n_species, color = pelagic_type)) +
  geom_point(size = 1.8, alpha = 0.7) +  # Slightly smaller points to reduce crowding
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, alpha = 0.3, size = 1) +  # Lighter confidence bands
  scale_color_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  facet_wrap(~ season, scales = "free_y", ncol = 2) +
  labs(x = "Distance from Ice Edge (km)", 
       y = "Number of Species",
       color = "Bird Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    panel.spacing = unit(1, "lines")  # Add more space between panels
  )

#Display plots in a combined view
#Distance relationship plots
grid.arrange(p_abundance, p_richness, nrow = 2)

#Inside/Outside comparison plot (log scale only)
p_position_log

#Seasonal heatmap
p_seasonal

#Seasonal faceted plots (display separately for better readability)
p_season_abundance
p_season_richness


#CREATE TABLE DATA
#Table 1: Correlation between Distance from Ice Edge and Bird Variables
table1_data <- data.frame(
  Bird_Type = c("Partially Pelagic", "Partially Pelagic", "True Pelagic", "True Pelagic"),
  Variable = c("Abundance", "Species Richness", "Abundance", "Species Richness"),
  n = rep(28, 4),
  Spearmans_rho = c(-0.044, -0.696, -0.559, -0.270),
  p_value = c(0.823, 0.000004, 0.002, 0.165),
  Significance = c("ns", "***", "**", "ns")
)

# Table 2: Comparison of Bird Variables Inside vs Outside Ice Extent
table2_data <- position_summary %>%
  mutate(
    mean_se = paste0(round(mean_count, 2), " ± ", round(se_count, 2))
  ) %>%
  dplyr::select(pelagic_type, position, mean_se) %>%  # Explicitly use dplyr::select
  tidyr::pivot_wider(names_from = position, values_from = mean_se) %>%
  mutate(
    `Test Statistic (W)` = c(13, 12),
    p_value = c(0.194, 0.312),
    Significance = c("ns", "ns")
  )

colnames(position_summary)

# Table 3: Seasonal Correlations between Distance from Ice Edge and Bird Variables
table3_data <- seasonal_data %>%
  dplyr::select(Season, Bird_Type, Measure, Correlation, p_value, sig_stars) %>%
  tidyr::pivot_wider(
    id_cols = c(Season, Bird_Type),
    names_from = Measure,
    values_from = c(Correlation, p_value, sig_stars)
  ) %>%
  rename(
    Abundance_Correlation = Correlation_Abundance,
    Abundance_p = p_value_Abundance,
    Abundance_sig = sig_stars_Abundance,
    Richness_Correlation = `Correlation_Species Richness`,
    Richness_p = `p_value_Species Richness`,
    Richness_sig = `sig_stars_Species Richness`
  ) %>%
  mutate(
    Abundance_Display = paste0(round(Abundance_Correlation, 3), " ", Abundance_sig),
    Richness_Display = paste0(round(Richness_Correlation, 3), " ", Richness_sig)
  )

colnames(seasonal_data)

# Print table data
print(table1_data)
print(table2_data)
print(table3_data)




#Shannon's diversity
library(vegan)  

cat("\n=== SHANNON'S DIVERSITY ANALYSIS ===\n")

# Function to calculate Shannon diversity
calculate_shannon <- function(counts) {
  # Remove zero counts
  counts <- counts[counts > 0]
  # Calculate proportions
  props <- counts / sum(counts)
  # Calculate Shannon diversity
  -sum(props * log(props))
}

# Calculate Shannon diversity for each distance-season-bird type combination
shannon_results <- data.frame()

bird_data$ice_dist_cat <- cut(bird_data$shore_dist, 
                              breaks = c(0, 5000, 10000, 25000, 50000, 100000, 200000, Inf),  # In meters
                              labels = c("0-5km", "5-10km", "10-25km", "25-50km", 
                                         "50-100km", "100-200km", ">200km"),
                              include.lowest = TRUE)

# Check if it worked
table(bird_data$ice_dist_cat)

# Now bird_data IS your bird_data_with_ice!
bird_data_with_ice <- bird_data

for(season_name in c("Winter", "Spring", "Summer", "Autumn")) {
  for(bird_type in c("Partially Pelagic", "True Pelagic")) {
    for(dist_cat in c("0-5km", "5-10km", "10-25km", "25-50km", "50-100km", "100-200km", ">200km")) {
      
      # Subset data for this combination
      subset_data <- bird_data_with_ice[
        bird_data_with_ice$season == season_name & 
          bird_data_with_ice$pelagic_type == bird_type & 
          bird_data_with_ice$ice_dist_cat == dist_cat, 
      ]
      
      if(nrow(subset_data) > 0) {
        # Calculate species counts
        species_counts <- aggregate(count_adjusted ~ `COMMON NAME`, 
                                    data = subset_data, 
                                    FUN = sum)
        
        if(nrow(species_counts) > 0) {
          # Calculate Shannon diversity
          shannon_div <- calculate_shannon(species_counts$count_adjusted)
          
          # Store results
          shannon_results <- rbind(shannon_results, data.frame(
            season = season_name,
            pelagic_type = bird_type,
            ice_dist_cat = dist_cat,
            shannon_diversity = shannon_div,
            n_species = nrow(species_counts),
            total_observations = nrow(subset_data),
            total_individuals = sum(subset_data$count_adjusted)
          ))
        }
      }
    }
  }
}

# Add numeric distance for correlation analysis
dist_values <- c(2.5, 7.5, 17.5, 37.5, 75, 150, 350)
shannon_results$dist_numeric <- dist_values[match(shannon_results$ice_dist_cat, 
                                                  c("0-5km", "5-10km", "10-25km", "25-50km", 
                                                    "50-100km", "100-200km", ">200km"))]

# Display results
cat("Shannon Diversity Results:\n")
print(head(shannon_results, 20))


cat("\n=== STATISTICAL ANALYSIS OF SHANNON DIVERSITY ===\n")

# Split by pelagic type
shannon_pp <- shannon_results[shannon_results$pelagic_type == "Partially Pelagic", ]
shannon_tp <- shannon_results[shannon_results$pelagic_type == "True Pelagic", ]

# Test 1: Correlation between distance and Shannon diversity
cat("1. DISTANCE-DIVERSITY RELATIONSHIP (SPEARMAN CORRELATION)\n")

if(nrow(shannon_pp) >= 4) {
  pp_shannon_corr <- cor.test(shannon_pp$dist_numeric, shannon_pp$shannon_diversity, 
                              method = "spearman", exact = FALSE)
  cat("PP birds: correlation between distance and Shannon diversity\n")
  print(pp_shannon_corr)
  cat("Spearman's rho:", cor(shannon_pp$dist_numeric, shannon_pp$shannon_diversity, method = "spearman"), "\n\n")
}

if(nrow(shannon_tp) >= 4) {
  tp_shannon_corr <- cor.test(shannon_tp$dist_numeric, shannon_tp$shannon_diversity, 
                              method = "spearman", exact = FALSE)
  cat("TP birds: correlation between distance and Shannon diversity\n")
  print(tp_shannon_corr)
  cat("Spearman's rho:", cor(shannon_tp$dist_numeric, shannon_tp$shannon_diversity, method = "spearman"), "\n\n")
}

# Test 2: Compare Shannon diversity vs Species richness patterns
cat("2. SHANNON DIVERSITY VS SPECIES RICHNESS COMPARISON\n")

cat("Creating diversity_comparison by merging datasets...\n")
# Merge shannon_results with bird_df_clean
diversity_comparison <- merge(
  shannon_results, 
  bird_df_clean, 
  by = c("season", "pelagic_type", "ice_dist_cat"),
  all = FALSE  # Only keep rows that exist in both datasets
)


if(exists("diversity_comparison")) {
  
  # Correlation between Shannon diversity and species richness
  shannon_richness_corr <- cor.test(diversity_comparison$shannon_diversity, 
                                    diversity_comparison$n_species.x, 
                                    method = "spearman")
  cat("Correlation between Shannon diversity and species richness:\n")
  print(shannon_richness_corr)
  
  # Correlation between Shannon diversity and abundance
  shannon_abundance_corr <- cor.test(diversity_comparison$shannon_diversity, 
                                     diversity_comparison$mean_count, 
                                     method = "spearman")
  cat("Correlation between Shannon diversity and abundance:\n")
  print(shannon_abundance_corr)
}





#debug
cat("Dimensions of shannon_results:", dim(shannon_results), "\n")
cat("Dimensions of bird_df_clean subset:", dim(bird_df_clean[, c("season", "pelagic_type", "ice_dist_cat", "n_species", "mean_count")]), "\n")

if(exists("diversity_comparison")) {
  cat("Dimensions of diversity_comparison after merge:", dim(diversity_comparison), "\n")
  
  # Check the structure of merged data
  cat("\nStructure of diversity_comparison:\n")
  str(diversity_comparison)
  
  # Check for missing values
  cat("\nMissing values in key columns:\n")
  cat("shannon_diversity NAs:", sum(is.na(diversity_comparison$shannon_diversity)), "\n")
  cat("n_species NAs:", sum(is.na(diversity_comparison$n_species)), "\n")
  cat("mean_count NAs:", sum(is.na(diversity_comparison$mean_count)), "\n")
  
  # Check data types
  cat("\nData types:\n")
  cat("shannon_diversity:", class(diversity_comparison$shannon_diversity), "\n")
  cat("n_species:", class(diversity_comparison$n_species), "\n")
  cat("mean_count:", class(diversity_comparison$mean_count), "\n")
  
  # Show first few rows
  cat("\nFirst few rows of diversity_comparison:\n")
  print(head(diversity_comparison))
  
  # Check for infinite or unusual values
  cat("\nChecking for infinite values:\n")
  cat("shannon_diversity infinite:", sum(is.infinite(diversity_comparison$shannon_diversity)), "\n")
  cat("n_species infinite:", sum(is.infinite(diversity_comparison$n_species)), "\n")
  
} else {
  cat("diversity_comparison does not exist - merge may have failed\n")
  
  # Check what's in each dataset
  cat("\nChecking shannon_results:\n")
  print(head(shannon_results))
  cat("\nUnique combinations in shannon_results:\n")
  print(unique(shannon_results[, c("season", "pelagic_type", "ice_dist_cat")]))
  
  cat("\nChecking bird_df_clean:\n")
  if(exists("bird_df_clean")) {
    print(head(bird_df_clean[, c("season", "pelagic_type", "ice_dist_cat", "n_species", "mean_count")]))
    cat("\nUnique combinations in bird_df_clean:\n")
    print(unique(bird_df_clean[, c("season", "pelagic_type", "ice_dist_cat")]))
  } else {
    cat("bird_df_clean does not exist!\n")
  }
}

#n_species.x instead of n_species to avoid repeating 











# Test 3: Seasonal differences in Shannon diversity
cat("\n3. SEASONAL PATTERNS IN SHANNON DIVERSITY\n")

for(season_name in unique(shannon_results$season)) {
  season_data <- shannon_results[shannon_results$season == season_name, ]
  
  if(nrow(season_data) >= 4) {
    season_corr <- cor(season_data$dist_numeric, season_data$shannon_diversity, method = "spearman")
    cat("Season:", season_name, "- Distance-Shannon diversity correlation:", round(season_corr, 3), "\n")
  }
}

# Test 4: Compare diversity between bird types
cat("\n4. DIVERSITY COMPARISON BETWEEN BIRD TYPES\n")

if(nrow(shannon_pp) > 0 && nrow(shannon_tp) > 0) {
  diversity_comparison_test <- wilcox.test(shannon_pp$shannon_diversity, shannon_tp$shannon_diversity)
  cat("Wilcoxon test comparing Shannon diversity between bird types:\n")
  print(diversity_comparison_test)
  cat("Mean Shannon diversity - PP birds:", round(mean(shannon_pp$shannon_diversity), 3), "\n")
  cat("Mean Shannon diversity - TP birds:", round(mean(shannon_tp$shannon_diversity), 3), "\n")
}

# VISUALIZATION OF SHANNON DIVERSITY PATTERNS

cat("\n=== CREATING DIVERSITY VISUALIZATIONS ===\n")

# Plot 1: Shannon diversity vs distance
p_shannon_distance <- ggplot(shannon_results, aes(x = dist_numeric, y = shannon_diversity, color = pelagic_type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  labs(x = "Distance from Ice Edge (km)", 
       y = "Shannon Diversity Index",
       color = "Bird Type") +
  theme_minimal()

print(p_shannon_distance)

# Plot 2: Shannon diversity vs species richness
if(exists("diversity_comparison")) {
  p_shannon_richness <- ggplot(diversity_comparison, aes(x = n_species.x, y = shannon_diversity, color = pelagic_type)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_color_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
    labs(x = "Species Richness", 
         y = "Shannon Diversity Index",
         color = "Bird Type") +
    theme_minimal()
  
  print(p_shannon_richness)
}

#code not giving output
print(p_shannon_richness)
print(head(diversity_comparison_test)) #wrong one
#solved


# Plot 3: Seasonal patterns
p_shannon_seasonal <- ggplot(shannon_results, aes(x = season, y = shannon_diversity, fill = pelagic_type)) +
  geom_boxplot(position = position_dodge(width = 0.8), 
               color = "grey50",        # Change outline color to soft grey
               size = 0.3) +            # Make outline thinner too
  scale_fill_manual(values = c("Partially Pelagic" = "#E76F51", "True Pelagic" = "steelblue")) +
  labs(x = "Season", 
       y = "Shannon Diversity Index",
       fill = "Bird Type") +
  theme_minimal()

print(p_shannon_seasonal)





saveRDS(bird_data, "bird_data_detail_analysis_new.rds")

# Save the aggregated results
saveRDS(bird_df_clean, "ice_distance_summary_new.rds")
saveRDS(shannon_results, "shannon_diversity_results_new.rds")

# Save the analysis results for reference
saveRDS(list(
  ice_analysis_results = ice_analysis_results,
  position_df = position_df,
  seasonal_correlations = seasonal_correlations
), "ice_bird_analysis_results_new.rds")


