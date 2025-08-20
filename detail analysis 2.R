rm(list = ls())

# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

# Set up spatial processing
sf_use_s2(FALSE)

cat("=== ARCTIC BIRD HOTSPOT ANALYSIS ===\n")

# Load processed data from previous analysis
bird_data <- readRDS("bird_data_effort_standardized.rds")
ice_distance_summary <- readRDS("ice_distance_summary_new.rds")
shannon_results <- readRDS("shannon_diversity_results_new.rds")

# Quick data check
cat("Data loaded successfully:\n")
cat("- Bird observations:", nrow(bird_data), "\n")
cat("- Ice distance summary:", nrow(ice_distance_summary), "\n")
cat("- Shannon diversity results:", nrow(shannon_results), "\n\n")

arctic_crs <- st_crs(bird_data)

#Step 1: Check bird_data CRS
st_crs(bird_data)

#Step 2: Create grid more carefully
# Overview grid (50km for main map)
grid_size_overview <- 50000
overview_grid <- st_make_grid(
  bird_data, 
  cellsize = grid_size_overview, 
  square = FALSE,
  what = "polygons"
) %>%
  st_sf() %>%
  mutate(grid_id = row_number())

# Detail grid (25km for zoom insets)  
grid_size_detail <- 25000
detail_grid <- st_make_grid(
  bird_data, 
  cellsize = grid_size_detail, 
  square = FALSE,
  what = "polygons"
) %>%
  st_sf() %>%
  mutate(grid_id = row_number())

# Function to process grid analysis
process_grid_analysis <- function(grid_data, grid_name) {
  cat("\n=== Processing", grid_name, "===\n")
  
  #Step 3: Break into chunks to save memory 
  chunk_size <- 10000  # Process 10k birds at a time
  n_birds <- nrow(bird_data)
  n_chunks <- ceiling(n_birds / chunk_size)
  
  cat("Processing", n_birds, "birds in", n_chunks, "chunks...\n")
  
  # Initialize grid_id column
  bird_data$grid_id <- NA
  
  # Process in chunks
  for(i in 1:n_chunks) {
    start_idx <- (i-1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n_birds)
    
    cat("Processing chunk", i, "of", n_chunks, "(birds", start_idx, "to", end_idx, ")\n")
    
    # Get chunk of birds
    bird_chunk <- bird_data[start_idx:end_idx, ]
    
    # Find intersections for this chunk only
    intersections <- st_intersects(bird_chunk, grid_data)
    
    # Assign grid_id (take first match for each bird)
    chunk_grid_ids <- sapply(intersections, function(x) if(length(x) > 0) x[1] else NA)
    
    # Update the main dataset
    bird_data$grid_id[start_idx:end_idx] <- chunk_grid_ids
    
    # Clean up
    rm(bird_chunk, intersections, chunk_grid_ids)
    gc()  # Force garbage collection
  }
  
  # Create final dataset
  bird_with_grid <- bird_data %>% filter(!is.na(grid_id))
  
  cat("Final result:\n")
  cat("Birds assigned to grid:", nrow(bird_with_grid), "\n")
  cat("Birds outside grid:", sum(is.na(bird_data$grid_id)), "\n")
  
  # Verify no duplicates
  duplicates <- bird_with_grid %>% 
    group_by_at(vars(-grid_id)) %>% 
    filter(n() > 1) %>% 
    nrow()
  
  cat("Duplicate observations:", duplicates, "\n")
  
  #Calculate basic metrics
  hotspot_metrics <- bird_with_grid %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(
      total_abundance = sum(abundance_standardized, na.rm = TRUE),
      n_species = n_distinct(`COMMON NAME`),
      total_observations = n(),
      
      # Effort metrics
      n_checklists = n_distinct(paste(`OBSERVATION DATE`, LOCALITY, sep = "_")),
      
      species_per_checklist = n_species / n_checklists,
      
      shannon_diversity = {
        if(n_distinct(`COMMON NAME`) > 1) {
          species_abundances <- tapply(abundance_standardized, `COMMON NAME`, sum, na.rm = TRUE)
          proportions <- species_abundances / sum(species_abundances)
          -sum(proportions * log(proportions))
        } else {
          0
        }
      },
      
      n_seasons = n_distinct(season),
      years_sampled = n_distinct(year),
      .groups = 'drop'
    )
  
  #Join back to grid
  final_grid <- grid_data %>%
    left_join(hotspot_metrics, by = "grid_id") %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))
  
  cat("Calculated metrics for", sum(final_grid$total_observations > 0), "cells with data\n")
  
  #Step 4: Define hotspot
  #Only use cells with data for thresholds
  grid_with_data <- final_grid %>% filter(total_observations > 0)
  
  #Calculate 75th percentile thresholds
  abundance_75 <- quantile(grid_with_data$total_abundance, 0.75)
  richness_75 <- quantile(grid_with_data$n_species, 0.75)
  shannon_75 <- quantile(grid_with_data$shannon_diversity, 0.75)
  
  #Add standardized hotspot definitions
  final_grid <- final_grid %>%
    mutate(
      # Original hotspots
      abundance_hotspot = total_abundance >= abundance_75,
      richness_hotspot = n_species >= richness_75,
      shannon_hotspot = shannon_diversity >= shannon_75,
      
      # Combined hotspots with effort correction
      effort_corrected_hotspot = abundance_hotspot & richness_hotspot,
      
      # Updated categories
      hotspot_type = case_when(
        total_observations == 0 ~ "No Data",
        abundance_hotspot & richness_hotspot ~ "Comprehensive",  # Both criteria
        abundance_hotspot ~ "High Abundance",                    # Just abundance
        richness_hotspot ~ "High Richness",                            # Just richness  
        TRUE ~ "Regular"
      )
    )
  
  cat("Thresholds - Abundance:", round(abundance_75, 2), "Richness:", round(richness_75), "Shannon:", round(shannon_75, 2), "\n")
  
  #Report sample sizes
  cat("Total bird observations processed:", nrow(bird_with_grid), "\n")
  cat("Grid cells with data:", nrow(grid_with_data), "of", nrow(final_grid), "total\n")
  
  return(final_grid)
}

# Run analysis for both grids
detail_hotspot_grid <- process_grid_analysis(detail_grid, "Detail Grid (25km)")
overview_hotspot_grid <- process_grid_analysis(overview_grid, "Overview Grid (50km)")

# Save results
saveRDS(detail_hotspot_grid, "detail_hotspot_grid_25km.rds")
saveRDS(overview_hotspot_grid, "overview_hotspot_grid_50km.rds")


# Define zoom in region
top_bbox <- c(xmin = -2500000, xmax = 2000000, ymin = 1500000, ymax = 4500000)  # Keep at TOP of Arctic boundary
bottom_bbox <- c(xmin = -3500000, xmax = 2000000, ymin = -3200000, ymax = -550000)  # Much BIGGER southern panel


#Step 5: maps
#Colours for hotspot types
hotspot_colors_heat <- c(
  "No Data" = "white",           
  "Regular" = "#F7F7F7",           
  "High Abundance" = "steelblue",    
  "High Richness" = "#E9C46A",     
  "Comprehensive" = "#E76F51"      
)

"#264653"
"steelblue"
"#E9C46A"
"#F4A261"
"#E76F51"
"#E63946"

# 1. Hotspot overview map
# Load world data
library(rnaturalearth)
library(rnaturalearthdata)

amap_boundary <- st_read("AMAP-area/amaplim_lam_poly.shp", quiet = TRUE) %>%
  st_transform(crs = st_crs(overview_hotspot_grid))

# Get world land data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = st_crs(overview_hotspot_grid))

# Get proper zoom based on AMAP boundary
bbox <- st_bbox(amap_boundary)
xlim <- c(bbox["xmin"] - 100000, bbox["xmax"] + 100000)
ylim <- c(bbox["ymin"] - 100000, bbox["ymax"] + 100000)

# Enhanced hotspot map with Arctic context
library(patchwork)

# Main overview map (50km grid)
p_overview <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = amap_boundary, fill = NA, color = "red", size = 0.5) +
  geom_sf(data = overview_hotspot_grid %>% filter(hotspot_type != "No Data"), 
          aes(fill = hotspot_type), color = "white", size = 0.1) +
  scale_fill_manual(values = hotspot_colors_heat, name = "Hotspot Type") +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_void() +
  theme(
    legend.position = "none") +
  ggtitle("(a)")

# Top zoom inset (25km grid) 
p_top <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = detail_hotspot_grid %>% filter(hotspot_type != "No Data"), 
          aes(fill = hotspot_type), color = "white", size = 0.05) +
  scale_fill_manual(values = hotspot_colors_heat, guide = "none") +
  coord_sf(xlim = c(top_bbox["xmin"], top_bbox["xmax"]), 
           ylim = c(top_bbox["ymin"], top_bbox["ymax"])) +
  theme_void() +
  ggtitle("(b)")

# Bottom zoom inset (25km grid)
p_bottom <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = detail_hotspot_grid %>% filter(hotspot_type != "No Data"), 
          aes(fill = hotspot_type), color = "white", size = 0.05) +
  scale_fill_manual(values = hotspot_colors_heat, guide = "none") +
  coord_sf(xlim = c(bottom_bbox["xmin"], bottom_bbox["xmax"]), 
           ylim = c(bottom_bbox["ymin"], bottom_bbox["ymax"])) +
  theme_void() +
  ggtitle("(c)")

# Create a separate legend plot
legend_p <- ggplot() +
  geom_point(aes(x = 1:4, y = 1, fill = names(hotspot_colors_heat)[-1]), 
             shape = 22, size = 3) +
  scale_fill_manual(values = hotspot_colors_heat[-1], name = "Hotspot Type") +
  theme_void() +
  theme(legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(override.aes = list(shape = 22, size = 3)))

library(ggpubr)

# Combine with proper layout
combined_plot <- (p_overview | (p_top / p_bottom)) / 
  wrap_elements(get_legend(legend_p)) +
  plot_layout(heights = c(10, 1))  # Main plots get 10 units, legend gets 1

print(combined_plot)

# Also create individual plots
print(p_overview)




# Observer distribution
# Quick spatial join to get grid_id for each bird
bird_grid_join <- st_intersects(bird_data, overview_hotspot_grid)
bird_data$grid_id <- sapply(bird_grid_join, function(x) if(length(x) > 0) x[1] else NA)

# Create observer metrics
observer_metrics <- bird_data %>%
  filter(!is.na(grid_id)) %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarise(
    n_observers = n_distinct(`OBSERVER ID`),
    n_checklists = n_distinct(paste(`OBSERVATION DATE`, LOCALITY, sep = "_")),
    total_effort_hours = sum(`DURATION MINUTES`, na.rm = TRUE) / 60,
    observer_density = n_observers / (grid_size_overview/1000)^2,
    checklist_density = n_checklists / (grid_size_overview/1000)^2,
    
    # Temporal spread
    n_years_active = n_distinct(year),
    n_seasons_active = n_distinct(season),
    temporal_consistency = n_years_active * n_seasons_active,
    .groups = 'drop'
  )

# Join to earlier grid
observer_grid <- overview_hotspot_grid %>%
  left_join(observer_metrics, by = "grid_id") %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


# Visualization
# 1. Simple observer intensity map
p_observer_intensity <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = amap_boundary, fill = NA, color = "red", size = 0.5) +
  geom_sf(data = observer_grid %>% filter(n_observers > 0), 
          aes(fill = n_observers), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Number of\nObservers", trans = "log10") +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_void()

print(p_observer_intensity)

# 2. Calculate overlap percentages
overlap_analysis <- overview_hotspot_grid %>%
  st_drop_geometry() %>%
  left_join(observer_metrics, by = "grid_id") %>%
  mutate(
    has_observers = n_observers > 0,
    high_observer_density = n_observers >= quantile(n_observers[n_observers > 0], 0.75, na.rm = TRUE),
    is_hotspot = hotspot_type %in% c("High Abundance", "High Richness", "Comprehensive")
  ) %>%
  summarise(
    # Overall coverage
    total_cells = n(),
    cells_with_birds = sum(total_observations > 0, na.rm = TRUE),
    cells_with_observers = sum(has_observers, na.rm = TRUE),
    
    # Hotspot coverage - FIXED
    hotspot_cells = sum(is_hotspot, na.rm = TRUE),
    regular_cells = sum(hotspot_type == "Regular", na.rm = TRUE),
    
    hotspots_with_observers = sum(is_hotspot & has_observers, na.rm = TRUE),
    hotspots_with_high_observers = sum(is_hotspot & high_observer_density, na.rm = TRUE),
    
    # Calculate percentages
    pct_hotspots_covered = round(100 * hotspots_with_observers / hotspot_cells, 1),
    pct_hotspots_well_covered = round(100 * hotspots_with_high_observers / hotspot_cells, 1),
    
    # Compare hotspot vs regular coverage
    regular_with_observers = sum(hotspot_type == "Regular" & has_observers, na.rm = TRUE),
    pct_regular_covered = round(100 * regular_with_observers / regular_cells, 1)
  )

print(overlap_analysis)

# Plot: Observer Intensity: Hotspots vs Regular Areas
bias_plot <- overview_hotspot_grid %>%
  st_drop_geometry() %>%
  left_join(observer_metrics, by = "grid_id") %>%
  filter(total_observations > 0) %>%
  mutate(
    area_type = ifelse(hotspot_type %in% c("High Abundance", "High Richness", "Comprehensive"), 
                       "Hotspots", "Regular Areas")
  ) %>%
  ggplot(aes(x = area_type, y = n_observers, fill = area_type)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(y = "Number of Observers (log scale)",
       x = "Area Type") +
  theme_minimal()

print(bias_plot)

# Summary table
bias_summary <- overview_hotspot_grid %>%
  st_drop_geometry() %>%
  left_join(observer_metrics, by = "grid_id") %>%
  filter(total_observations > 0) %>%
  mutate(area_type = ifelse(hotspot_type %in% c("High Abundance", "High Richness", "Comprehensive"), 
                            "Hotspots", "Regular Areas")) %>%
  group_by(area_type) %>%
  summarise(median_observers = median(n_observers),
            q25 = quantile(n_observers, 0.25),
            q75 = quantile(n_observers, 0.75))

print(bias_summary)



# Check the actual hotspot type distribution
hotspot_distribution <- overview_hotspot_grid %>%
  st_drop_geometry() %>%
  filter(total_observations > 0) %>%  # Only cells with data
  count(hotspot_type)

print(hotspot_distribution)














# Check what combinations exist with the new effort-standardized logic
hotspot_check <- hotspot_grid %>% 
  st_drop_geometry() %>%
  filter(total_observations > 0) %>%
  count(effort_abundance_hotspot, richness_hotspot, shannon_hotspot) %>%
  arrange(desc(n))

print(hotspot_check)
# Delete high diversity and multi metrics



# 2. Abundance map
p2_enhanced <- ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = amap_boundary, fill = NA, color = "red", size = 0.5) +
  geom_sf(data = hotspot_grid %>% filter(total_observations > 0), 
          aes(fill = total_abundance), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Standardized Abundance\n(birds/observer-hr)", 
                       trans = "log10",
                       breaks = c(0.1, 1, 10, 100, 1000),
                       labels = c("0.1", "1", "10", "100", "1000")) +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )

print(p2_enhanced)

summary(hotspot_grid$total_abundance)
range(hotspot_grid$total_abundance, na.rm = TRUE)
max(hotspot_grid$total_abundance, na.rm = TRUE)

#debugging
names(bird_data)
duplicate_check <- bird_data %>%
  st_drop_geometry() %>% 
  group_by(`OBSERVATION DATE`, `SCIENTIFIC NAME`, LOCALITY, `OBSERVATION COUNT`) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1) %>%
  arrange(desc(count))

print(head(duplicate_check))





# 3. Species richness map
p3_enhanced <- ggplot() +
  # Add land first (as background)
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.1) +
  # Add AMAP boundary
  geom_sf(data = amap_boundary, fill = NA, color = "red", size = 0.5) +
  # Add richness grid on top - EXCLUDE "No Data" polygons
  geom_sf(data = hotspot_grid %>% filter(total_observations > 0), 
          aes(fill = n_species), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "Number of\nSpecies", 
                       option = "plasma",
                       breaks = c(1, 10, 20, 30, 40, 50),
                       labels = c("1", "10", "20", "30", "40", "50+")) +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p3_enhanced)
