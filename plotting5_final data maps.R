rm(list = ls())

library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(lubridate)

# Disable S2 for polar regions
sf_use_s2(FALSE)


create_final <- function(bird_file = "bird_data_effort_standardized.rds", 
                             ice_file = "nsidc/processed_data/fast_ice_for_birds.rds",
                             amap_file = "AMAP-area/amaplim_lam_poly.shp") {
  
  # Load data
  bird_data <- readRDS(bird_file)
  ice_data <- readRDS(ice_file)
  
  # Fix column names if needed
  names(bird_data) <- gsub(" ", "_", names(bird_data))
  
  # Filter by date if possible
  if("OBSERVATION_DATE" %in% names(bird_data)) {
    # Convert to date if needed
    if(!inherits(bird_data$OBSERVATION_DATE, "Date")) {
      bird_data$OBSERVATION_DATE <- as.Date(bird_data$OBSERVATION_DATE)
    }
    
    # Filter to 2003-2025 range
    bird_data <- bird_data %>%
      filter(OBSERVATION_DATE >= as.Date("2003-01-01") & 
               OBSERVATION_DATE <= as.Date("2025-03-31"))
  }
  
  # Add season column
  if("OBSERVATION_DATE" %in% names(bird_data)) {
    bird_data$season <- case_when(
      month(bird_data$OBSERVATION_DATE) %in% c(12, 1, 2) ~ "Winter",
      month(bird_data$OBSERVATION_DATE) %in% c(3, 4, 5) ~ "Spring",
      month(bird_data$OBSERVATION_DATE) %in% c(6, 7, 8) ~ "Summer",
      month(bird_data$OBSERVATION_DATE) %in% c(9, 10, 11) ~ "Autumn"
    )
  }
  
  # Load map data
  land <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_transform(crs = st_crs(bird_data))
  
  amap_boundary <- st_read(amap_file, quiet = TRUE) %>%
    st_transform(crs = st_crs(bird_data))
  
  # Simplify the ice geometry
  ice_overall_simple <- st_simplify(ice_data$overall, dTolerance = 5000)
  
  # Get proper zoom based on AMAP boundary
  bbox <- st_bbox(amap_boundary)
  xlim <- c(bbox["xmin"] - 100000, bbox["xmax"] + 100000)
  ylim <- c(bbox["ymin"] - 100000, bbox["ymax"] + 100000)
  
  # Create a better visualization
  p <- ggplot() +
    # Base map
    geom_sf(data = land, fill = "grey95", color = "grey80", size = 0.1) +
    # Ice filled area with light blue
    geom_sf(data = ice_overall_simple, fill = "lightblue", alpha = 0.3, color = NA) +
    # Ice boundary
    geom_sf(data = ice_overall_simple, fill = NA, color = "darkblue", size = 0.8) +
    # AMAP boundary
    geom_sf(data = amap_boundary, fill = NA, color = "red", size = 0.5) +
    # Bird data with very small points and transparency to handle overlap
    geom_sf(data = bird_data, aes(color = pelagic_type), 
            size = 0.3, alpha = 0.5) +
    # Better color scheme
    scale_color_manual(values = c(
      "True Pelagic" = "#663399",
      "Partially Pelagic" = "coral"
    )) +
    # Proper zoom to AMAP area
    coord_sf(xlim = xlim, ylim = ylim) +
    # Clean theme
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  # Save the map
  ggsave("figures/birds_and_ice_recovered.png", p, width = 10, height = 10)
  
  cat("Clear visualization created with proper zoom and all data points\n")
  
  # Create seasonal maps
  if("season" %in% names(bird_data)) {
    for(season_name in c("Winter", "Spring", "Summer", "Autumn")) {
      # Get seasonal data
      season_birds <- bird_data %>% filter(season == season_name)
      
      if(nrow(season_birds) > 0) {
        # Get seasonal ice
        season_ice <- ice_data$seasonal %>% 
          filter(season == season_name) %>%
          st_simplify(dTolerance = 5000)
        
        if("season" %in% names(ice_data$seasonal)) {
          ice_data$seasonal$season <- recode(ice_data$seasonal$season, "Fall" = "Autumn")
        }
        
        p_season <- ggplot() +
          # Base map
          geom_sf(data = land, fill = "grey95", color = "grey80", size = 0.1) +
          # Ice filled area
          geom_sf(data = season_ice$geometry, fill = "lightblue", alpha = 0.3, color = NA) +
          # Ice boundary
          geom_sf(data = season_ice$geometry, fill = NA, color = "darkblue", size = 0.8) +
          # AMAP boundary
          geom_sf(data = amap_boundary, fill = NA, color = "red", size = 0.5) +
          # Bird data
          geom_sf(data = season_birds, aes(color = pelagic_type), 
                  size = 0.3, alpha = 0.5) +
          # Color scheme
          scale_color_manual(values = c(
            "True Pelagic" = "#663399",
            "Partially Pelagic" = "lightgreen"
          )) +
          # Zoom
          coord_sf(xlim = xlim, ylim = ylim) +
          # Theme
          theme_minimal() +
          theme(
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", color = NA),
            axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "white", color = NA)
          )
        
        ggsave(paste0("figures/birds_ice_recovered_", tolower(season_name), ".png"), 
               p_season, width = 10, height = 10)
      }
    }
  }
}

create_final()
