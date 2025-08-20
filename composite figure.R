rm(list = ls())

# Load libraries
library(dplyr)
library(ggplot2)
library(sf)
library(gridExtra)
library(grid)
library(scales)
library(cowplot)
library(stringr)

# Load data
bird_data <- readRDS("bird_data_effort_standardized.rds")

# Data preparation
bird_data$count_adjusted <- as.numeric(bird_data$`OBSERVATION COUNT`)
bird_data$count_adjusted[bird_data$`OBSERVATION COUNT` == "X"] <- 1
bird_data$year <- as.numeric(format(bird_data$`OBSERVATION DATE`, "%Y"))
bird_data$month <- as.numeric(format(bird_data$`OBSERVATION DATE`, "%m"))
bird_data$season <- case_when(
  bird_data$month %in% c(12, 1, 2) ~ "Winter",
  bird_data$month %in% c(3, 4, 5) ~ "Spring", 
  bird_data$month %in% c(6, 7, 8) ~ "Summer",
  bird_data$month %in% c(9, 10, 11) ~ "Autumn"
)

# Define color schemes
pelagic_colors <- c("Partially Pelagic" = "#F08080", "True Pelagic" = "steelblue")
neutral_color <- "#4A5568"
observer_colors <- c("Long-term (5+ years)" = "#8B0000",      # Dark red
                     "Medium-term (3-4 years)" = "#B22",   
                     "Short-term (2 years)" = "#F08080",      
                     "Single year" = "#FFB6C1") 
covid_colors <- c("2020" = "#1f3a93", "2021" = "#3b82f6", "2022" = "#93c5fd")


# Helper function for region names
clean_region_name <- function(region) {
  case_when(
    region == "alaska" ~ "Alaska", region == "canada" ~ "Canada", 
    region == "iceland" ~ "Iceland", region == "norway" ~ "Norway",
    region == "greenland" ~ "Greenland", region == "svalbard" ~ "Svalbard",
    region == "faroeislands" ~ "Faroe Islands", region == "highsea" ~ "High Seas",
    TRUE ~ str_to_title(region)
  )
}

total_obs <- nrow(bird_data)

# PANEL A: Temporal Coverage
yearly_summary <- bird_data %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  filter(year >= 2003, year <= 2025)

panel_a <- ggplot(yearly_summary, aes(x = year, y = n_observations)) +
  geom_col(fill = neutral_color, alpha = 0.8) +
  geom_smooth(data = yearly_summary %>% filter(year < 2025), 
              aes(color = "Temporal trend"), method = "loess", se = FALSE, linewidth = 1.2) +
  geom_point(data = yearly_summary %>% filter(year == 2025), 
             aes(x = year, y = n_observations), color = "#E76F51", size = 3) +
  scale_color_manual(values = c("Temporal trend" = "#E76F51"), name = "") +
  scale_x_continuous(breaks = seq(2003, 2025, 3)) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "(A)", x = "Year", y = "Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(size = 20),
        plot.margin = margin(5.5, 5.5, 5.5, 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20),
        legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 18))

# PANEL B: Species Composition (Raw Counts)
species_summary_raw <- bird_data %>%
  st_drop_geometry() %>%
  group_by(`COMMON NAME`, pelagic_type) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  arrange(desc(n_observations)) %>%
  head(10) %>%
  mutate(
    percentage = (n_observations / total_obs) * 100,
    `COMMON NAME` = factor(`COMMON NAME`, levels = rev(`COMMON NAME`))
  )

panel_b <- ggplot(species_summary_raw, aes(x = n_observations, y = `COMMON NAME`, fill = pelagic_type)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.1, size = 7) +
  scale_fill_manual(values = pelagic_colors, name = "Type") +
  scale_x_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(B)", x = "Observations", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.y = element_text(size = 18), 
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 18), 
        axis.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

# PANEL D: Seasonal Distribution (Raw Counts)
seasonal_summary_raw <- bird_data %>%
  st_drop_geometry() %>%
  group_by(season, pelagic_type) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))

seasonal_totals_raw <- seasonal_summary_raw %>%
  group_by(season) %>%
  summarise(total_season_obs = sum(n_observations), .groups = 'drop') %>%
  mutate(total_season_pct = (total_season_obs / total_obs) * 100)

panel_d <- ggplot(seasonal_summary_raw, aes(x = season, y = n_observations, fill = pelagic_type)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(data = seasonal_totals_raw, 
            aes(x = season, y = total_season_obs + max(total_season_obs) * 0.05, 
                label = paste0(round(total_season_pct, 1), "%")), 
            inherit.aes = FALSE, size = 7, vjust = 0) +
  scale_fill_manual(values = pelagic_colors, name = "Type") +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(D)", x = "Season", y = "Observations") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(size = 20), 
        plot.margin = margin(5.5, 5.5, 5.5, 20),
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

# PANEL C: Regional Coverage
regional_summary <- bird_data %>%
  st_drop_geometry() %>%
  group_by(data_source) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  mutate(data_source_clean = clean_region_name(data_source),
         percentage = (n_observations / total_obs) * 100) %>%
  arrange(desc(n_observations)) %>%
  mutate(data_source_clean = factor(data_source_clean, levels = data_source_clean))

panel_c <- ggplot(regional_summary, aes(x = n_observations, y = data_source_clean)) +
  geom_col(fill = neutral_color, alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.1, size = 7) +
  scale_x_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "(C)", x = "Observations", y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20))

# PANEL E: Monthly Distribution (Raw Counts)
monthly_summary_raw <- bird_data %>%
  st_drop_geometry() %>%
  group_by(month, pelagic_type) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  mutate(month_name = factor(month.abb[month], levels = month.abb))

panel_e <- ggplot(monthly_summary_raw, aes(x = month_name, y = n_observations, fill = pelagic_type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = pelagic_colors, name = "Type") +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "(E)", x = "Month", y = "Observations") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(size = 20), 
        plot.margin = margin(5.5, 5.5, 5.5, 20),
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

# PANEL F: Species Richness Over Time
quality_summary <- yearly_summary %>%
  left_join(bird_data %>% st_drop_geometry() %>% group_by(year) %>% 
              summarise(n_species = n_distinct(`COMMON NAME`), .groups = 'drop'), by = "year")

panel_f <- ggplot(quality_summary, aes(x = year, y = n_species)) +
  geom_line(data = quality_summary %>% filter(year < 2025), 
            color = neutral_color, linewidth = 1.2) +
  geom_point(data = quality_summary %>% filter(year < 2025), 
             color = neutral_color, size = 3) +
  geom_point(data = quality_summary %>% filter(year == 2025), 
             color = neutral_color, size = 3) +
  scale_x_continuous(breaks = seq(2003, 2025, 3)) +
  labs(title = "(F)", x = "Year", y = "Number of Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(size = 20),
        plot.margin = margin(5.5, 5.5, 5.5, 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20))

# PANEL G: Observer Composition
observer_patterns <- bird_data %>%
  st_drop_geometry() %>%
  group_by(`OBSERVER ID`, data_source) %>%
  summarise(years_active = n_distinct(year), n_checklists = n_distinct(`SAMPLING EVENT IDENTIFIER`), .groups = 'drop') %>%
  mutate(observer_type = case_when(years_active >= 5 ~ "Long-term (5+ years)", 
                                   years_active >= 3 ~ "Medium-term (3-4 years)", 
                                   years_active >= 2 ~ "Short-term (2 years)", 
                                   TRUE ~ "Single year"),
         data_source_clean = clean_region_name(data_source))

observer_summary <- observer_patterns %>%
  group_by(data_source_clean, observer_type) %>%
  summarise(n_observers = n(), .groups = 'drop') %>%
  group_by(data_source_clean) %>%
  mutate(total_observers = sum(n_observers), prop_observers = n_observers / total_observers * 100) %>%
  ungroup()

panel_g <- ggplot(observer_summary, aes(x = reorder(data_source_clean, total_observers), 
                                        y = prop_observers, fill = observer_type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = observer_colors, name = "Observer Type") +
  coord_flip() +
  labs(title = "(G)", x = "Region", y = "% of Observers") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# PANEL H: COVID Impact & Recovery (2020-2022)
covid_data_extended <- bird_data %>%
  st_drop_geometry() %>%
  filter(year %in% 2019:2022) %>%
  group_by(year, data_source) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  group_by(data_source) %>%
  arrange(year) %>%
  mutate(baseline_obs = first(n_observations), 
         change_from_2019 = (n_observations / baseline_obs - 1) * 100,
         data_source_clean = clean_region_name(data_source)) %>%
  filter(year %in% c(2020, 2021, 2022)) %>%
  select(data_source_clean, year, change_from_2019)

panel_h <- ggplot(covid_data_extended, aes(x = factor(year), y = change_from_2019, fill = factor(year))) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ data_source_clean, scales = "fixed") +
  scale_fill_manual(values = covid_colors, name = "Year") +
  labs(title = "(H)", x = "Year", y = "% Change from 2019") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom",
        plot.title = element_text(size = 20), 
        strip.text = element_text(size = 18),
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

# COMBINE PANELS (3x3 layout)
composite_figure <- plot_grid(
  panel_a, panel_b, panel_c,
  panel_d, panel_e, panel_f,
  panel_g, panel_h, NULL,
  ncol = 3, nrow = 3
)

ggsave("composite_final.png", composite_figure, 
       width = 25, height = 20, dpi = 600, bg = "white")









# Check Alaska data availability by year
alaska_check <- bird_data %>%
  st_drop_geometry() %>%
  filter(str_detect(tolower(data_source), "alaska")) %>%
  group_by(year) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  filter(year >= 2019)

print("Alaska observations by year:")
print(alaska_check)

# Check covid data processing for Alaska specifically
covid_debug <- bird_data %>%
  st_drop_geometry() %>%
  filter(year %in% 2019:2022) %>%
  group_by(year, data_source) %>%
  summarise(n_observations = n(), .groups = 'drop') %>%
  filter(str_detect(tolower(data_source), "alaska"))

print("Alaska in COVID processing:")
print(covid_debug)













cat("=== DATASET OVERVIEW STATISTICS ===\n")
cat("Total observations:", nrow(bird_data), "\n")
cat("Total species:", n_distinct(bird_data$`COMMON NAME`), "\n")
cat("Total observers:", n_distinct(bird_data$`OBSERVER ID`), "\n")
cat("Study period:", min(bird_data$year, na.rm = TRUE), "-", max(bird_data$year, na.rm = TRUE), "\n\n")

# Top 10 species percentages
cat("=== TOP 10 SPECIES (% of total OBSERVATIONS) ===\n")
species_summary_raw %>%
  arrange(desc(n_observations)) %>%
  select(`COMMON NAME`, pelagic_type, n_observations, percentage) %>%
  print()

cat("\nTop 10 species account for:", round(sum(species_summary_raw$percentage), 1), "% of total observations\n\n")

# Seasonal breakdown
cat("=== SEASONAL DISTRIBUTION (RAW OBSERVATIONS) ===\n")
seasonal_totals_raw %>%
  select(season, total_season_obs, total_season_pct) %>%
  rename(Season = season, `Total Observations` = total_season_obs, `Percentage` = total_season_pct) %>%
  print()

# Regional breakdown
cat("\n=== REGIONAL DISTRIBUTION (RAW OBSERVATIONS) ===\n")
regional_summary %>%
  select(data_source_clean, n_observations, percentage) %>%
  print()

# Seasonal comparison
cat("\n=== OBSERVATION COUNT COMPARISONS ===\n")
summer_obs <- seasonal_totals_raw$total_season_obs[seasonal_totals_raw$season == "Summer"]
winter_obs <- seasonal_totals_raw$total_season_obs[seasonal_totals_raw$season == "Winter"]
summer_winter_ratio <- summer_obs / winter_obs

cat("Summer observations:", format(summer_obs, big.mark = ","), "\n")
cat("Winter observations:", format(winter_obs, big.mark = ","), "\n")
cat("Summer is", round(summer_winter_ratio, 1), "times higher than winter\n\n")

# Monthly peak
monthly_totals_raw <- monthly_summary_raw %>%
  group_by(month) %>%
  summarise(monthly_total = sum(n_observations), .groups = 'drop') %>%
  arrange(desc(monthly_total))

peak_month <- month.name[monthly_totals_raw$month[1]]
peak_obs <- monthly_totals_raw$monthly_total[1]
cat("Peak month:", peak_month, "with", format(peak_obs, big.mark = ","), "observations\n")

# COVID impact summary
covid_summary <- covid_data_extended %>%
  group_by(year) %>%
  summarise(mean_change = mean(change_from_2019, na.rm = TRUE),
            regions_affected = sum(change_from_2019 < 0, na.rm = TRUE),
            .groups = 'drop')

cat("\n=== COVID IMPACT SUMMARY ===\n")
print(covid_summary)
