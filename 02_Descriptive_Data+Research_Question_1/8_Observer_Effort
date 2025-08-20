# Focus: Regional patterns, COVID impact, and data reliability
rm( list = ls())

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

# Load data
bird_data <- readRDS("bird_data_detail_analysis_new.rds")

cat("=== OBSERVER EFFORT ANALYSIS ===\n")
cat("Total observations:", nrow(bird_data), "\n")
cat("Study period:", min(bird_data$year), "-", max(bird_data$year), "\n\n")

# 1. ANNUAL EFFORT PATTERNS BY REGION
cat("1. Annual effort patterns by region...\n")

annual_effort <- bird_data %>%
  st_drop_geometry() %>%
  group_by(year, data_source) %>%
  summarise(
    n_observers = n_distinct(`OBSERVER ID`),
    n_checklists = n_distinct(`SAMPLING EVENT IDENTIFIER`),
    n_observations = n(),
    total_duration_hours = sum(`DURATION MINUTES`, na.rm = TRUE) / 60,
    mean_duration_min = mean(`DURATION MINUTES`, na.rm = TRUE),
    n_species = n_distinct(`COMMON NAME`),
    .groups = 'drop'
  )

# Overall regional summary
regional_summary <- annual_effort %>%
  group_by(data_source) %>%
  summarise(
    total_obs = sum(n_observations),
    total_observers = sum(n_observers),
    years_active = n_distinct(year),
    first_year = min(year),
    last_year = max(year),
    .groups = 'drop'
  ) %>%
  mutate(prop_dataset = total_obs / sum(total_obs) * 100) %>%
  arrange(desc(total_obs))

print("Regional data contributions:")
print(regional_summary)

# 2. COVID-19 IMPACT ANALYSIS
cat("\n2. COVID-19 impact analysis (2019-2021)...\n")

covid_comparison <- bird_data %>%
  st_drop_geometry() %>%
  filter(year %in% 2019:2021) %>%
  group_by(year, data_source) %>%
  summarise(
    n_observers = n_distinct(`OBSERVER ID`),
    n_checklists = n_distinct(`SAMPLING EVENT IDENTIFIER`),
    n_observations = n(),
    mean_duration = mean(`DURATION MINUTES`, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate percentage changes from 2019 baseline (only show 2020 and 2021)
covid_changes <- covid_comparison %>%
  group_by(data_source) %>%
  arrange(year) %>%
  mutate(
    baseline_obs = first(n_observations),
    obs_change_from_2019 = (n_observations / baseline_obs - 1) * 100
  ) %>%
  filter(year %in% c(2020, 2021)) %>%
  dplyr::select(data_source, year, n_observations, obs_change_from_2019)

print("COVID-19 impact and recovery (% change from 2019):")
print(covid_changes)

# Recovery analysis
recovery_summary <- covid_changes %>%
  dplyr::select(data_source, year, obs_change_from_2019) %>%  # Add dplyr:: prefix
  pivot_wider(names_from = year, values_from = obs_change_from_2019, 
              names_prefix = "change_") %>%
  mutate(
    recovery_magnitude = change_2021 - change_2020,
    recovery_status = case_when(
      change_2021 > -10 ~ "Near full recovery (>90% of 2019 levels)",
      recovery_magnitude > 50 ~ "Strong recovery (>50% improvement from 2020)",
      recovery_magnitude > 20 ~ "Moderate recovery (20-50% improvement)",
      recovery_magnitude > 0 ~ "Slight recovery",
      TRUE ~ "No recovery or continued decline"
    )
  ) %>%
  arrange(change_2020)

cat("\n2020 impact and 2021 recovery patterns:\n")
print(recovery_summary)


select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename


final_table <- recovery_summary %>%
  select(data_source, change_2020, change_2021, recovery_magnitude) %>%
  rename(
    "Data Source" = data_source,
    "2020 Impact (% vs 2019)" = change_2020,
    "2021 Level (% vs 2019)" = change_2021,
    "2021 Recovery (% vs 2020)" = recovery_magnitude
  )

print(final_table)

# 3. OBSERVER CONSISTENCY AND RELIABILITY
cat("\n3. Observer reliability analysis...\n")

observer_patterns <- bird_data %>%
  st_drop_geometry() %>%
  group_by(`OBSERVER ID`, data_source) %>%
  summarise(
    years_active = n_distinct(year),
    n_checklists = n_distinct(`SAMPLING EVENT IDENTIFIER`),
    total_observations = n(),
    n_species_total = n_distinct(`COMMON NAME`),
    mean_duration = mean(`DURATION MINUTES`, na.rm = TRUE),
    cv_duration = sd(`DURATION MINUTES`, na.rm = TRUE) / mean(`DURATION MINUTES`, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(n_checklists >= 3) %>%  # Only multi-checklist observers
  mutate(
    observer_type = case_when(
      years_active >= 5 ~ "Long-term (5+ years)",
      years_active >= 3 ~ "Medium-term (3-4 years)",
      years_active >= 2 ~ "Short-term (2 years)",
      TRUE ~ "Single year"
    ),
    consistency = case_when(
      cv_duration <= 0.5 ~ "High consistency",
      cv_duration <= 1.0 ~ "Medium consistency",
      TRUE ~ "Variable"
    )
  )

reliability_by_region <- observer_patterns %>%
  group_by(data_source, observer_type) %>%
  summarise(
    n_observers = n(),
    mean_obs_per_observer = mean(total_observations),
    .groups = 'drop'
  )

print("Observer reliability by region:")
print(reliability_by_region)


# 4. EFFORT-STANDARDIZED DETECTION RATES
cat("\n4. Effort-standardized detection rates...\n")

# Calculate per-checklist metrics
checklist_metrics <- bird_data %>%
  st_drop_geometry() %>%
  filter(`DURATION MINUTES` > 0 & `DURATION MINUTES` <= 480) %>%  # 0-8 hours
  group_by(`SAMPLING EVENT IDENTIFIER`, year, data_source, season) %>%
  summarise(
    duration_hours = first(`DURATION MINUTES`) / 60,
    n_species = n_distinct(`COMMON NAME`),
    n_observations = n(),
    total_birds = sum(count_adjusted, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    species_per_hour = n_species / duration_hours,
    birds_per_hour = total_birds / duration_hours
  )

# Annual standardized rates by region
standardized_annual <- checklist_metrics %>%
  group_by(year, data_source) %>%
  summarise(
    n_checklists = n(),
    mean_species_per_hour = mean(species_per_hour, na.rm = TRUE),
    median_species_per_hour = median(species_per_hour, na.rm = TRUE),
    mean_birds_per_hour = mean(birds_per_hour, na.rm = TRUE),
    .groups = 'drop'
  )

print("Effort-standardized detection rates (recent years):")
print(standardized_annual %>% filter(year >= 2020))

# 5. TEMPORAL PATTERNS IN EFFORT
cat("\n5. Temporal effort patterns...\n")

# Monthly effort distribution
monthly_effort <- bird_data %>%
  st_drop_geometry() %>%
  group_by(month, data_source) %>%
  summarise(
    n_observations = n(),
    .groups = 'drop'
  ) %>%
  group_by(data_source) %>%
  mutate(prop_annual = n_observations / sum(n_observations))

# Seasonal effort by region
seasonal_effort <- bird_data %>%
  st_drop_geometry() %>%
  group_by(season, data_source) %>%
  summarise(
    n_observations = n(),
    n_checklists = n_distinct(`SAMPLING EVENT IDENTIFIER`),
    .groups = 'drop'
  ) %>%
  group_by(data_source) %>%
  mutate(prop_annual = n_observations / sum(n_observations))

print("Seasonal effort distribution by region:")
print(seasonal_effort)

# 6. VISUALIZATIONS
cat("\n6. Creating visualizations...\n")

# Plot 1: Overall data coverage over time (ALL regions)
p1 <- annual_effort %>%
  clean_region_names() %>%
  ggplot(aes(x = year, y = n_observations, color = data_source)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.5) +
  scale_color_brewer(type = "qual", palette = "Set3") +
  scale_y_log10() +
  labs(x = "Year", y = "Observations (log scale)", color = "Region") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: COVID impact on ALL regions
p2 <- recovery_summary %>%
  clean_region_names() %>%
  select(data_source, change_2020, change_2021) %>%
  pivot_longer(cols = c(change_2020, change_2021), 
               names_to = "year", values_to = "change") %>%
  mutate(year = ifelse(year == "change_2020", "2020", "2021")) %>%
  ggplot(aes(x = factor(year), y = change, fill = factor(year))) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ data_source, scales = "free_y") +
  scale_fill_manual(values = c("2020" = "#E76F51", "2021" = "steelblue"), name = "Year") +
  labs(x = "Year", y = "% Change from 2019 Baseline") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        axis.title.y = element_text(margin = margin(r = 15)))

print(p2)

# Plot 3: Long-term data reliability by region
reliability_data <- annual_effort %>%
  group_by(data_source) %>%
  summarise(
    total_obs = sum(n_observations),
    years_covered = n(),
    consistency = sd(n_observations) / mean(n_observations),
    .groups = 'drop'
  )

#change direction, p3 not needed

effort_quality <- bird_data %>%
  filter(!is.na(`DURATION MINUTES`)) %>%
  ggplot(aes(x = data_source, y = `DURATION MINUTES`)) +
  geom_boxplot()

print(effort_quality) #maybe not needed as well.

# Plot 4: Detection rates are comparable (effort standardization)
p4 <- standardized_annual %>%
  filter(n_checklists >= 5) %>%  # Only regions with sufficient data
  ggplot(aes(x = data_source, y = mean_species_per_hour)) +
  geom_boxplot(alpha = 0.7, fill = "lightblue") +
  labs(x = "Region", y = "Species per Hour") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)
#p4 not needed as well
colnames(bird_data)

p_observer_composition <- reliability_by_region %>%
  clean_region_names() %>%
  group_by(data_source) %>%
  mutate(total_observers = sum(n_observers),
         prop_observers = n_observers / total_observers * 100) %>%
  ggplot(aes(x = reorder(data_source, total_observers), y = prop_observers, fill = observer_type)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("Long-term (5+ years)" = "#E63946",
                               "Medium-term (3-4 years)" = "#E76F51", 
                               "Short-term (2 years)" = "#F4A261",
                               "Single year" = "#E9C46A")) +
  coord_flip() +
  labs(x = "Region", y = "% of Observers",
       fill = "Observer Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_observer_composition)



#Capital
clean_region_names <- function(data, region_column = "data_source") {
  data %>%
    mutate(
      !!region_column := case_when(
        !!sym(region_column) == "alaska" ~ "Alaska",
        !!sym(region_column) == "canada" ~ "Canada", 
        !!sym(region_column) == "iceland" ~ "Iceland",
        !!sym(region_column) == "norway" ~ "Norway",
        !!sym(region_column) == "greenland" ~ "Greenland",
        !!sym(region_column) == "svalbard" ~ "Svalbard",
        !!sym(region_column) == "faroeislands" ~ "Faroe Islands",
        !!sym(region_column) == "highsea" ~ "High Seas",
        TRUE ~ str_to_title(!!sym(region_column))  # Fallback for any other regions
      )
    )
}

library(stringr)


# 7. STATISTICAL TESTS
cat("\n7. Statistical significance tests...\n")

# Test 1: Observer engagement patterns (ALL regions)
cat("1. Observer engagement patterns across ALL regions...\n")

observer_all <- reliability_by_region %>%
  mutate(observer_category = ifelse(observer_type == "Single year", "Single year", "Multi-year")) %>%
  group_by(data_source, observer_category) %>%
  summarise(n_observers = sum(n_observers), .groups = 'drop') %>%
  pivot_wider(names_from = observer_category, values_from = n_observers, values_fill = 0)

print(observer_all)

# Chi-square test
observer_matrix <- as.matrix(observer_all[, -1])
rownames(observer_matrix) <- observer_all$data_source
chi_result <- chisq.test(observer_matrix)

cat(paste("Chi-square p-value:", round(chi_result$p.value, 4), "\n"))
cat(paste("Regions tested:", nrow(observer_all), "\n\n"))
#Chi-square test reported p<0.001, confirm regional difference in observer pattern

# Test 2: COVID impacts (ALL regions with 2019-2021 data)
cat("2. COVID impacts across ALL regions...\n")

covid_all <- data.frame()
all_regions <- unique(bird_data$data_source)

for(region in all_regions) {
  region_data <- bird_data %>%
    st_drop_geometry() %>%
    filter(data_source == region, year %in% c(2019, 2020, 2021)) %>%
    group_by(year) %>%
    summarise(n_obs = n(), .groups = 'drop')
  
  # Only include if has data for all 3 years
  if(nrow(region_data) == 3 && all(region_data$n_obs > 0)) {
    pre_covid <- region_data$n_obs[region_data$year == 2019]
    covid <- region_data$n_obs[region_data$year == 2020] 
    
    covid_impact_pct <- (covid - pre_covid) / pre_covid * 100
    
    covid_all <- rbind(covid_all, data.frame(
      region = region,
      covid_impact_pct = covid_impact_pct
    ))
  }
}

print(covid_all)

# Wilcoxon test
covid_wilcox <- wilcox.test(covid_all$covid_impact_pct, mu = 0)
cat(paste("Wilcoxon p-value:", round(covid_wilcox$p.value, 3), "\n"))
cat(paste("Median COVID impact:", round(median(covid_all$covid_impact_pct), 1), "%\n"))
cat(paste("Regions tested:", nrow(covid_all), "\n\n"))

# Test 3: Correlation (ALL regions with both datasets)
cat("3. Observer patterns vs COVID resilience - ALL regions...\n")

single_year_props <- observer_all %>%
  mutate(prop_single_year = `Single year` / (`Single year` + `Multi-year`) * 100) %>%
  select(data_source, prop_single_year)

correlation_all <- single_year_props %>%
  left_join(covid_all, by = c("data_source" = "region")) %>%
  filter(!is.na(covid_impact_pct))

print(correlation_all)

# Spearman correlation
cor_result <- cor.test(correlation_all$prop_single_year, 
                       abs(correlation_all$covid_impact_pct),
                       method = "spearman")

cat("Spearman correlation:", round(cor_result$estimate, 3), "\n")
cat(paste("P-value:", round(cor_result$p.value, 3), "\n"))
cat(paste("Regions tested:", nrow(correlation_all), "\n"))


# Save results
saveRDS(list(
  regional_summary = regional_summary,
  observer_engagement = observer_all,  # The simple single/multi-year table
  covid_impacts = covid_all,
  annual_effort = annual_effort,
  reliability_by_region = reliability_by_region,
  statistical_results = list(
    chi_square_p = chi_result$p.value,
    covid_wilcox_p = covid_wilcox$p.value,
    correlation_p = cor_result$p.value
  )
), "observer_effort_analysis_final.rds")

cat("Observer effort analysis complete! Moving to main pelagic bird distribution analysis.\n")
