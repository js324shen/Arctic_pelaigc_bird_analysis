# UNIFIED EFFORT STANDARDIZATION FOR ALL ANALYSES

rm(list = ls())

library(dplyr)
library(ggplot2)
library(sf)

# Load dataset with ice distances already calculated
bird_data <- readRDS("bird_data_with_annual_ice_distance.rds")

cat("Applying unified effort standardization...\n")

# First, examine and filter the data
cat("Original data size:", nrow(bird_data), "\n")
cat("Zero duration records:", sum(bird_data$`DURATION MINUTES` == 0, na.rm = TRUE), "\n")
cat("Missing duration records:", sum(is.na(bird_data$`DURATION MINUTES`)), "\n")

# Filter out zero duration and examine duration distribution
bird_data_filtered <- bird_data %>%
  filter(`DURATION MINUTES` > 0 | is.na(`DURATION MINUTES`))

cat("After removing zero duration:", nrow(bird_data_filtered), "\n")

# Plot duration histogram
duration_plot <- ggplot(bird_data_filtered, aes(x = `DURATION MINUTES`)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  labs(x = "Duration (minutes)", y = "Number of checklists",
       title = "Distribution of eBird Checklist Durations",
       subtitle = "eBird recommends <180 min for stationary counts") +
  geom_vline(xintercept = 180, color = "red", linetype = "dashed", size = 1) +
  theme_minimal()

print(duration_plot)

# Summary statistics
cat("\nDuration summary statistics:\n")
print(summary(bird_data$`DURATION MINUTES`))
cat("99th percentile:", quantile(bird_data$`DURATION MINUTES`, 0.99, na.rm = TRUE), "minutes\n")
cat("Checklists >180 min:", sum(bird_data$`DURATION MINUTES` > 180, na.rm = TRUE), "\n")
cat("Checklists >300 min:", sum(bird_data$`DURATION MINUTES` > 300, na.rm = TRUE), "\n")

# Examine long checklists (>300 minutes = 5+ hours)
cat("\n=== EXAMINING LONG CHECKLISTS (>300 minutes) ===\n")
long_checklists <- bird_data_filtered %>%
  st_drop_geometry() %>%
  filter(`DURATION MINUTES` > 300) %>%
  dplyr::select(`DURATION MINUTES`, `OBSERVATION DATE`, year, `PROTOCOL NAME`, 
                `OBSERVER ID`, `NUMBER OBSERVERS`, `EFFORT DISTANCE KM`, 
                LOCALITY, `ALL SPECIES REPORTED`) %>%
  distinct()

cat("Number of unique long checklists:", nrow(long_checklists), "\n")

# Year distribution
cat("\nYear distribution of long checklists:\n")
year_table <- table(long_checklists$year)
print(year_table)

# Top 10 longest checklists
cat("\nTop 10 longest checklists:\n")
longest <- long_checklists %>%
  arrange(desc(`DURATION MINUTES`)) %>%
  head(10)
print(longest)



# Filter and standardize
bird_data_standardized <- bird_data %>%
  filter(`DURATION MINUTES` > 0 & `DURATION MINUTES` <= 180) %>%
  mutate(
    duration_hours = `DURATION MINUTES` / 60,
    n_observers = ifelse(is.na(`NUMBER OBSERVERS`) | `NUMBER OBSERVERS` == 0, 1, `NUMBER OBSERVERS`),
    total_observer_effort = duration_hours * n_observers,
    abundance_standardized = count_adjusted / total_observer_effort,
    presence_standardized = ifelse(abundance_standardized > 0, 1, 0)
  )

# Save
saveRDS(bird_data_standardized, "bird_data_effort_standardized.rds")
colnames(bird_data_standardized)
