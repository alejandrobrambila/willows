## read in data
willowsurvive <- read.csv("SurvivalRates2025.csv")

# Add libraries
library(ggplot2)
library(dplyr)
library(tidyverse)

# New column combining month and year
willowsurvive$season_year <- paste(willowsurvive$season, willowsurvive$year)

# Establish correct timeline order
season_year_order <- c("Fall 2023", "Spring 2024", "Summer 2024", "Fall 2024",
                       "Spring 2025", "Fall 2025")
willowsurvive <- willowsurvive %>%
  mutate(season_year = factor(season_year, levels = season_year_order))


## ??
ggplot(willowsurvive, aes(x = season_year, y = percentsurvived, color = species, 
                          line = plantingtype, group = species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ field) +
  labs(title = "Percent Survival Over Time by Species",
       x = "Season",
       y = "Percent Survived") +
  theme_minimal()



## Percent survived (mean)
willowsurvive %>%
  group_by(season_year, species) %>%
  summarise(mean_survival = mean(percentsurvived, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season_year,
             y = mean_survival,
             color = species,
             group = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Percent Survival Over Time by Species",
       x = "Season",
       y = "Mean Percent Survived (%)") +
  theme_minimal()


## Percent survived (mean) by field  
willowsurvive %>%
  group_by(field, season_year, species) %>%
  summarise(mean_survival = mean(percentsurvived, na.rm = TRUE),
              .groups = "drop") %>%
  ggplot(aes(x = season_year,
               y = mean_survival,
               color = species,
               group = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ field)
  labs(title = "Average Percent Survival Over Time by Species",
       x = "Season",
       y = "Mean Percent Survived (%)") +
    theme_minimal()  



## Percent survived by species & plantingtype in Fall 2025 (most recent data collected)
willowsurvive %>%
filter(season_year == "Fall 2025") %>%
ggplot(aes(x = species, y = percentsurvived, fill = field)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ season_year) +
  theme_minimal() +
  labs(title = "Percent Survived by Species and Planting Type",
       x = "Species",
       y = "Percent Survived (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Mean survival count
willowsurvive %>%
  group_by(field, season_year, species) %>%
  summarise(mean_count = mean(count, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season_year,
             y = mean_count,
             color = species,
             group = species)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ field) +
  labs(title = "Average Surviving Count Over Time by Species and Field",
       x = "Season",
       y = "Mean Surviving Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Initial vs final count scatter plot (with species)
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = species,
             shape = plantingtype)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ field) +
  labs(title = "Final Surviving Count vs Initial Count (Fall 2025)",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()


## Initial vs final count (by only planting type)
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = plantingtype)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ field) +
  labs(title = "Final Surviving vs Initial Count by Planting Type",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()


## Initial vs final count (by fieldplanting)
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = fieldplanting)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ field) +
  labs(title = "Final Surviving vs Initial Count by Field–Planting Combination",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()

