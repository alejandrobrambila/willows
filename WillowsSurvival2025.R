## read in data
willowsurvive <- read.csv("SurvivalRates2025.csv")

# Add libraries
library(tidyverse)

# New column combining month and year
willowsurvive$season_year <- paste(willowsurvive$season, willowsurvive$year)

# Establish correct timeline order
season_year_order <- c("Fall 2023", "Spring 2024", "Summer 2024", "Fall 2024",
                       "Spring 2025", "Fall 2025")
willowsurvive <- willowsurvive %>%
  mutate(season_year = factor(season_year, levels = season_year_order))


## Alejandro script

ggplot(
  data = willowsurvive,
  mapping = aes(x = season_year, y = ((count)), color = species, shape = tube))+
  geom_jitter(width=.15) +
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(y ="Survivors", color = "Legend")+ 
  facet_wrap(~plantingtype)

ggplot(
  data = willowsurvive,
  mapping= aes(x = season_year, y = ((percentsurvived)), color = species, shape = field))+
  geom_jitter(width=.15) +
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(y ="% Survivors", color ="Legend") + facet_wrap(~plantingtype)

ggplot(
  data = willowsurvive,
  mapping = aes(x = season_year, y = (1/(count/80)), color = species, shape = tube))+
  geom_jitter(width=.15)+ scale_y_log10()+
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(y ="Row Spacing (in ft)", color ="Legend")+ facet_wrap(~plantingtype)

ggplot(
  data = willowsurvive,
  mapping = aes(x = season_year, y = (1/(count/80)), color = species, shape = field))+
  geom_jitter(width=.15)+scale_y_log10()+
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(y ="Row Spacing (in ft)", color = "Legend")+ facet_wrap(~plantingtype)


## Leah addition
ggplot(
  data = willowsurvive,
  mapping = aes(x = season_year, y = (1/(count/80)), color = species, shape = plantingtype))+
  geom_jitter(width=.15)+scale_y_log10()+
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(y ="Row Spacing (in ft)", color = "Species")+ facet_wrap(~field)

ggplot(
  data = willowsurvive,
  mapping = aes(x = season_year, y = (1/(count/80)), color = field, shape = plantingtype))+
  geom_jitter(width=.15)+scale_y_log10()+
  scale_color_manual(values = c( "GP1"= "darkorange1", "GP2" = "violetred2",
                                 "Barberry" = "royalblue","Underhill"  = "darkolivegreen3")) +
  labs(y ="Row Spacing (in ft)", color = "Field")+ facet_wrap(~species)


## Leah script
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
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(title = "Average Percent Survival Over Time by Species",
       x = "Season",
       y = "Mean Percent Survived (%)") +
  theme_minimal()


willowsurvive %>%
  group_by(season_year, field) %>%
  summarise(mean_survival = mean(percentsurvived, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season_year,
             y = mean_survival,
             color = field,
             group = field)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c( "GP1"= "darkorange1", "GP2" = "violetred2",
                                 "Barberry" = "royalblue","Underhill"  = "darkolivegreen3")) +
  labs(title = "Average Percent Survival Over Time by Field",
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
  facet_wrap(~ field) +
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(title = "Average Percent Survival Over Time by Species",
       x = "Season",
       y = "Mean Percent Survived (%)") +
    theme_minimal()  



## *** Percent survived by species & field in Fall 2025 (most recent data collected)
willowsurvive %>%
filter(season_year == "Fall 2025") %>%
ggplot(aes(x = species, y = percentsurvived, fill = field)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ season_year) +
  theme_minimal() +
  scale_color_manual(values = c( "GP1"= "darkorange1", "GP2" = "violetred2",
                                 "Barberry" = "royalblue","Underhill"  = "darkolivegreen3")) +
  labs(title = "Percent Survived by Species and Field",
       x = "Species",
       y = "Percent Survived (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## Mean survival count
willowsurvive %>%
  group_by(field, season_year, species, plantingtype) %>%
  summarise(mean_count = mean(count, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season_year,
             y = mean_count,
             color = species, 
             linetype = plantingtype,
             group = interaction(species, plantingtype))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ field) +
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
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
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(title = "Final Surviving Count vs Initial Count (Fall 2025)",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()



## Same but averaged
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  group_by(field, species, plantingtype) %>%
  summarise(
    mean_firstcount = mean(firstcount, na.rm = TRUE),
    mean_count      = mean(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = mean_firstcount,
             y = mean_count,
             color = species,
             shape = plantingtype)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ field) +
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(title = "Average Final Surviving Count vs Initial Count (Fall 2025)",
       x = "Average Initial Count",
       y = "Average Final Surviving Count") +
  theme_minimal()


## Distinguishes the rows
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = species,
             shape = plantingtype,
             alpha = row)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ field) +
  scale_alpha_manual(values = c("Row1" = 1, "Row2" = 0.7, "Row3" = 0.4)) +
  scale_color_manual(values = c("Discolor"= "gold","Lucida" = "salmon",
                                "Purpurea" = "mediumorchid2","Sericea"  = "turquoise")) +
  labs(title = "Final Surviving Count vs Initial Count (Fall 2025)",
       x = "Initial Count",
       y = "Final Surviving Count",
       alpha = "Row") +
  theme_minimal()


## facet by species
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = field,
             shape = plantingtype)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ species) +
  scale_color_manual(values = c( "GP1"= "darkorange1", "GP2" = "violetred2",
                                 "Barberry" = "royalblue","Underhill"  = "darkolivegreen3")) +
  labs(title = "Final Surviving Count vs Initial Count (Fall 2025)",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()

## Same but averaged
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  group_by(field, species, plantingtype) %>%
  summarise(
    mean_firstcount = mean(firstcount, na.rm = TRUE),
    mean_count      = mean(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = mean_firstcount,
             y = mean_count,
             color = field,
             shape = plantingtype)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ species) +
  scale_color_manual(values = c( "GP1"= "darkorange1", "GP2" = "violetred2",
                                 "Barberry" = "royalblue","Underhill"  = "darkolivegreen3")) +
  labs(title = "Average Final Surviving Count vs Initial Count (Fall 2025)",
       x = "Average Initial Count",
       y = "Average Final Surviving Count") +
  theme_minimal()



## Do i actually like these:
{
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


## Initial vs final count (by species)
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = fieldplanting)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ species) +
  labs(title = "Final Surviving vs Initial Count by Field–Planting Combination",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()

## Initial vs final count (by fieldplanting)
willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  ggplot(aes(x = firstcount,
             y = count,
             color = species)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ fieldplanting) +
  labs(title = "Final Surviving vs Initial Count by Field–Planting Combination",
       x = "Initial Count",
       y = "Final Surviving Count") +
  theme_minimal()
  }


## Tube

willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  mutate(survival_ratio = count / firstcount) %>%
  ggplot(aes(x = tube,
             y = survival_ratio,
             fill = tube)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ field) +
  labs(title = "Effect of Tubes on Survival (Fall 2025)",
       x = "Tube Status",
       y = "Proportion Surviving") +
  theme_minimal()

willowsurvive %>%
  group_by(field, season_year, tube) %>%
  summarise(mean_survival = mean(percentsurvived, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season_year,
             y = mean_survival,
             color = tube,
             group = tube)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ field) +
  labs(title = "Survival Trends by Tube Status",
       x = "Season",
       y = "Mean Percent Survived (%)",
       color = "Tube Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

willowsurvive %>%
  filter(season_year == "Fall 2025") %>%
  mutate(survival_ratio = count / firstcount) %>%
  ggplot(aes(x = tube,
             y = survival_ratio,
             fill = species)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~ field) +
  labs(title = "Tube Effects by Species (Fall 2025)",
       x = "Tube Status",
       y = "Proportion Surviving") +
  theme_minimal()


willowsurvive %>%
  group_by(field, season_year, tube) %>%
  summarise(mean_survival = mean(percentsurvived, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = season_year,
             y = mean_survival,
             color = tube,
             group = tube)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ field) +
  labs(title = "Survival Trends: Tube vs No-Tube",
       x = "Season",
       y = "Mean Percent Survived (%)",
       color = "Tube Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
