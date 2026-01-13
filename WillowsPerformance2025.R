# Set location to read in data
setwd("C:/Users/leahm/OneDrive/Desktop/Trustees/R Studio")

# Read in data
willowperform <- read.csv("Willow Performance Data(TidyData).csv")

# Add libraries (first is for plots, second is for editing data)
library(ggplot2)
library(dplyr)


# New column combining month and year
willowperform$month_year <- paste(willowperform$month, willowperform$year)


# Remove unnecessary columns
willowperform <- willowperform %>%
  select(-protected, -death, -browse, -X, -X.1)


# Plot: Willow height by all factors
ggplot(willowperform, aes(x = species, y = height_cm, fill = plantingtype)) +
  geom_boxplot() +
  facet_wrap(~ field) +
  scale_fill_manual(
    values = c("Rooted" = "turquoise4", "Staked" = "violetred3")) +
  labs(title = "Willow height by species, method, and location")


# Plot: Violin graph of height distribution by field
ggplot(willowperform, aes(x = field, y = height_cm, fill = field)) +
  geom_violin(trim = FALSE) +
  labs(title = "Height Distribution by Field",
       x = "Field", y = "Height (cm)") +
  theme_minimal()


# Plot: Height by field and planting type
ggplot(willowperform, aes(x = field, y = height_cm, fill = plantingtype)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Height by Field and Planting Type",
       x = "Field", y = "Height (cm)") +
  theme_minimal()


# GROWTH OVER TIME


# Reorder
month_year_order <- c("March 2024", "May 2024", "July 2024", "October 2024", 
                      "April 2025","July 2025", "August 2025", "October 2025")

# Fix month order
willowperform <- willowperform %>%
  mutate(month_year = factor(month_year, levels = month_year_order))

# BY HEIGHT

# Summarize data
willow_summary <- willowperform %>%
  group_by(month_year) %>%
  summarise(mean_height = mean(height_cm, na.rm = TRUE))

# Plot: Height overtime
ggplot(willow_summary, aes(x = month_year, y = mean_height, group = 1)) +
  geom_line(color = "forestgreen", linewidth = 1.2) +
  geom_point() +
  labs(title = "Average Plant Height Over Time",
       x = "Month/Year", y = "Average Height (cm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# height by species
ggplot(willowperform, aes(x = month_year, y = height_cm, 
                          color = species, group = species)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +    
  # average height per time point
  stat_summary(fun = mean, geom = "point", size = 2) +     
  # optional: add points
  scale_color_manual(values = c( "discolor"= "gold",
                                 "lucida" = "salmon",
                                 "purpurea" = "mediumorchid2",
                                 "sericea"  = "turquoise3")) +
  labs(title = "Average Willow Height Growth Over Time by Species",
       x = "Month and Year",
       y = "Average Height (cm)",
       color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Height by species and plantingtype
ggplot(willowperform, aes(x = month_year, y = height_cm, 
                          color = species, linetype = plantingtype, group = interaction(species, plantingtype))) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +    
  # average height per time point
  stat_summary(fun = mean, geom = "point", size = 2) +     
  # optional: add points
 # facet_wrap(~ plantingtype) +
  scale_color_manual(values = c( "discolor"= "gold",
                                 "lucida" = "salmon",
                                 "purpurea" = "mediumorchid2",
                                 "sericea"  = "turquoise3")) +
  labs(title = "Average Willow Height Growth Over Time by Species and Planting type",
       x = "Month and Year",
       y = "Average Height (cm)",
       color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Height by field
ggplot(willowperform, aes(x = month_year, y = height_cm, 
                          color = field, group = field)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +    
  # average height per time point
  stat_summary(fun = mean, geom = "point", size = 2) +     
  # optional: add points
  scale_color_manual(values = c( "GP1"= "gold",
                                 "GP2" = "salmon",
                                 "Barberry" = "mediumorchid2",
                                 "Underhill"  = "turquoise3")) +
  labs(title = "Average Willow Height Growth Over Time by Field",
       x = "Month and Year",
       y = "Average Height (cm)",
       color = "Field") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Plot: Average Height overtime by field and species
ggplot(willowperform, aes(x = month_year, y = height_cm, 
                          color = species, group = species)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +    
  # average height per time point
  stat_summary(fun = mean, geom = "point", size = 2) +     
  # optional: add points
  facet_wrap(~ field) + 
  scale_color_manual(values = c( "discolor"= "gold",
                                 "lucida" = "salmon",
                                 "purpurea" = "mediumorchid2",
                                 "sericea"  = "turquoise3")) +
  labs(title = "Average Willow Height Growth Over Time by Field and Species",
       x = "Month and Year",
       y = "Average Height (cm)",
       color = "Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Swap field and species
ggplot(willowperform, aes(x = month_year, y = height_cm, 
                          color = field, group = field)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +    
  # average height per time point
  stat_summary(fun = mean, geom = "point", size = 2) +     
  # optional: add points
  facet_wrap(~ species) + 
  scale_color_manual(values = c( "GP1"= "gold",
                                 "GP2" = "salmon",
                                 "Barberry" = "mediumorchid2",
                                 "Underhill"  = "turquoise3")) +
  labs(title = "Average Willow Height Growth Over Time by Species and Field",
       x = "Month and Year",
       y = "Average Height (cm)",
       color = "Field") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## species and plantingtype
ggplot(willowperform, aes(x = month_year, y = height_cm, 
                          color = plantingtype, group = plantingtype)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +    
  # average height per time point
  stat_summary(fun = mean, geom = "point", size = 2) +     
  # optional: add points
  facet_wrap(~ species) + 
  scale_color_manual(values = c( "Staked"= "gold","Rooted" = "mediumorchid2")) +
  labs(title = "Average Willow Height Growth Over Time by Species and Planting type",
       x = "Month and Year",
       y = "Average Height (cm)",
       color = "Planting type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Define standard error function for error bars
se_function <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x[!is.na(x)]))
}

# Plot: Willow Height Growth Over Time by Species and Field
ggplot(willowperform, aes(x = month_year, y = height_cm,
                          color = species,
                          group = interaction(species, field))) +
  
  # Mean line
  stat_summary(fun = mean, geom = "line", size = 0.8) +
  
  # Mean point
  stat_summary(fun = mean, geom = "point", size = 1.5) +
  
  # Error bars (± SE)
  stat_summary(fun.data = function(y) {
    data.frame(
      y = mean(y, na.rm = TRUE),
      ymin = mean(y, na.rm = TRUE) - se_function(y),
      ymax = mean(y, na.rm = TRUE) + se_function(y)
    )
  }, geom = "errorbar", width = 0.2, aes(color = NULL), color = "grey30", size = 0.4) +
scale_color_manual(values = c(
  "discolor"= "gold",
  "lucida" = "salmon",
  "purpurea" = "mediumorchid2",
  "sericea"  = "turquoise3"
)) +
  
  labs(title = "Willow Height Growth Over Time by Species and Field",
       x = "Month and Year",
       y = "Height (cm)",
       color = "Species") +
  
  theme_minimal(base_size = 13) +
  facet_wrap(~ field) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14)
  )



# Adding library to fix date
library(lubridate)

# Turn month and year into a readable date
willowperform <- willowperform %>%
  mutate(date_real = ymd(paste(year, month, "01")))

# Plot: Willows Average Height Overtime (Correctly spaced out)
ggplot(willowperform, aes(
  x = date_real,
  y = height_cm,
  color = species,
  linetype = plantingtype,
  group = interaction(species, plantingtype)
)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~ field) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(title = "Willows Average Height Overtime",
    x = "Date",
    y = "Average Height (cm)",
    color = "Species",
    linetype = "Planting Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Summarize data by height & field
height_field <- willowperform %>%
  group_by(month_year, field) %>%
  summarise(mean_height = mean(height_cm, na.rm = TRUE))

# Plot: Average height overtime by field
ggplot(height_field, aes(x = month_year, y = mean_height, 
                         color = field, group = field)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Average Willow Height Over Time by Field",
    x = "Month / Year",
    y = "Average Height (cm)",
    color = "Field"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Summarize data by height and planting type
height_type <- willowperform %>%
  group_by(month_year, plantingtype) %>%
  summarise(mean_height = mean(height_cm, na.rm = TRUE))

# Plot: Height by planting type
ggplot(height_type, aes(x = month_year, y = mean_height, 
                        color = plantingtype, group = plantingtype)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Average Willow Height Over Time by Planting Type",
    x = "Month / Year",
    y = "Average Height (cm)",
    color = "Planting Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# BY STEM COUNT


# Summarize data by stems & field
stems_field <- willowperform %>%
  group_by(month_year, field) %>%
  summarise(mean_stems = mean(stems, na.rm = TRUE))

# Plot: Stems by field overtime
ggplot(stems_field, aes(x = month_year, y = mean_stems, 
                        color = field, group = field)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Average Number of Stems Over Time by Field",
    x = "Month / Year",
    y = "Average Number of Stems",
    color = "Field"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summarize data by stems & planting type
stems_type <- willowperform %>%
  group_by(month_year, plantingtype) %>%
  summarise(mean_stems = mean(stems, na.rm = TRUE))

# Plot: Stems by planting type overtime
ggplot(stems_type, aes(x = month_year, y = mean_stems, 
                       color = plantingtype, group = plantingtype)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Average Number of Stems Over Time by Planting Type",
    x = "Month / Year",
    y = "Average Number of Stems",
    color = "Planting Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# AGE

# Fix date column to classify as date
willowperform$date <- as.Date(willowperform$date, format = "%m/%d/%Y")

# New table with planting dates
start_dates <- willowperform %>%
  group_by(field, plantingtype, species) %>%
  summarise(start_date = min(date, na.rm = TRUE)) %>%
  ungroup()

# New table combining age dates
willow_age <- willowperform %>%
  left_join(start_dates, by = c("field", "species", "plantingtype"))

# Calculating age by days, weeks, and months
willow_age <- willow_age %>%
  mutate(age_days = as.numeric(difftime(date, start_date, units = "days")),
         age_weeks = age_days / 7,
         age_months = age_days / 30.44)

# Plot: Height by age (weeks)
ggplot(willow_age, aes(x = age_weeks, y = height_cm,
                       color = species, linetype = plantingtype)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  facet_wrap(~ field) +
  labs(title = "Willow Height Growth by Age (Weeks)",
       x = "Age (weeks since first measurement)",
       y = "Height (cm)",
       color = "Species",
       linetype = "Planting Type") +
  theme_minimal(base_size = 13)

