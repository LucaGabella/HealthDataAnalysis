# ------------------------------------------------------------------------------
# Data Visualization of Global Health Trends
# Author: Jesse Ohwariovbe
# Description: This script analyzes trends in BMI, obesity, blood pressure, 
# diabetes, and socioeconomic factors across different superregions.
# ------------------------------------------------------------------------------

# Check R Version
R.version.string  # R version 4.2.2 (2022-10-31 ucrt)

# Load required libraries
library(ggplot2)      # Version: packageVersion("ggplot2") - Visualization
library(tidyverse)    # Version: packageVersion("tidyverse") - Data Wrangling & Visualization
library(readxl)       # Version: packageVersion("readxl") - Reading Excel Files
library(dplyr)        # Version: packageVersion("dplyr") - Data Manipulation

# ------------------------------------------------------------------------------
# Load Dataset
# ------------------------------------------------------------------------------
organisation_dataset <- read.csv("IntOrg_NCD_variables_2024_02_02.csv")# Read dataset from CSV
head(organisation_dataset)  # Display first few rows of dataset

# Remove data from the year 2016, due to lack of sufficient and effective data
organisation_dataset <- organisation_dataset[organisation_dataset$Year != 2016, ]

# Check the number of distinct "Superregion" values
n_distinct(organisation_dataset$Superregion)  # Count unique Superregions

# ------------------------------------------------------------------------------
# BMI and Obesity Trends by Superregion
# ------------------------------------------------------------------------------

# Plot Mean BMI Trends for Children
ggplot(organisation_dataset, aes(x = Year, y = Mean_BMI_children, color = Superregion)) +
  geom_line(stat = "summary", fun = "mean") +  # Aggregates mean BMI for each year
  labs(title = "Mean BMI Trends for Children by Superregion", y = "Mean BMI", x = "Year") +
  theme_minimal()

# Plot Mean BMI Trends for Adults
ggplot(organisation_dataset, aes(x = Year, y = Mean_BMI_adults, color = Superregion)) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Mean BMI Trends for Adults by Superregion", y = "Mean BMI", x = "Year") +
  theme_minimal()

# Boxplot of Child Obesity Prevalence by Superregion
ggplot(organisation_dataset, aes(x = Superregion, y = Prevalence_obesity_children, fill = Superregion)) +
  geom_boxplot() +  # Shows distribution of obesity prevalence in children
  labs(title = "Obesity Prevalence for Children Across Superregions", y = "Obesity Prevalence", x = "Superregion") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot of Adult Obesity Prevalence by Superregion
ggplot(organisation_dataset, aes(x = Superregion, y = Prevalence_obesity_adults, fill = Superregion)) +
  geom_boxplot() +
  labs(title = "Obesity Prevalence for Adults Across Superregions", y = "Obesity Prevalence", x = "Superregion") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------------------------
# Blood Pressure and Diabetes Trends by Superregion
# ------------------------------------------------------------------------------

# Violin Plot: Distribution of Systolic Blood Pressure by Superregion
ggplot(organisation_dataset, aes(x = Superregion, y = Systolic_blood_pressure, fill = Superregion)) +
  geom_violin() +  # Violin plot to show density distribution of BP values
  labs(title = "Systolic Blood Pressure by Superregion", y = "Systolic BP", x = "Superregion") +
  theme_minimal() + 
  theme(axis.text = element_text(angle = 30, hjust = 1))  # Rotate x-axis labels for readability

# Line Plot: Diabetes Prevalence Over Time by Superregion
ggplot(organisation_dataset, aes(x = Year, y = Diabetes_prevalence, color = Superregion)) +
  geom_line(stat = "summary", fun = "mean") +  # Shows trends of diabetes prevalence over years
  labs(title = "Diabetes Prevalence Over Time by Superregion", y = "Diabetes Prevalence", x = "Year") +
  theme_minimal()

# ------------------------------------------------------------------------------
# Socioeconomic Factors and Health
# ------------------------------------------------------------------------------

# Scatter Plot: Child Obesity vs GDP by Superregion
ggplot(organisation_dataset, aes(x = GDP_USD, y = Prevalence_obesity_children, color = Superregion)) +
  geom_point(alpha = 0.6) +  # Alpha for transparency
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression line
  labs(title = "Child Obesity vs GDP by Superregion", y = "Obesity Prevalence", x = "GDP (USD)") +
  theme_minimal()

# Scatter Plot: Adult Obesity vs GDP by Superregion
ggplot(organisation_dataset, aes(x = GDP_USD, y = Prevalence_obesity_adults, color = Superregion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Adult Obesity vs GDP by Superregion", y = "Obesity Prevalence", x = "GDP (USD)") +
  theme_minimal()

# Scatter Plot: Years of Education vs. Obesity Prevalence (Adults)
ggplot(organisation_dataset, aes(x = Years_of_education, y = Prevalence_obesity_adults, color = Superregion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Obesity vs Education by Superregion", y = "Obesity Prevalence", x = "Years of Education") +
  theme_minimal()

# Scatter Plot: Western Diet Score vs Obesity Prevalence (Adults)
ggplot(organisation_dataset, aes(x = Western_diet_score, y = Prevalence_obesity_adults, color = Superregion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Western Diet Score vs Obesity", y = "Obesity Prevalence", x = "Western Diet Score") +
  theme_minimal()

# ------------------------------------------------------------------------------
# Child vs Adult Health Comparison
# ------------------------------------------------------------------------------

# Scatter Plot: Child vs Adult Obesity Prevalence
ggplot(organisation_dataset, aes(x = Prevalence_obesity_children, y = Prevalence_obesity_adults, color = Superregion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Child vs Adult Obesity Prevalence", y = "Adult Obesity", x = "Child Obesity") +
  theme_minimal()

# Scatter Plot: Underweight Prevalence - Children vs Adults
ggplot(organisation_dataset, aes(x = Prevalence_underweight_children, y = Prevalence_underweight_adults, color = Superregion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Underweight: Children vs Adults", y = "Adult Underweight", x = "Child Underweight") +
  theme_minimal()

# ------------------------------------------------------------------------------
# End of Script
# ------------------------------------------------------------------------------

