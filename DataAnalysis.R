# ==============================================================================
# Age and Education Differences in Climate Change Concerns
# by Ricarda Haslinger

# How are people’s concerns on climate change associated to age and educational 
#  attainment in Australia?
# Department of Demography, University of Vienna
# ==============================================================================


# ==============================================================================
# 000: Housekeeping
# ==============================================================================


# Remove all existing objects from the workspace
rm(list = ls())
# Clear console
cat("\014")

# Load required packages
library(readxl)
library(tidyverse)


# ==============================================================================
# 001: Load and prepare data
# ==============================================================================


data <- read_excel("Peoples_Climate_Vote_Database_2024.xlsx")

# Quick look
head(data)
dim(data)
colnames(data)

# Convert weighted mean to numeric
data <- data %>%
  mutate(`Weighted Mean` = as.numeric(`Weighted Mean`))

# Select Australia and relevant question
aus_data <- data %>%
  filter(
    Country == "Australia",
    `Question Text` == "How worried are you about the effects of climate change on the next generation?"
  )

# Check filtered dataset
head(aus_data)
dim(aus_data)
unique(aus_data$Age)
unique(aus_data$Education)
unique(aus_data$Response)
unique(aus_data$Category)

# Inspect demographic identifiers
aus_data %>%
  distinct(AID, Age, EID, Education) %>%
  arrange(AID, EID)

# Check missing values
colSums(is.na(aus_data))


# ==============================================================================
# 002: Exploratory data analysis
# ==============================================================================


# Differences across age groups ================================================

age_data <- aus_data %>%
  filter(
    Education == "All Education",
    Age != "All Ages",
    !is.na(`Weighted Mean`)
  ) %>%
  mutate(
    Age = factor(
      Age,
      levels = c("Under 18", "18 to 35", "36 to 59", "60 plus")
    )
  )

# Check available age groups
age_data %>%
  distinct(AID, Age)

# Check weighted percentages
age_data %>%
  group_by(Age) %>%
  summarise(
    Total_Percentage = sum(`Weighted Mean`),
    .groups = "drop"
  )

# Descriptive statistics
age_data %>%
  select(Age, Response, `Weighted Mean`) %>%
  arrange(Age, Response)

response_order <- c("Extremely", "Very", "Somewhat", "Not very", "Not at all",
                    "Don't know")

age_data <- age_data %>%
  mutate(Response = factor(Response, levels = response_order))

# Visualisation
ggplot(
  age_data,
  aes(x = Age, y = `Weighted Mean`, fill = Response)
) +
  geom_col(position = position_stack(reverse = FALSE)) +
  labs(
    title = "Concern about Climate Change by Age Group",
    subtitle = "Australia - Peoples' Climate Vote 2024",
    x = "Age Group",
    y = "Weighted percentage (%)",
    fill = "Response"
  ) +
  theme_minimal()

# Differences across education levels ==========================================

education_data <- aus_data %>%
  filter(
    Age == "All Ages",
    Education != "All Education",
    !is.na(`Weighted Mean`)
  ) %>%
  mutate(
    Education = factor(
      Education,
      levels = c("Under 12", "12 to 19", "20 or over")
    )
  )

# Check available education groups
education_data %>%
  distinct(EID, Education)

# Check weighted percentages
education_data %>%
  group_by(Education) %>%
  summarise(
    Total_Percentage = sum(`Weighted Mean`),
    .groups = "drop"
  )

# Descriptive statistics
education_data %>%
  select(Education, Response, `Weighted Mean`) %>%
  arrange(Education, Response)

education_data <- education_data %>%
  mutate(Response = factor(Response, levels = response_order))

# Visualisation
ggplot(
  education_data,
  aes(x = Education, y = `Weighted Mean`, fill = Response)
) +
  geom_col(position = position_stack(reverse = FALSE)) +
  labs(
    title = "Concern about Climate Change by Education Level",
    subtitle = "Australia - Peoples' Climate Vote 2024",
    x = "Education Level",
    y = "Weighted percentage (%)",
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
