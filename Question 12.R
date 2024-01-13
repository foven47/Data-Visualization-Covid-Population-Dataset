# Set the working directory
setwd("C:/Users/MUSLIHIN/OneDrive - Universiti Sains Malaysia/Desktop/Assignment2_351")

# Load the data from the CSV file
covid_data <- read.csv("covid_cases_age.csv")

# DATA PREPROCESSING AND CLEANING
# Check for missing values
sum(is.na(covid_data))

# Check for duplicate rows
covid_data[duplicated(covid_data), ]

# Convert 'date' to Date type
covid_data$date <- as.Date(covid_data$date)

# Factor state
covid_data$state <- as.factor(covid_data$state)

# Explore the structure and summary of the dataset
str(covid_data)
summary(covid_data)

#Question 12

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate and visualize the total cases by state
total_cases_by_state <- covid_data %>%
  group_by(state) %>%
  summarise(total_cases = sum(cases_child + cases_adolescent + cases_adult + cases_elderly + 
                                cases_0_4 + cases_5_11 + cases_12_17 + cases_18_29 +
                                cases_30_39 + cases_40_49 + cases_50_59 + cases_60_69 +
                                cases_70_79 + cases_80))

# Exclude "Malaysia" from the results
total_cases_by_state <- total_cases_by_state %>%
  filter(state != "Malaysia")

# Plot total COVID-19 cases by state in Malaysia
ggplot(total_cases_by_state, aes(x = reorder(state, -total_cases), y = total_cases, fill = state)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total COVID-19 Cases Across States in Malaysia",
    x = "State",
    y = "Total Cases"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Business Decision Analysis
# Identify states with the highest total cases and allocate more healthcare resources to those areas.
# Select the top 3 states with the highest total cases
high_risk_states <- total_cases_by_state %>%
  arrange(desc(total_cases)) %>%
  head(3)

print("Allocate more healthcare resources to the following high-risk states:")
print(high_risk_states)
