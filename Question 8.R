# Set the working directory
setwd("C:/Users/MUSLIHIN/OneDrive - Universiti Sains Malaysia/Desktop/Assignment2_351")

# Load the data from the CSV file
covid_data <- read.csv("covid_cases_age.csv")

#DATA PREPROCESSING AND CLEANING
# Check for missing values
sum(is.na(covid_data))

# Check for duplicate rows
covid_data[duplicated(covid_data), ]

# Convert 'date' to Date type
covid_data$date <- as.Date(covid_data$date)

# Factor state
covid_data$state <- as.factor(covid_data$state)

# Create a new column for year
covid_data$year <- format(covid_data$date, "%Y")

# Create a new column for month
covid_data$month <- format(covid_data$date, "%Y-%m")

# Check for missing values
summary(covid_data)

#Question 8

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Filter data for the year 2020-2023
covid_data_2020 <- filter(covid_data, year == "2020")
covid_data_2021 <- filter(covid_data, year == "2021")
covid_data_2022 <- filter(covid_data, year == "2022")
covid_data_2023 <- filter(covid_data, year == "2023")


# Aggregate data by month and age category for the year 2020
monthly_totals_2020 <- covid_data_2020 %>%
  group_by(month) %>%
  summarise(
    total_child = sum(cases_child),
    total_adolescent = sum(cases_adolescent),
    total_adult = sum(cases_adult),
    total_elderly = sum(cases_elderly)
  )

# Aggregate data by month and age category for the year 2021
monthly_totals_2021 <- covid_data_2021 %>%
  group_by(month) %>%
  summarise(
    total_child = sum(cases_child),
    total_adolescent = sum(cases_adolescent),
    total_adult = sum(cases_adult),
    total_elderly = sum(cases_elderly)
  )

# Aggregate data by month and age category for the year 2022
monthly_totals_2022 <- covid_data_2022 %>%
  group_by(month) %>%
  summarise(
    total_child = sum(cases_child),
    total_adolescent = sum(cases_adolescent),
    total_adult = sum(cases_adult),
    total_elderly = sum(cases_elderly)
  )

# Aggregate data by month and age category for the year 2023
monthly_totals_2023 <- covid_data_2023 %>%
  group_by(month) %>%
  summarise(
    total_child = sum(cases_child),
    total_adolescent = sum(cases_adolescent),
    total_adult = sum(cases_adult),
    total_elderly = sum(cases_elderly)
  )



# Reshape data for plotting
monthly_totals_2020_long <- monthly_totals_2020 %>%
  pivot_longer(cols = starts_with("total_"), names_to = "Age_Category", values_to = "Total_Cases")

# Reshape data for plotting
monthly_totals_2021_long <- monthly_totals_2021 %>%
  pivot_longer(cols = starts_with("total_"), names_to = "Age_Category", values_to = "Total_Cases")

# Reshape data for plotting
monthly_totals_2022_long <- monthly_totals_2022 %>%
  pivot_longer(cols = starts_with("total_"), names_to = "Age_Category", values_to = "Total_Cases")

# Reshape data for plotting
monthly_totals_2023_long <- monthly_totals_2023 %>%
  pivot_longer(cols = starts_with("total_"), names_to = "Age_Category", values_to = "Total_Cases")



# Create visualizations for the year 2020 using a grouped bar chart
ggplot(monthly_totals_2020_long, aes(x = month, y = Total_Cases, fill = Age_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "COVID-19 Cases in Malaysia for the year 2020 by Age Category",
    x = "Month",
    y = "Total Cases"
  ) +
  scale_x_discrete(labels = month.name) +  # Display only month names
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create visualizations for the year 2021 using a grouped bar chart
ggplot(monthly_totals_2021_long, aes(x = month, y = Total_Cases, fill = Age_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "COVID-19 Cases in Malaysia for the year 2021 by Age Category",
    x = "Month",
    y = "Total Cases"
  ) +
  scale_x_discrete(labels = month.name) +  # Display only month names
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create visualizations for the year 2022 using a grouped bar chart
ggplot(monthly_totals_2022_long, aes(x = month, y = Total_Cases, fill = Age_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "COVID-19 Cases in Malaysia for the year 2022 by Age Category",
    x = "Month",
    y = "Total Cases"
  ) +
  scale_x_discrete(labels = month.name) +  # Display only month names
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create visualizations for the year 2023 using a grouped bar chart
ggplot(monthly_totals_2023_long, aes(x = month, y = Total_Cases, fill = Age_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "COVID-19 Cases in Malaysia for the year 2023 by Age Category",
    x = "Month",
    y = "Total Cases"
  ) +
  scale_x_discrete(labels = month.name) +  # Display only month names
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
