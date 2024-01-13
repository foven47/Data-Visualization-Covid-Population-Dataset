setwd("C:/Users/muham/OneDrive - Universiti Sains Malaysia/USM/Computer Science/Year 3/Year 3 Semester 1/CPC 351 CPM 351/Assignments/Assignment 2")
file_path <- "covid_cases_age.csv"
covid_data <- read.csv(file_path)

str(covid_data)

# Display the first few rows of the dataset
head(covid_data)

#DATA PREPROCESSING AND CLEANING

# Check for missing values
sum(is.na(covid_data))

# Check for duplicate rows
covid_data[duplicated(covid_data), ]

# Convert 'date' to Date type
covid_data$date <- as.Date(covid_data$date)

# Factor state
covid_data$state <- as.factor(covid_data$state)

summary(covid_data)

#QUESTION 7

# Loading package
library(ggplot2)
library(dplyr)

# Filter data for Penang and extract years 2020 to 2023
covid_Penang_20_23 <- covid_data[format((covid_data$date), "%Y") > "2019" & covid_data$state == "Pulau Pinang", ]


# Group by year and month and sum the total cases across all age groups
monthly_cases_all_years <- summarise(group_by(covid_Penang_20_23, year = format(date, "%Y"), month = format(date, "%m")),
                                     total_cases = sum(cases_child + cases_adolescent + cases_adult + cases_elderly))

# Factor the month
monthly_cases_all_years$month <- as.factor(monthly_cases_all_years$month)

# Convert month to factor and in month name
monthly_cases_all_years$month <- month.abb[monthly_cases_all_years$month]

# Put month to in order
monthly_cases_all_years$month = factor(monthly_cases_all_years$month, levels = month.abb)

summary(monthly_cases_all_years)

# Create a bar plot for Cases by Month for the Years 2020-2023
ggplot(monthly_cases_all_years, aes(x = factor(month), y = total_cases, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = total_cases),
    position = position_dodge(width = 1),
    vjust = 0,
    angle = 90,
    size = 2,
    hjust = -0.1
  ) +
  labs(title = "Cases by Month for the Years 2020-2023",
       x = "Month",
       y = "Cases",
       fill = "Year") +
  theme_minimal()
