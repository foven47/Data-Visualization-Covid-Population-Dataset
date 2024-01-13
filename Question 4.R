# Set the working directory
setwd("C:/Users/MUSLIHIN/OneDrive - Universiti Sains Malaysia/Desktop/Assignment2_351")

# Load the data from the CSV file
population_data <- read.csv("population_state.csv")

#data cleaning
#get first 6 row of the data
head(population_data)
#summary of the data
summary(population_data)

#check missing values
sum(is.na(population_data))

#check for duplicate rows
population_data[duplicated(population_data)]

#factor the character type
population_data$sex <- as.factor(population_data$sex)
population_data$age <- as.factor(population_data$age)
population_data$state <- as.factor(population_data$state)
population_data$ethnicity <- as.factor(population_data$ethnicity)

#convert 'date' to datatype
population_data$date <- as.Date(population_data$date)

#Question 4

# Load necessary libraries
library(ggplot2)
library(tidyverse)

# Filter data for Perak and age group 0-4
perak_age_0_4 <- population_data %>%
  filter(state == "Perak", age == "0-4")

# Calculate the overall percentage in the number of people in this age group from 2020 to 2023 in Perak
overall_percentage <- ((sum(perak_age_0_4$population[year(perak_age_0_4$date) == 2023]) -
                          sum(perak_age_0_4$population[year(perak_age_0_4$date) == 2020])) /
                         sum(perak_age_0_4$population[year(perak_age_0_4$date) == 2020])) * 100

# Visualize the data using a grouped bar chart
ggplot(perak_age_0_4, aes(x = date, y = population, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = ifelse(sex == "overall_sex" & ethnicity == "overall_ethnicity", 
                               sprintf("%.1f", population), "")),
            position = position_dodge(width = 0.9), vjust = -0.5, hjust = -0.5, size = 3) +
  labs(title = paste("Population in Perak (Age 0-4) from 2020 to 2023",
                     "\nOverall Percentage Change: ", sprintf("%.2f%%", overall_percentage)),
       x = "Year",
       y = "Population ('000)",
       fill = "Sex") +
  theme_minimal()
