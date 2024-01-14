install.packages("tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)

<<<<<<< Updated upstream
#set working directory (insert your working directory here)
setwd("")

#load cs file
population_state <- read.csv("population_state.csv")
=======
# Data preprocessing and cleaning
# Get first 6 row of data
head(population_state)
# Summary of the data
summary(population_state)
# Check for missing values
sum(is.na(population_state))
# Check for duplicated rows
population_state[duplicated(population_state),]
# Factor the character type
population_state$sex <- as.factor(population_state$sex)
population_state$age <- as.factor(population_state$age)
population_state$state <- as.factor(population_state$state)
population_state$ethnicity <- as.factor(population_state$ethnicity)
# Convert 'date' to datatype
population_state$date <- as.Date(population_state$date)
>>>>>>> Stashed changes

# Filter out unnecessary rows (e.g., overall_sex, overall_ethnicity)
clean_data <- population_state %>%
  filter(!grepl("overall", sex) & !grepl("overall", ethnicity) & age != "overall_age")

# Convert date to a Date object
clean_data$date <- as.Date(clean_data$date, format = "%m/%d/%Y")

# Aggregate data to get the total population by state, sex, and age
mass_data <- clean_data %>%
  group_by(state, sex, age) %>%
  summarise(total_population = sum(population))

# Pivot the data for better analysis
pivot_data <- mass_data %>%
  pivot_wider(names_from = sex, values_from = total_population)

# Calculate the total population for each age group
pivot_data <- pivot_data %>%
  mutate(total_population = male + female)


ggplot(pivot_data, aes(x = age, y = total_population, fill = state)) + geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Malaysia Population ", x = "Age Range", y = "Population ('000)") + theme_minimal()
