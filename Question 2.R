library(readxl)
library(dplyr)
library(ggplot2)

#set working directory (insert your working directory here)
setwd("")

#load cs file
population_state <- read.csv("population_state.csv")

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

# Aggregate data by state to get the total population of females in the specified age range
agg_data <- clean_data %>%
  group_by(state) %>%
  summarise(total = sum(population))

# Identify the top 10 states with the highest numbers of females in the age range of 15 to 29
top_states <- agg_data %>%
  arrange(desc(total)) %>%
  head(10)

# Visualization using ggplot2
ggplot(top_states, aes(x = reorder(state, -total), y = total)) + geom_bar(stat = "identity", fill = "green") +
  labs(title = "Top 10 States with Highest Number of Female (15-29)", x = "State", y = "Population ('000)") + theme_minimal()