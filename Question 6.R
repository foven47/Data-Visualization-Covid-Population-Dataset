install.packages("tidyr")
library(dplyr)
library(tidyr)
library(ggplot2)

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
