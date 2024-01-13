library(readxl)
library(dplyr)
library(ggplot2)

# Data preprocessing and cleaning
clean_data <- population_state %>%
  filter(sex == "female", age %in% c("15-19", "20-24", "25-29"))

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