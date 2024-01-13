
#install library ggplot2 to visualize data
install.packages("ggplot2")
install.packages("viridis")
library(ggplot2)
library(viridis)

#set working directory
setwd("C:/Users/hazira/Desktop/assignment2_351")
#import csv file
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
#summary of the data
summary(covid_data)

#filter data for year 2021
covid_data_21 <- covid_data[format((covid_data$date),"%Y")=="2021",]
covid_data_21_filter <- subset(covid_data_21, !(state=="Malaysia"))

# Sum the cases for each state and age group in the year 2021
total_cases_by_state <- aggregate(cbind(cases_child, cases_adolescent, cases_adult, cases_elderly) ~ state, 
                                  data = covid_data_21_filter, 
                                  FUN = sum)

# Print the summary of total cases by state
print(total_cases_by_state)

# Visualize the total cases by state in a bar plot
ggplot(total_cases_by_state, aes(x = reorder(state, -(cases_child + cases_adolescent + cases_adult + cases_elderly)), y = cases_child + cases_adolescent + cases_adult + cases_elderly, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") + scale_y_continuous(labels = scales::comma) +
  labs(title = "Total COVID-19 Cases by State in 2021",
       x = "State",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))


# Sort the total_cases_by_state data frame by total cases in descending order
sorted_data <- total_cases_by_state[order(-(total_cases_by_state$cases_child + total_cases_by_state$cases_adolescent + total_cases_by_state$cases_adult + total_cases_by_state$cases_elderly)), ]

# Select the top 3 states
top_3_states <- head(sorted_data, 3)

# Print the top 3 states
print(top_3_states)
# Set pastel colors
pastel_colors <- c("#FFB6C1", "#ADD8E6", "#C8A2C8")

# Visualize top 3 state with highest cases 
ggplot(top_3_states, aes(x = "", y = cases_child + cases_adolescent + cases_adult + cases_elderly, fill = state)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = pastel_colors) +  # You can customize the color palette
  labs(title = "Top 3 States with Highest COVID-19 Cases in 2021",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(label = scales::comma(cases_child + cases_adolescent + cases_adult + cases_elderly)), position = position_stack(vjust = 0.5))

