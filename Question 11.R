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

#QUESTION 11

# Loading package
library(ggplot2)
library(dplyr)

summary(covid_data)

# Filter data for Malaysia and extract years 2020 to 2023
covid_Malaysia_20_23 <- covid_data[format((covid_data$date), "%Y") > "2019" & covid_data$state == "Malaysia", ]


# Create a new dataset of total covid cases of child+adolescent and adult in Malaysia from 2020 to 2023
new_covid_Malaysia_20_23 <- data.frame(
  age_group = c("child_adolescent","adult"),
  value = c(sum(covid_Malaysia_20_23$cases_child + covid_Malaysia_20_23$cases_adolescent),sum(covid_Malaysia_20_23$cases_adult))
)


#create pie chart for COVID-19 Cases of Child+Adloescent and Adult in Malaysia (2020-2023)
ggplot(new_covid_Malaysia_20_23, aes(x="", y=value, fill=age_group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = scales::percent(value / sum(value))), position = position_stack(vjust=0.5)) +
  labs(title = "COVID-19 Cases of Child+Adloescent and Adult in Malaysia (2020-2023)", x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
