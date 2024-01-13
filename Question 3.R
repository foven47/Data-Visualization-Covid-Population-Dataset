setwd("C:/Users/muham/OneDrive - Universiti Sains Malaysia/USM/Computer Science/Year 3/Year 3 Semester 1/CPC 351 CPM 351/Assignments/Assignment 2")
file_path <- "population_state.csv"
population_data <- read.csv(file_path)

str(population_data)

# Display the first few rows of the dataset
head(population_data)

#data cleaning

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




#QUESTION 3

# Loading package
library(ggplot2)

summary(population_data)

# Filter data for age group of 80+ with different sex, overall etnicity and extract years 2020 to 2023
population_age80_20_23 <- population_data[format((population_data$date), "%Y") > "2019" &
                          (population_data$age == "80-84" | population_data$age == "85+") &
                          (population_data$sex != "overall_sex") &
                          (population_data$ethnicity == "overall_ethnicity"), ]

#convert 'date' value format to Year
population_age80_20_23$date <-  format(population_age80_20_23$date, "%Y")

# Sum the total cases for all age groups of 80+
new_population_age80_20_23 <- aggregate(cbind(population_age80_20_23$population) ~  population_age80_20_23$date
                                        + population_age80_20_23$state + population_age80_20_23$sex
                                        , data = population_age80_20_23, sum)
# Assign meaningful column names
colnames(new_population_age80_20_23) <- c("Year", "State", "Sex", "Population")

# Filter data for year 2020
population_age80_20 <- new_population_age80_20_23[new_population_age80_20_23$Year == "2020",]
# Filter data for year 2021
population_age80_21 <- new_population_age80_20_23[new_population_age80_20_23$Year == "2021",]
# Filter data for year 2022
population_age80_22 <- new_population_age80_20_23[new_population_age80_20_23$Year == "2022",]
# Filter data for year 2021
population_age80_23 <- new_population_age80_20_23[new_population_age80_20_23$Year == "2023",]

# Create a bar plot for year 2020
ggplot(population_age80_20, aes(x = State, y = Population, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Population),
    position = position_dodge(width = 1),
    vjust = 0.5,
    angle = 90,
    size = 3,
    hjust = -0.1
  ) +
  labs(title = "Population by state for age group 80+ (2020)",
       x = "State",
       y = "Population ('000)",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Create a bar plot for year 2021
ggplot(population_age80_21, aes(x = State, y = Population, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Population),
    position = position_dodge(width = 1),
    vjust = 0.5,
    angle = 90,
    size = 3,
    hjust = -0.1
  ) +
  labs(title = "Population by state for age group 80+ (2021)",
       x = "State",
       y = "Population ('000)",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Create a bar plot for year 2022
ggplot(population_age80_20, aes(x = State, y = Population, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Population),
    position = position_dodge(width = 1),
    vjust = 0.5,
    angle = 90,
    size = 3,
    hjust = -0.1
  ) +
  labs(title = "Population by state for age group 80+ (2022)",
       x = "State",
       y = "Population ('000)",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Create a bar plot for year 2023
ggplot(population_age80_21, aes(x = State, y = Population, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = Population),
    position = position_dodge(width = 1),
    vjust = 0.5,
    angle = 90,
    size = 3,
    hjust = -0.1
  ) +
  labs(title = "Population by state for age group 80+ (2023)",
       x = "State",
       y = "Population ('000)",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



