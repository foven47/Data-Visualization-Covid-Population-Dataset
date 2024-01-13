#install library ggplot2 to visualize data
install.packages("ggplot2")
library(ggplot2)

setwd("C:/Users/hazira/Desktop/assignment2_351")

population_data <- read.csv("population_state.csv")

#DATA PREPROCESSING AND CLEANING
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

# Filter data for Penang and the specified years and remove overall ethnicity, overall sex and overall age.
penang_data_20_23 <- population_data[format((population_data$date), "%Y")>"2019" & population_data$state == "Pulau Pinang",]
penang_data_20_23_filtered <- subset(penang_data_20_23, !(ethnicity == "overall_ethnicity" | sex == "overall_sex" | age == "overall_age"))
summary(penang_data_20_23_filtered)
summary(penang_data_20_23)

# Bar plot for ethnicity
ggplot(penang_data_20_23_filtered, aes(x = as.factor(format(date,"%Y")),y = population, fill = ethnicity)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Population in Penang for year 2020-2023 based on Ethnicity",
       x = "Year",
       y = "Population ('000)",
      fill = "ethnicity")+
  theme_minimal()

# Bar plot for gender
ggplot(penang_data_20_23_filtered, aes(x = as.factor(format(date,"%Y")),y = population, fill = sex)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Population in Penang for year 2020-2023 based on Gender",
       x = "Year",
       y = "Population ('000)",
       fill = "sex")+
  theme_minimal()

# Box plot for age group
ggplot(penang_data_20_23_filtered, aes(x = as.factor(format(date,"%Y")),y = population, fill = age)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Population in Penang for year 2020-2023 based on Age group",
       x = "Year",
       y = "Population ('000)",
       fill = "age")+
  theme_minimal()
