#install library ggplot2 to visualize data
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

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

#----------------------BASED ON ETHNICITY-----------------------------------------------------------------

#filter overall ethnicity in ethnic
penang_data_20_23_ethnic <- subset(penang_data_20_23, !(ethnicity == "overall_ethnicity"))

#filter data to take only row with overall_sex and overall_age
penang_data_20_23_ethnic <- penang_data_20_23_ethnic[penang_data_20_23_ethnic$sex == "overall_sex" & penang_data_20_23_ethnic$age == "overall_age",]

# Group by year and ethnicity and sum the population of all state based on ethincity
penang_data_20_23_ethnic <- summarise(group_by(penang_data_20_23_ethnic, date, ethnicity ),
                                      population = sum(population))

# Bar plot for ethnicity
ggplot(penang_data_20_23_ethnic, aes(x = as.factor(format(date,"%Y")),y = population, fill = ethnicity)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Population in Penang for year 2020-2023 based on Ethnicity",
       x = "Year",
       y = "Population ('000)",
      fill = "ethnicity")+
  theme_minimal()

#----------------------BASED ON GENDER-----------------------------------------------------------------
#filter overall gender in gender
penang_data_20_23_gender <- subset(penang_data_20_23, !(sex == "overall_sex"))

#filter data to take only row with overall_ethnicity and overall_age
penang_data_20_23_gender <- penang_data_20_23_gender[penang_data_20_23_gender$ethnicity == "overall_ethnicity" & penang_data_20_23_gender$age == "overall_age",]

# Group by year and ethnicity and sum the population of all state based on ethincity
penang_data_20_23_gender <- summarise(group_by(penang_data_20_23_gender, date, sex ),
                                      population = sum(population))

# Bar plot for gender
ggplot(penang_data_20_23_gender, aes(x = as.factor(format(date,"%Y")),y = population, fill = sex)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Population in Penang for year 2020-2023 based on Gender",
       x = "Year",
       y = "Population ('000)",
       fill = "sex")+
  theme_minimal()

#----------------------BASED ON AGE GROUP-----------------------------------------------------------------
#filter overall gender in gender
penang_data_20_23_age <- subset(penang_data_20_23, !(age == "overall_age"))

#filter data to take only row with overall_ethnicity and overall_age
penang_data_20_23_age <- penang_data_20_23_age[penang_data_20_23_age$ethnicity == "overall_ethnicity" & penang_data_20_23_age$sex == "overall_sex",]

# Group by year and ethnicity and sum the population of all state based on ethincity
penang_data_20_23_age <- summarise(group_by(penang_data_20_23_age, date, age ),
                                      population = sum(population))

# Box plot for age group
ggplot(penang_data_20_23_age, aes(x = as.factor(format(date,"%Y")),y = population, fill = age)) +
  geom_bar(stat="identity",position = "dodge") +
  labs(title = "Population in Penang for year 2020-2023 based on Age group",
       x = "Year",
       y = "Population ('000)",
       fill = "age")+
  theme_minimal()
