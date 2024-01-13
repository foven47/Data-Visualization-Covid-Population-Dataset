
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

#filter data to remove overall_ethnicity
population_data_filtered <- subset(population_data, !(ethnicity=="overall_ethnicity"))
summary(population_data_filtered)

# Create new variables for overall categories
population_data_overall_sex <- population_data_filtered[population_data_filtered$sex == "overall_sex", ]
population_data_overall_age <- population_data_filtered[population_data_filtered$age == "overall_age", ]

# Aggregate the data with overall categories
new_population_overall <- aggregate(cbind(population_data_filtered$population) ~
                                      population_data_overall_sex +
                                      population_data_overall_age ,
                                    data = population_data_filtered, FUN = sum)

# Filter data for year 2020
ethnicity_composition_20 <- population_data_filtered[format((population_data_filtered$date),"%Y") == "2020",]
# Filter data for year 2021
ethnicity_composition_21 <- population_data_filtered[format((population_data_filtered$date),"%Y") == "2021",]
# Filter data for year 2022
ethnicity_composition_22 <- population_data_filtered[format((population_data_filtered$date),"%Y") == "2022",]
# Filter data for year 2021
ethnicity_composition_23 <- population_data_filtered[format((population_data_filtered$date),"%Y") == "2023",]
  
  # Create a bar plot for year 2020
  ggplot(ethnicity_composition_20, aes(y = population, x = ethnicity, fill = ethnicity)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7)+
    geom_text(
      aes(label = population),
      position = position_dodge(width = 1),
      vjust = 0.5,
      angle = 90,
      size = 3,
      hjust = -0.1
    ) +
    labs(title = paste("Ethnicity Composition in year 2020"),
         x = "Ethnicity",
         y = "Population ('000')") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Create a bar plot for year 2021
  ggplot(ethnicity_composition_21, aes(y = population, x = ethnicity, fill = ethnicity)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7)+
    geom_text(
      aes(label = population),
      position = position_dodge(width = 1),
      vjust = 0.5,
      angle = 90,
      size = 3,
      hjust = -0.1
    ) +
    labs(title = paste("Ethnicity Composition in year 2021"),
         x = "Ethnicity",
         y = "Population ('000')") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Create a bar plot for year 2022
  ggplot(ethnicity_composition_22, aes(y = population, x = ethnicity, fill = ethnicity)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7)+
    geom_text(
      aes(label = population),
      position = position_dodge(width = 1),
      vjust = 0.5,
      angle = 90,
      size = 3,
      hjust = -0.1
    ) +
    labs(title = paste("Ethnicity Composition in year 2022"),
         x = "Ethnicity",
         y = "Population ('000')") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Create a bar plot for year 2023
  ggplot(ethnicity_composition_23, aes(y = population, x = ethnicity, fill = ethnicity)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7)+
    geom_text(
      aes(label = population),
      position = position_dodge(width = 1),
      vjust = 0.5,
      angle = 90,
      size = 3,
      hjust = -0.1
    ) +
    labs(title = paste("Ethnicity Composition in year 2023"),
         x = "Ethnicity",
         y = "Population ('000')") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))




