library(ggplot2)

#set working directory (your working directory here)
setwd("")
#load csv file
covid_cases_age <- read.csv("covid_cases_age.csv")

# Convert the 'date' column to Date type
covid_cases_age$date <- as.Date(covid_cases_age$date)

# Take out the year from the 'date' column
covid_cases_age$year <- as.numeric(format(covid_cases_age$date, "%Y"))

# Calculate the total cases for each state by year
totalcases <- aggregate(. ~ state + year, data = covid_cases_age, sum)


# Function to get the overall states with the lowest adult cases for a specific year
states <- function(year)
{
  covid_cases_age_year <- totalcases[totalcases$year == year,]
  covid_cases_age_year <- covid_cases_age_year[order(covid_cases_age_year$cases_adult),]
  return(covid_cases_age_year)
}

# Create a data frame for the top 5 states for each year
result_covid_cases_age <- do.call(rbind, lapply(2020:2023, function(year) states(year)))

# Create a bar plot using ggplot2
ggplot(result_covid_cases_age, aes(x = state, y = cases_adult, fill = as.factor(year))) + geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Adult COVID-19 Cases (2020-2023)", x = "State", y = "Total Adult Cases") + theme_minimal()
