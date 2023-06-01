# Exploratory Data Analysis on several data sets


# Load packages

library(tidyverse)   
library(ggplot2)     
library(dplyr)
library(zoo)
library(readxl)


# Load Data into variable
daily <- read_excel('daily_tasks_data.xlsx')

# Looking at daily tasks data
View(daily)
head(daily)
dim(daily)

mean(daily$tbi_caffeine)
sd(daily$add_placebo)
median(daily$tbi_ritalin)

?t.test
# Dependent t-test
t.test(
  x = daily$tbi_caffeine,
  y = daily$tbi_ritalin,
  alternative = 'two.sided',
  paired = TRUE
)
# Independent t-test
t.test(
  x = daily$add_placebo,
  y = daily$tbi_placebo,
  alternative = 'two.sided',
  paired = FALSE
)

# ADD w/ Ritalin

library(ggplot2)

ggplot(data = daily, aes(x = add_ritalin)) +
  geom_histogram(binwidth = 2, fill = 'skyblue', color = 'black') +
  labs(x = " Daily Tasks: ADD_Ritalin", y = 'Frequency') +
  theme_classic()

# Import final_practice data set \
practice <- read_excel('final_practice_data.xlsx')

# Looking at Data
practice
View(practice)
head(practice)

ncol(practice)
colnames(practice)
nrow(practice)
dim(practice)

# Descriptive Stats

mean(practice$tbi_digits)
mean(practice$tbi_words)
median(practice$tbi_digits)
median(practice$tbi_words)
sd(practice$tbi_digits)
sd(practice$tbi_words)


?t.test
t.test(
  x = practice$control_letters,
  y = practice$tbi_letters,
  alternative = 'two.sided',
  paired = FALSE
)


t.test(
  x = practice$tbi_letters,
  y = practice$tbi_words,
  alternative = 'two.sided',
  paired = TRUE
)

library(ggplot2)

ggplot(data = practice, aes(x = tbi_digits)) +
  geom_histogram(binwidth = 1, fill = 'skyblue', color = 'black') +
  labs(x = " WM Capacity: TBI", y = 'Frequency') +
  theme_classic()


# Import Covid-19 Data sets
covid_test <- read_csv("COVID19_line_list_data.csv")
covid_data <- read_csv("Covid-19 data.csv")

View(covid_test)
View(covid_data)

# Structure and Summary of Data sets

str(covid_test)
str(covid_data)

summary(covid_test)
summary(covid_data)

# Working with the Covid_test data set
install.packages("Hmisc")
library(Hmisc)

# describe data
describe(covid_test)

# cleaned up death column
covid_test$death_dummy <- as.integer(covid_test$death != 0)

# death rate
sum(covid_test$death_dummy) / nrow(covid_test)

# AGE:
# Claim: Are people who die are typically older?
dead <- subset(covid_test, death_dummy == 1)
alive <- subset(covid_test, death_dummy == 0)

mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# Is this statistically significant
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.95)
# Normally, p-value < 0.05, we reject the null hypothesis 
# Here, p-value is 2.2e-16 (~ 0) so we reject the null hypothesis and
#   conclude that this is statistically significant


# GENDER:
# Claim: Are men more likely to die than women?
men <- subset(covid_test, gender == 'male')
women <- subset(covid_test, gender == 'female')

mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

# Is this statistically significant
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", 
       conf.level = 0.95)
# Normally, p-value < 0.05, we reject the null hypothesis 
# Here, p-value is 0.002105 so we reject the null hypothesis and
#   conclude that men have a higher death rate than women which is 
#   statistically significant


# Working with the Covid_Data data set

summary(covid_data$total_cases)
summary(covid_data$total_deaths)

# Rolling average of cases
covid_data$rolling_avg <- rollmean(covid_data$total_cases, k = 7, fill = NA)
covid_data$rolling_avg

# Cases by Country
country_cases <- covid_data %>%
  group_by(location) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE))

# Cases by Continent
continent_cases <- covid_data %>%
  group_by(continent) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE))

# Total Cases and Deaths by country + continent

cases_by_country <- covid_data %>% 
  group_by(location) %>% 
  summarise(total_cases = sum(total_cases, na.rm = T),
            total_deaths = sum(total_deaths, na.rm = T))

cases_by_country

cases_by_continent <- covid_data %>% 
  filter(total_cases != 'NA' & total_deaths != 'NA' & continent != 'NA') %>% 
  group_by(continent) %>% 
  summarise(total_cases = sum(total_cases),
            total_deaths = sum(total_deaths))

cases_by_continent

# Bar Plot of total cases by continent

ggplot(cases_by_continent, aes(x = continent, y = total_cases, fill = continent)) +
  geom_bar(stat = 'identity') +
  labs(x = "Continent", y = "Total Cases") +
  theme_minimal()

?ggplot

