# Exploratory Data Analysis: COVID-19 

# Install packages

install.packages(c("tidyverse", "ggplot2", "dplyr", "gganimate", "gapminder", 
                   "ggthemes", "gifski"))

# Load packages

library(tidyverse)   
library(ggplot2)     
library(dplyr)       
library(gganimate)
library(gapminder)
library(ggthemes)
library(gifski)
library(zoo)

# Import Data sets

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

# Bar Plot of total cases by  continent

ggplot(cases_by_continent, aes(x = continent, y = total_cases)) +
  geom_bar(stat = 'identity') +
  labs(x = "Continent", y = "Total Cases") +
  theme_minimal()

# Graph 1: Gapminder static plot

gapminder

graph1 = gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Life Expectancy vs GDP Per Capita by Country",
       x = "Income per person (GDP / capita)",
       y = "Life expectancy (years)",
       color = "Continent",
       caption = "Source: Gapminder") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2")

# Graph 2: Gapminder animated plot

graph1.animation = graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}")

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("gapminder graph.gif")graph1 = gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Life Expectancy vs GDP Per Capita by Country",
       x = "Income per person (GDP / capita)",
       y = "Life expectancy (years)",
       color = "Continent",
       caption = "Source: Gapminder") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2")

graph1.animation = graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}")

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("gapminder graph.gif")


