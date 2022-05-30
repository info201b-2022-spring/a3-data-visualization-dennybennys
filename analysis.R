#Load data
incarceration_df <- read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

#Load packages
library(dplyr)
library(tidyverse)
install.packages("knitr")
library(knitr)

#What is the location (county, state) with the highest number of people of color in prison?

incarceration_df <- incarceration_df %>% 
  mutate(location = paste0(county_name, ", ", state))

highest_poc_prison_pop_location <- incarceration_df %>% 
  group_by(location) %>% 
  summarise(total_poc_prisoner = sum(aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, na.rm = TRUE)) %>% 
  filter(total_poc_prisoner == max(total_poc_prisoner), na.rm = TRUE) %>% 
  pull(location)


#When was the prison population at its highest?

highest_prison_pop <- incarceration_df %>% 
  group_by(year) %>% 
  summarise(total_prisoner = sum(total_prison_pop, na.rm = TRUE)) %>% 
  filter(total_prisoner == max(total_prisoner, na.rm = TRUE)) %>% 
  pull(year)
  

#What is the total number of people of color prisoners as well as the total number of non-POC (people of color) prisoners across all states in the most recent year with data?

total_poc_prisoner_2016 <- incarceration_df %>%
  filter(year == 2016) %>% 
  group_by(state) %>% 
  summarise(total_poc_prisoner_2016 = sum(aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, na.rm = TRUE))

total_non_poc_prisoner_2016 <- incarceration_df %>% 
  filter(year == 2016) %>% 
  group_by(state) %>% 
  summarise(total_non_poc_prisoner_2016 = sum(white_prison_pop, na.rm = TRUE))

#What is the ratio of the people of color prisoner population to the total prisoner population in 2016?

incarceration_df <- incarceration_df %>% 
  rowwise() %>% 
  mutate(poc_prison_pop = sum(aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, na.rm = TRUE))

total_prisoner_pop <- incarceration_df %>% 
  group_by(year) %>% 
  filter(year == 2016) %>% 
  summarise(total_prisoner_pop = sum(total_prison_pop, na.rm = TRUE)) %>% 
  pull(total_prisoner_pop)

total_poc_prison_pop <- incarceration_df %>% 
  group_by(year) %>% 
  filter(year == 2016) %>% 
  summarise(total_poc_prison_pop = sum(poc_prison_pop, na.rm = TRUE)) %>% 
  pull(total_poc_prison_pop)

poc_to_total_ratio <- total_poc_prison_pop / total_prisoner_pop


#What is the ratio of the non-POC prisoners to the total prisoner population in 2016?

total_non_poc_prison_pop <- incarceration_df %>% 
  group_by(year) %>% 
  filter(year == 2016) %>% 
  summarise(total_non_poc_prison_pop = sum(white_prison_pop, na.rm = TRUE)) %>% 
  pull(total_non_poc_prison_pop)

non_poc_to_total_ratio <- total_non_poc_prison_pop / total_prisoner_pop


#From 1970 to 2016, how has the total number of people of color prisoners changed across all states?

total_poc_prison_change <- incarceration_df %>% 
  group_by(year) %>% 
  summarise(total_poc_prison_change = sum(poc_prison_pop, na.rm = TRUE)) %>% 
  filter(between(year, 1970, 2016))

#From 1970 to 2016, how has the total number of non-POC prisoners changed across all states?

total_non_poc_prison_change <- incarceration_df %>% 
  group_by(year) %>% 
  summarise(total_non_poc_prison_change = sum(white_prison_pop, na.rm =TRUE)) %>% 
  filter(between(year, 1970, 2016))
