#Load package
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

#Load data 
incarceration_df <- read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

incarceration_df <- incarceration_df %>% 
  rowwise() %>% 
  mutate(poc_prison_pop = sum(aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, na.rm = TRUE))


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

poc_non_poc_totals <- left_join(total_poc_prison_change, total_non_poc_prison_change)

poc_non_poc_totals <- melt(poc_non_poc_totals, id.vars = "year")

poc_non_poc_totals <- rename(poc_non_poc_totals, race_totals = variable)

poc_non_poc_totals <- rename(poc_non_poc_totals, prisoners = value)

#Trends over time chart

options(scipen = 999)

ggplot(poc_non_poc_totals, aes(year, prisoners, col = race_totals)) +
  geom_line() +
  labs(title = "The Yearly Change of POC Prisoners vs Non-POC Prisoners", x = "year", y = "number of prisoners")
