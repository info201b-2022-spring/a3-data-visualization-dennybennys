#Load package
library("dplyr")
library("tidyverse", warn.conflicts = FALSE)
library("ggplot2")
install.packages("plotly")
library("plotly", warn.conflicts = FALSE)
install.packages("rjson")
library("rjson", warn.conflicts = FALSE)
install.packages("kableExtra")
library("kableExtra")
library("reshape2")

#Load data 
incarceration_df <- read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

prison_jail_population <- select(incarceration_df, year, aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, white_prison_pop, other_race_prison_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop) %>% 
  group_by(year) %>% 
  summarise(sum_aapi_pop = sum(aapi_prison_pop, na.rm = TRUE) + sum(aapi_jail_pop, na.rm = TRUE), sum_black_pop = sum(black_prison_pop, na.rm = TRUE) + sum(black_jail_pop, na.rm = TRUE), sum_latinx_pop = sum(latinx_prison_pop, na.rm = TRUE) + sum(latinx_jail_pop, na.rm = TRUE), sum_native_pop = sum(native_prison_pop, na.rm = TRUE) + sum(native_jail_pop, na.rm = TRUE), sum_white_pop = sum(white_prison_pop, na.rm = TRUE) + sum(white_jail_pop, na.rm = TRUE), sum_other_race_pop = sum(other_race_prison_pop, na.rm = TRUE) + sum(other_race_jail_pop, na.rm = TRUE), total_prison_jail_population = sum(sum_aapi_pop, sum_black_pop, sum_latinx_pop, sum_native_pop, sum_white_pop, sum_other_race_pop))

total_population <- prison_jail_population %>% 
  slice(-c(1:15, 48:49))

year <- c(total_population$year)
aapi <- c(total_population$sum_aapi_pop)
black <- c(total_population$sum_black_pop)
latinx <- c(total_population$sum_latinx_pop)
native <- c(total_population$sum_native_pop)
white <- c(total_population$sum_white_pop)
other_race <- c(total_population$sum_other_race_pop)

chart_data <- data.frame(total_population, year, aapi, black, latinx, native, white, other_race)

comparison_chart <- chart_data %>% 
  select(year, aapi, black, latinx, native, white, other_race)

comparison_chart <- melt(comparison_chart, id.vars = "year")

comparison_chart <- rename(comparison_chart, race = variable)

comparison_chart <- rename(comparison_chart, population = value)

total_pop <- chart_data %>% 
  select(year, total_prison_jail_population)

chart2 <- left_join(comparison_chart, total_pop, by = "year")  

#Variable Comparison Chart

ggplot(data = chart2) +
  geom_col(aes(x = year, y = population, fill = race))
