#Load Package
library("dplyr")
library("ggplot2")
library("plotly")
library("scales")
library("maps")
library("mapproj")

#Load data 
incarceration_df <- read.csv(file = "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

incarceration_df <- incarceration_df %>% 
  rowwise() %>% 
  mutate(poc_prison_pop = sum(aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, na.rm = TRUE))


state_shape <- map_data("state")

incarceration_df <- incarceration_df %>% 
  mutate(state_fullname = tolower(state.name[match(state, state.abb)]))

state_data <- incarceration_df %>% 
  group_by(state_fullname) %>% 
  summarize(poc_prison_pop = sum(aapi_prison_pop, black_prison_pop, latinx_prison_pop, native_prison_pop, other_race_prison_pop, na.rm = TRUE))

state_shape_data <- left_join(state_shape, state_data,
                              by = c("region" = "state_fullname"))

#Map

ggplot(state_shape_data) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = poc_prison_pop), color = "black") +
  scale_fill_continuous(low = 'yellow', high = 'red', labels = label_number_si()) +
  coord_map() + 
  labs(title = 'Total POC Prisoners From 1970 to 2018', fill = 'Number of POC prisoners')

