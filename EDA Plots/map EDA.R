# Map EDA 

library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)
library(data.table)
library(feather)
library(jsonlite)
library(ggplot2)
library(maps)
library(stringr)

allmonths <- read_feather(here('mlm_dataset_4time.feather'))
merge <- read_csv(here('state code.csv'))
                  
merge <- merge %>% select(-Abbrev) 
merge$State <- tolower(merge$State)

subset <- allmonths %>% select(dep_delay, origin_state_abr)

full<- left_join(subset, merge, by = c('origin_state_abr' = 'Code'))


state_coord <- as_tibble(map_data("state"))

small <- full %>% group_by(State, origin_state_abr) %>% summarise(avg = mean(dep_delay))

plot_state <- left_join(small,state_coord, by = c('State' = 'region'))


ggplot(plot_state, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = avg)) + 
  ggtitle("Average flight departure delay by state") + 
  theme(plot.title = element_text(hjust = 0.5))


