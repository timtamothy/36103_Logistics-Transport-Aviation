# Load Packages ----
library(tidyverse)
library(here)
library(feather)
library(dplyr)
library(janitor)

# Load Files ----
maindf <- read_feather(here('clean_3airlines.feather'))

delta <- read_feather(here('DAL_fleet.feather'))

american <- read_feather(here('AA_fleet.feather'))

united <- read_feather(here('UAL_fleet.feather'))

# Join Files ----

allrows <- bind_rows(delta, american, united)

testjoin <- left_join(maindf, allrows, by = c('tail_num' = 'Tail'))

col_status <- status(testjoin)

# Clean column names ----
clean_join <- clean_names(testjoin)

# Separate the age/year columns into two columns
separate <- clean_join %>% 
  separate(age_built, c('age', 'model_year'))

status(separate)

# Save a feather file

write_feather(separate, here('clean_3airline_fleet.feather'))
