# 1. Load your packages. These were used: ----
library(tidyverse)
library(feather)
library(here)
library(janitor)
library(future.apply)

# Load data
removed_col <- read_feather(here('mlm_dataset.feather'))

# Select only wanted columns
removed_col <- removed_na %>% 
  select('dep_delay', 'year', 'month',
         'day_of_week', 'fl_date',
         'origin_state_abr', 'dest_state_abr',
         'dep_time', 'arr_delay', 'air_time',
         'airline', 'taxi_out', 'taxi_in',
         'origin_airport_code', 'dest_airport_code',
         'manufacturer', 'model', 'age', 'emptotal')

removed_col <- removed_col %>% 
  filter(taxi_in > 0)

removed_col <- removed_col %>% 
  filter(air_time > 0)

status(removed_col)


# Save data

write_feather(removed_col, here('mlm_dataset.feather'))


num_col <- select_if(removed_col, is.numeric)
num_col <- num_col %>% sample_n(50000)

pairs(num_col)

smaller <- removed_col %>% sample_n(50000)

library(corrplot)
corrplot(cor(num_col), method = 'number')

corrplot(cor(num_col), method = 'ellipse')
