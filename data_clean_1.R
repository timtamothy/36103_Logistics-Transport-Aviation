# load packages ----

library(tidyverse)
library(magrittr)
library(plotly) #3D modeling
library(regclass) #look at what is going on with dataset
library(Boruta) #for determining variable importance
library(feather)
library(here)
library(janitor)
library(funModeling)
library(Hmisc)
library(DataExplorer)
library(future.apply)
plan(multisession) ## run in parallel on local computer

# Load Data ----
allmonths <- read_feather(here('all_ontime.feather'))

colnames(allmonths)

# Clean column names ----
clean_months <- clean_names(allmonths)

col_status <- status(clean_months)

write_csv(col_status, here('col_status.csv'))

# Removing unused columns

clean_months <- clean_months %>% 
  select('year', 'quarter', 'month', 'day_of_month', 'day_of_week',
         'fl_date', 'tail_num', 'op_carrier_fl_num', 'origin_state_abr',
         'dest_state_abr', 'dep_time', 'dep_delay',
         'dep_delay_new', 'taxi_out', 'taxi_in', 'arr_time', 'arr_delay',
         'arr_delay_new', 'air_time', 'distance', 'carrier_delay',
         'weather_delay', 'nas_delay', 'security_delay',
         'late_aircraft_delay', 'first_dep_time', 'total_add_gtime',
         'airline', 'origin_airport_code', 'dest_airport_code')

# Change column types

wrong_columns <- c('quarter', 'month', 'day_of_month', 'day_of_week',
                   'op_carrier_fl_num', 'dep_time', 'arr_time', 'first_dep_time')

clean_months[wrong_columns] <- future_lapply(clean_months[wrong_columns], factor)


# Check columns

col_status_cleaner <- status(clean_months)


# Save new dataset

write_feather(clean_months, here('clean_allairlines_allmonths.feather'))
