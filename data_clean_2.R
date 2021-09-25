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
library(httr)
library(jsonlite)
plan(multisession) ## run in parallel on local computer

# Load Data ----
allmonths <- read_feather(here('clean_3airline_fleet_employ.feather'))


# Clean column names ----
clean_months <- clean_names(allmonths)

colnames(clean_months)
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
         'airline', 'origin_airportname', 'origin_airport_code',
         'dest_airportname','dest_airport_code', 'manufacturer',
         'model', 'age', 'model_year', 'empfull', 'emppart',
         'emptotal', 'empfte')

# Change column types

wrong_columns <- c('age', 'model_year')

clean_months['age'] <- future_lapply(clean_months['age'], as.numeric)
clean_months['model_year'] <- future_lapply(clean_months['model_year'], as.factor)


# Check columns

col_status_cleaner <- status(clean_months)

#write interim feather file

write_feather(clean_months, here('clean_3airline_fleet_employ_2.feather'))

