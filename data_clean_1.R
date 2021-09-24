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
allmonths <- read_feather(here('clean_allairlines_allmonths.feather'))

colnames(allmonths)
unique(allmonths$airline)

# Merge Data ----
icao <- GET(url = 'https://applications.icao.int/dataservices/api/safety-characteristics-list?api_key=ec846ee6-18a5-4c62-9425-f8f66264a1ca&airports=&states=USA', format='JSON')
response <- content(icao, 'parsed')
API_data <- fromJSON(response)
API_data <- API_data %>% select(airportCode, airportName)

#Merging Data ----
allmonths <- left_join(allmonths, API_data, by = c("origin_airport_code" = "airportCode"))
names(allmonths)[names(allmonths) == "airportName"] <- "Origin_AIRPORTNAME"
allmonths <- left_join(allmonths, API_data, by = c("dest_airport_code" = "airportCode"))
names(allmonths)[names(allmonths) == "airportName"] <- "Dest_AIRPORTNAME"
allmonths <- allmonths[allmonths$CANCELLED == 0,]


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
         'dest_airportname','dest_airport_code')

# Change column types

wrong_columns <- c('quarter', 'month', 'day_of_month', 'day_of_week',
                   'op_carrier_fl_num', 'dep_time', 'arr_time', 'first_dep_time')

clean_months[wrong_columns] <- future_lapply(clean_months[wrong_columns], factor)


# Check columns

col_status_cleaner <- status(clean_months)

#write interim feather file

write_feather(clean_months, here('clean_allairlines.feather'))

# Select only AA, DL, and UA
target = ('American Airlines inc.')

test <- clean_months %>% 
  filter(airline %in% c('American Airlines Inc.',
                        'Delta Air Lines Inc.',
                        'United Air Lines Inc.'))

unique(test$airline)

# Save new dataset

write_feather(test, here('clean_3airlines.feather'))
