# Load Packages ----
library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)
library(data.table)
library(feather)

# Load Files ----
allmonths <- fread(here('Dataset', 'All', 'allmonths.csv'))

icao <- GET(url = 'https://applications.icao.int/dataservices/api/safety-characteristics-list?api_key=ec846ee6-18a5-4c62-9425-f8f66264a1ca&airports=&states=USA', format='JSON')
response <- content(icao, 'parsed')
API_data <- fromJSON(response)

id_airline <- read.csv("https://raw.githubusercontent.com/timothywallaby/36103_Logistics-Transport-Aviation/main/L_UNIQUE_CARRIERS%20(1).csv")
colnames(id_airline) <- c("OP_UNIQUE_CARRIER", "Airline")

#API_data <- API_data %>% select(airportCode, airportName)
allmonths <- allmonths %>% mutate_all(~replace(., is.na(.), 0))
colnames(id_airline) <- c("OP_UNIQUE_CARRIER", "Airline")

#Merging Data ----
ontime <- left_join(allmonths, id_airline, by = "OP_UNIQUE_CARRIER")%>% 
  mutate(Origin_airportCode = paste("K",ORIGIN, sep = ""), 
         Dest_airportCode = paste("K",DEST, sep = ""))

allmonths <- left_join(allmonths, API_data, by = c("Origin_airportCode" = "airportCode"))
names(allmonths)[names(allmonths) == "airport_name"] <- "Origin_AIRPORTNAME"

allmonths <- left_join(allmonths, API_data, by = c("Dest_airportCode" = "airportCode"))
names(allmonths)[names(allmonths) == "airport_name"] <- "Dest_AIRPORTNAME"

ontime <- ontime[ontime$CANCELLED == 0,]

ontime$YEAR <- as.factor(ontime$YEAR)

# Select only AA, DL, UA
allmonths <- allmonths %>% 
  filter(Airline == c('American Airlines', 'Delta Airlines', 'United Airlines'))


# Write data to feather file
write_feather(allmonths, here('all_ontime.feather'))







# Test loading of feather data and using it
all_ontime <- read_feather(here('all_ontime.feather'))

View(all_ontime)

# View which months have the greatest delays or which dates
all_ontime %>% 
  filter(DEP_DELAY_NEW > 0) %>% 
  group_by(MONTH) %>% 
  select(Airline, DEP_DELAY_NEW, OP_UNIQUE_CARRIER, DEP_DELAY) %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW), 
            median_delay = median(DEP_DELAY_NEW),
            number_flights = length(OP_UNIQUE_CARRIER)
  ) %>% 
  arrange(mean_delay)

all_ontime %>% 
  filter(DEP_DELAY > 0 & DEP_DELAY <=60) %>%
  filter(YEAR %in% c('2019', '2020', '2021')) %>%
  filter(OP_UNIQUE_CARRIER %in% c('AA', 'DL', 'UA')) %>% 
  ggplot(aes(x = Airline, y = DEP_DELAY, fill = YEAR)) + 
  geom_boxplot(alpha = 0.3) +
  theme_minimal() +
  #ylim(0, 30) +
  labs(title='Departure Delays in Q1 by Airline from 2019 to 2021') +
  ylab('Departure Delay in Minutes') +
  xlab('Airline') +
  scale_fill_brewer(palette="Pastel1")
