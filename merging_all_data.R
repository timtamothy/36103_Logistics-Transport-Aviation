# Load Packages ----
library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)

#load the file names ----
files <- list.files(path=here('Dataset', 'All'), full.names = TRUE)

#change all first dep time columns to numeric and combine csvs
output <- files %>% 
  map_df(~{
    read_csv(.x) %>% 
      mutate(FIRST_DEP_TIME = as.numeric(FIRST_DEP_TIME))
  })

#new CSV of all data
write_csv(output, here('Dataset', 'All', 'allmonths.csv'))




#load data ----
allmonths <- read_csv(here('Dataset', 'All', 'allmonths.csv'))

#load API data
icao <- GET(
  url = 'https://applications.icao.int/dataservices/api/safety-characteristics-list?api_key=8d00ef90-0982-11ec-9d72-8160549d64ab&airports=&states=USA'
)
response <- content(icao, 'parsed')
API_data <- fromJSON(response)

#load airline name data
id_airline <- read.csv("https://raw.githubusercontent.com/timothywallaby/36103_Logistics-Transport-Aviation/main/L_UNIQUE_CARRIERS%20(1).csv")





#Clean some data ----
# Selecting only some columns for API data
API_data <- API_data %>% select(airportCode, airportName)

# Replace null values
allmonths <- allmonths %>% mutate_all(~replace(., is.na(.), 0))

# Change col names
colnames(id_airline) <- c("OP_UNIQUE_CARRIER", "Airline")




#Merging Data ----
ontime <- left_join(allmonths, id_airline, by = "OP_UNIQUE_CARRIER")%>% 
  mutate(Origin_airportCode = paste("K",ORIGIN, sep = ""), 
         Dest_airportCode = paste("K",DEST, sep = ""))

ontime <- left_join(ontime, API_data, by = c("Origin_airportCode" = "airportCode"))
names(ontime)[names(ontime) == "airportName"] <- "Origin_AIRPORTNAME"

ontime <- left_join(ontime, API_data, by = c("Dest_airportCode" = "airportCode"))
names(ontime)[names(ontime) == "airportName"] <- "Dest_AIRPORTNAME"

ontime <- ontime[ontime$CANCELLED == 0,]


#Write new csv of merged datasets
write_csv(output, here('Dataset', 'ontimeallmonths.csv'))






#EDA of ALL data points ----
ontime_perc <- ontime %>% mutate(ontime = ifelse(ARR_DELAY >0, "NO", "YES")) %>% 
  select(ontime, ARR_DELAY)

late <- sum(ontime_perc$ontime == "NO", na.rm=TRUE)
total <- length(ontime_perc$ontime)

slices <- c(total - late ,late)
lbls <- c("On time", "Late")
color <- c("white","red")

pct <- c(round((total - late)/total * 100,2), round(late/total * 100,2))	
lbls <- paste(c("On time", "Late"), pct, "%")	

pie(slices, labels = lbls, col = color)
