# Load Packages ----
library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(scales)

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




# Bar chart for AA across all years

contribution <- ontime %>% mutate(WITHCAUSE_DELAY = CARRIER_DELAY + WEATHER_DELAY 
                                   + NAS_DELAY,SECURITY_DELAY + LATE_AIRCRAFT_DELAY 
                                   , OTHER_DELAY = ifelse(DEP_DELAY_NEW > WITHCAUSE_DELAY,
                                                          DEP_DELAY_NEW - WITHCAUSE_DELAY,
                                                          0))  %>%
  filter(ontime$OP_UNIQUE_CARRIER == 'AA') %>% 
  select(CARRIER_DELAY, WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY, 
         DEP_DELAY_NEW, WITHCAUSE_DELAY, OTHER_DELAY, Airline)
  

contribution <- contribution %>% mutate_all(~replace(., is.na(.), 0))

contribution <- contribution %>% select(-c(DEP_DELAY_NEW, WITHCAUSE_DELAY, OTHER_DELAY))
sum_contribution <- data.frame(value = apply(contribution, 2, sum))
sum_contribution$key = rownames(sum_contribution)

ggplot(data = sum_contribution, aes(x = reorder(key, value), y = value, fill = key)) + 
  geom_bar(colour = "black", stat = "identity", show.legend = FALSE) + xlab("Causes") + ylab('Number of Delays') +
  labs(title='Total Minutes of Delay for AA from 2019 to 2021 by Type') +
  theme_minimal() +
  scale_fill_brewer(palette="Set3") +
  coord_flip() +
  scale_y_continuous(name='Minutes', labels = comma)



# Summary Statistics for each airline
# Which airline has the least average delay?

# View mean and median of delays above 0
ontime %>% 
  filter(DEP_DELAY_NEW > 0) %>% 
  group_by(Airline) %>% 
  select(Airline, DEP_DELAY_NEW, OP_UNIQUE_CARRIER, DEP_DELAY) %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW), 
            median_delay = median(DEP_DELAY_NEW),
            number_flights = length(OP_UNIQUE_CARRIER)
  ) %>% 
  arrange(mean_delay)

# View which months have the greatest delays or which dates
ontime %>% 
  filter(DEP_DELAY_NEW > 0) %>% 
  group_by(MONTH) %>% 
  select(Airline, DEP_DELAY_NEW, OP_UNIQUE_CARRIER, DEP_DELAY) %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW), 
            median_delay = median(DEP_DELAY_NEW),
            number_flights = length(OP_UNIQUE_CARRIER)
  ) %>% 
  arrange(mean_delay)

# View which airports with 50,000 or more flights have the greatest delays
ontime %>% 
  filter(DEP_DELAY_NEW > 0) %>% 
  group_by(Dest_AIRPORTNAME) %>% 
  select(Airline, DEP_DELAY_NEW, OP_UNIQUE_CARRIER, DEP_DELAY) %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW), 
            median_delay = median(DEP_DELAY_NEW),
            number_flights = length(OP_UNIQUE_CARRIER)) %>% 
  filter(number_flights >= 50000) %>% 
  arrange(-mean_delay)


# View which airports with 50,000 or more flights have the greatest delays for AA

ontime %>% 
  filter(DEP_DELAY_NEW > 0) %>% 
  group_by(Dest_AIRPORTNAME) %>% 
  select(Airline, DEP_DELAY_NEW, OP_UNIQUE_CARRIER, DEP_DELAY) %>% 
  filter(OP_UNIQUE_CARRIER == 'AA') %>% 
  summarize(mean_delay = mean(DEP_DELAY_NEW), 
            median_delay = median(DEP_DELAY_NEW),
            number_flights = length(OP_UNIQUE_CARRIER)) %>% 
  filter(number_flights >= 5000) %>% 
  arrange(-mean_delay)


colnames(ontime)


# Box Plot for Q1 of 2019, 2020, 2021 of Delays > 0

ontime %>% 
  filter(DEP_DELAY > 0 & DEP_DELAY <=60) %>%
  filter(YEAR %in% c('2019', '2020', '2021')) %>%
  filter(OP_UNIQUE_CARRIER %in% c('AA', 'DL', 'UA')) %>% 
  ggplot(aes(x = Airline, y = DEP_DELAY, fill = Airline)) + 
  geom_boxplot(alpha = 0.3) +
  #ylim(0, 30) +
  labs(title='Departure Delays in Q1 by Airline from 2019 to 2021') +
  ylab('Departure Delay in Minutes') +
  xlab('Airline') +
  scale_fill_brewer(palette="PRGn")

# Summary Statistics for each year
year2019 <- ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <=60) %>% 
  filter(YEAR == 2019)

year2019 %>% 
  group_by(Airline) %>% 
  summarize(mean = mean(DEP_DELAY), median = median(DEP_DELAY))

year2020 <- ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <=60) %>% 
  filter(YEAR == 2020)

year2020 %>% 
  group_by(Airline) %>% 
  summarize(mean = mean(DEP_DELAY), median = median(DEP_DELAY))

year2021 <- ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <=60) %>% 
  filter(YEAR == 2021)

year2021 %>% 
  group_by(Airline) %>% 
  summarize(mean =mean(DEP_DELAY), median = median(DEP_DELAY))

