library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)

numCores <- detectCores()

#load the file names
files <- list.files(path=here('Dataset', 'Q1'), full.names = TRUE)


#this has done a multicore workload of loading all data in the files path
#data <- files %>% 
  #future_lapply(read_csv)


#Change all first dep time columns to numberic
output <- files %>% 
  map_df(~{
    read_csv(.x) %>% 
      mutate(FIRST_DEP_TIME = as.numeric(FIRST_DEP_TIME))
  })

View(output)

#Make a new CSV of all Q1 collated data
write_csv(output, here('Dataset', 'Q1', 'q1.csv'))


# read in Q1 data
Q1 <- read_csv(here('Dataset', 'Q1', 'q1.csv'))

# create a list of carriers we would like
carriers <- c('AA', 'DL', 'UA')

# filter tibble by carriers we would like
Q1_big_three <- filter(Q1, OP_UNIQUE_CARRIER %in% carriers)

# write tibble containing AA, DL, UA into csv
write_csv(Q1_big_three, here('Dataset', 'Q1', 'q1_big_three.csv'))



#Charlotte ---- 
Jan_2019 <- read_csv("C:/Users/tsetc/OneDrive/Desktop/dataset for logistics/2019_01.csv")
Feb_2019 <- read_csv("C:/Users/tsetc/OneDrive/Desktop/dataset for logistics/2019_02.csv")

Jan_2019 <- Jan_2019 %>% mutate(FIRST_DEP_TIME = as.numeric(FIRST_DEP_TIME))

ontime <- full_join(Jan_2019, Feb_2019)
ontime <- ontime[!is.na(ontime$ARR_DELAY),]
ontime <- ontime %>% mutate_all(~replace(., is.na(.), 0))

#Tims Added
Q1_big_three <- read_csv(here('Dataset', 'Q1', 'q1_big_three.csv'))

ontime3 <- Q1_big_three[!is.na(Q1_big_three$ARR_DELAY),]
ontime3 <- ontime3 %>% mutate_all(~replace(., is.na(.), 0))
  #was an error with FL_DATE

########################################################################
icao <- GET(
  url = 'https://applications.icao.int/dataservices/api/safety-characteristics-list?api_key=8d00ef90-0982-11ec-9d72-8160549d64ab&airports=&states=USA'
)
response <- content(icao, 'parsed')
API_data <- fromJSON(response)

#Only pick those columns that are useful later on 
API_data <- API_data %>% select(airportCode, airportName)

id_airline <- read.csv("https://raw.githubusercontent.com/timothywallaby/36103_Logistics-Transport-Aviation/main/L_UNIQUE_CARRIERS%20(1).csv")
colnames(id_airline) <- c("OP_UNIQUE_CARRIER", "Airline")

ontime3 <- left_join(ontime3, id_airline, by = "OP_UNIQUE_CARRIER")%>% 
  mutate(Origin_airportCode = paste("K",ORIGIN, sep = ""), 
         Dest_airportCode = paste("K",DEST, sep = ""))

ontime3 <- left_join(ontime3, API_data, by = c("Origin_airportCode" = "airportCode"))
names(ontime3)[names(ontime3) == "airportName"] <- "Origin_AIRPORTNAME"

ontime3 <- left_join(ontime3, API_data, by = c("Dest_airportCode" = "airportCode"))
names(ontime3)[names(ontime3) == "airportName"] <- "Dest_AIRPORTNAME"

#Clean data 
ontime3 <- ontime3[ontime3$CANCELLED == 0,]

#EDA 

#On time performance 
ontime_perc <- ontime3 %>% mutate(ontime = ifelse(ARR_DELAY >0, "NO", "YES")) %>% 
  select(ontime, ARR_DELAY)

#ggplot(ontime_perc, aes(x = "", y = ARR_DELAY, group = ontime)) + 
#geom_bar(stat = "identity", width = 1) + 
#coord_polar("y", start = 0)

late <- sum(ontime_perc$ontime == "NO")
total <- length(ontime_perc$ontime)

slices <- c(total - late ,late)
lbls <- c("On time", "Late")
color <- c("white","red")

pct <- c(round((total - late)/total * 100,2), round(late/total * 100,2))	
lbls <- paste(c("On time", "Late"), pct, "%")	

pie(slices, labels = lbls, col = color)

# Note: No buffer were added 

#Contribution of delay to total delay 

#ggplot(data = ontime, aes = CARRIER_DELAY, WEATHER_DELAY
#,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY) + geom_histogram() 


contribution <- ontime3 %>% mutate(WITHCAUSE_DELAY = CARRIER_DELAY + WEATHER_DELAY 
                                  + NAS_DELAY,SECURITY_DELAY + LATE_AIRCRAFT_DELAY 
                                  , OTHER_DELAY = ifelse(ARR_DELAY_NEW > WITHCAUSE_DELAY,
                                                         ARR_DELAY_NEW - WITHCAUSE_DELAY,
                                                         0))  %>%
  select(CARRIER_DELAY, WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY, 
         ARR_DELAY_NEW, WITHCAUSE_DELAY, OTHER_DELAY)

contribution <- contribution %>% mutate_all(~replace(., is.na(.), 0))

contribution <- contribution %>% select(-c(ARR_DELAY_NEW, WITHCAUSE_DELAY, OTHER_DELAY))
sum_contribution <- data.frame(value = apply(contribution, 2, sum))
sum_contribution$key = rownames(sum_contribution)

ggplot(data = sum_contribution, aes(x = reorder(key, value), y = value, fill = key)) + 
  geom_bar(colour = "black", stat = "identity", show.legend = FALSE) + xlab("Causes") + ylab('Total Delays') +
  labs(title='Total Causes of Delays in Q1 of 2019-2021') +
  coord_flip()


#For illustration 
sum(na.omit(contribution$CARRIER_DELAY)) + sum(na.omit(contribution$WEATHER_DELAY))+ 
  sum(na.omit(contribution$NAS_DELAY)) + sum(na.omit(contribution$SECURITY_DELAY)) + 
  sum(na.omit(contribution$LATE_AIRCRAFT_DELAY)) + sum(na.omit(contribution$OTHER_DELAY))

sum(na.omit(contribution$ARR_DELAY_NEW))
# some of the flights have delay from those factors, but did not delay in the end 


# Competitor Review 
# Compare big 3 companies across quarters
unique(ontime3$Airline) 
# American Airlines Inc.
# Delta Air Lines Inc.
# United Air Lines Inc. 
# SkyWest Airlines Inc.

ontime3 %>% 
  filter(ontime3$ARR_DELAY >= 0 & ontime3$ARR_DELAY <=60) %>%
  filter(YEAR < 2021) %>% 
  ggplot(aes(x = Airline, y = ARR_DELAY, colour = Airline)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title='Arrival Delays in Q1 by Airline in 2020 and 2019') +
  ylab('Arrival Delay in Minutes')

ontime3 %>% 
  filter(ontime3$ARR_DELAY >= 0 & ontime3$ARR_DELAY <= 60) %>%
  filter(YEAR == 2021) %>% 
  ggplot(aes(x = Airline, y = ARR_DELAY, colour = Airline)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title='Arrival Delays in Q1 by Airline in 2021') +
  ylab('Arrival Delay in Minutes')

# Testing commit

ontime3 %>% 
  filter(ontime3$ARR_DELAY <= 60, ontime3$YEAR == 2021, ontime3$MONTH == 1) %>% 
  ggplot(aes(x=ARR_DELAY, y=DEP_DELAY, color = Airline)) +
  geom_point()

# Departure Delays
ontime3$YEAR <- as.factor(ontime3$YEAR)

ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <=60) %>%
  filter(YEAR %in% c('2019', '2020', '2021')) %>%
  ggplot(aes(x = YEAR, y = DEP_DELAY, fill = Airline)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title='Departure Delays in Q1 by Airline in 2020 and 2019') +
  ylab('Departure Delay in Minutes') +
  xlab('1st Quarter of Year')

ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <= 60) %>%
  filter(YEAR == 2021) %>% 
  ggplot(aes(x = Airline, y = DEP_DELAY, colour = Airline)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title='Departure Delays in Q1 by Airline in 2021') +
  ylab('Departure Delay in Minutes')

colnames(ontime3)

year20192020 <- ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <=60) %>% 
  filter(YEAR < 2021)

year20192020 %>% 
  group_by(Airline) %>% 
  summarize(mean = mean(DEP_DELAY), median = median(DEP_DELAY))

year2021 <- ontime3 %>% 
  filter(ontime3$DEP_DELAY >= 0 & ontime3$DEP_DELAY <=60) %>% 
  filter(YEAR == 2021)

year2021 %>% 
  group_by(Airline) %>% 
  summarize(mean =mean(DEP_DELAY), median = median(DEP_DELAY))
