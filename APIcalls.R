# Load packages ---- 

library(httr)
library(jsonlite)
library(rwunderground)
library(readr)
library(here)


#### Endpoints for Amadeus include GET and POST
#### Pipeline = Filter, Parser, Endpoint, Serializer


#### Request an access token
#### Send post request to:
#### https://test.api.amadeus.com/v1/security/oauth2/token
#### With header:
#### Content-Type: application/x-www-form-urlencoded
#### With body:
#### grant_type=clinet_credentials&client_id=API_Key&client_secret=API_Secret

#### Get the Airport Data from ICAO API ----
icao <- GET(
  url = 'https://applications.icao.int/dataservices/api/safety-characteristics-list?api_key=8d00ef90-0982-11ec-9d72-8160549d64ab&airports=&states=USA'
)

#### Get response JSON content ----
response <- content(icao, 'parsed')

#### Convert to data tibble from JSON ----
data <- fromJSON(response)

write_csv(data,here('Dataset','Airports.csv'))


summary(data)

wugrndkey <- "ba2302a18fc84a31a302a18fc84a3178"
# wugrndkey <- 'ba2302a18fc84a31a302a18fc84a3178'

apikey <- ''

airports_to_collect <- c('ICAO', 'PWSID')
airports_to_collect1 <- c('KJFK', 'KNYQUEEN31')
#airports_to_collect2 <- c('KLAX', 'KCALOSAN958')KCALOSAN357 KCALOSAN698, KCAELSEG13, KCAELSEG23)

list_airport_wID <- list(airports_to_collect, airports_to_collect1,airports_to_collect2)
view(list_airport_wID)





weather_data <- history_range(set_location(airport_code = "KLAX"),
              date_start = "20190110",
              date_end = "20190314",
              limit = 1,
              no_api = FALSE,
              use_metric = FALSE,
              key = apikey,
              raw = FALSE,
              message = TRUE)

weather_data2<-history(set_location(airport_code = "KLAX"), date = "20200101")

weather <- GET(url = 'https://api.weather.com/v2/pws/history/daily?stationId=KNYVALLE15&format=json&units=m&date=20181001&apiKey=ba2302a18fc84a31a302a18fc84a3178')
wresponse <- content(x = weather, as = 'text', encoding = 'UTF-8')
weather_data3 <- fromJSON(wresponse) 



location <- GET(url = 'https://api.weather.com/v3/location/point?icaoCode=KLAX&language=en-US&format=json&apiKey=ba2302a18fc84a31a302a18fc84a3178')
lresponse <- content(x = location, as = 'text', encoding = 'UTF-8')
location_data1 <- fromJSON(lresponse) 
## ==============================================================================================

## Get weather data

start <- as.Date('20191115',format = '%Y%m%d')
end <- as.Date('20191231',format = '%Y%m%d')

iDate <- start
weatherdata_list<-list()

i = 1

while (iDate <= end) {

  iDateF <- format(iDate, '%Y%m%d')
  print(iDateF)
  
  weather_api_data <- GET(url = 'https://api.weather.com/v2/pws/history/daily', 
                          query = list(stationId= 'KCAELSEG13',
                                       format='json',
                                       units='m',
                                       date=iDateF,
                                       apiKey='ba2302a18fc84a31a302a18fc84a3178'))
  
  wresponse <- content(x = weather_api_data, as = 'text', encoding = 'UTF-8')
  weather_data <- fromJSON(wresponse)

  iDate <- iDate + 1
  print(weather_data$observations)
  obs_with_airportName <- c('KLAX', weather_data$observations) 
  
  weatherdata_list[[i]] <- obs_with_airportName
  
  temp <- t(data.frame(matrix(unlist(weatherdata_list[[i]]))))
  if(i > 1) {
    matrix1 <- rbind(matrix1, temp)
  }
   else {
     matrix1 <- temp
   }  
  
  i = i + 1
}



df1 <- as.data.frame(matrix1)

names(df1) <- c('ICAO', 'stationID','tz', 'obsTimeUtc', 'obsTimeLocal',
                'epoch','lat','lon','solarRadiationHigh','uvHigh', 'winddirAvg',
                'humidityHigh','humidityLow','humidityAvg',
                'qcStatus',
                'tempHigh','tempLow','tempAvg',
                'windspeedHigh','windspeedLow','windspeedAvg',
                'windgustHigh','windgustLow','windgustAvg',
                'dewptHigh','dewptLow','dewptAvg',
                'windchillHigh','windchillLow','windchillAvg',
                'heatindexHigh','heatindexLow','heatindexAvg',
                'pressureMax','pressureMin','pressureTrend',
                'precipRate','precipTotal')


write_csv(df1,here('Dataset','KJFK2020-2.csv'))

## ==============================================================================================




df <- t(data.frame(matrix(unlist(weatherdata_list[[1]]))))

df1$airport <- 'KJFK'

cbind(df,'KJFK' )

df <- as.data.frame(weatherdata_list)

df <- as.data.frame(t(as.data.frame(weatherdata_list)))


weather_data <- dplyr::bind_rows(weather_data_list)


start_formatted <- format(start, '%Y%m%d')


dates <- seq(as.Date('20200101',format = '%Y%m%d'), as.Date('20200110',format = '%Y%m%d'), by=1)

weather_data_list <- list()
i <- 1



for(d in dates) {
  
  weather_api_data <- GET(url = 'https://api.weather.com/v2/pws/history/daily', 
                  query = list(stationId= 'KNYQUEEN31',
                               format='json',
                               units='m',
                               date=format(d, '%Y%m%d'),
                               apiKey='ba2302a18fc84a31a302a18fc84a3178'))
  
  wresponse <- content(x = weather_api_data, as = 'text', encoding = 'UTF-8')
  weather_data_list[i] <- fromJSON(wresponse) 
  i <= i+1
}


weather4 <- GET(url = 'https://api.weather.com/v2/pws/history/daily', 
            query = list(stationId= 'KCALOSAN994',
                        format='json',
                        units='m',
                        date='20180101',
                        apiKey='ba2302a18fc84a31a302a18fc84a3178'))
            
wresponse4 <- content(x = weather4, as = 'text', encoding = 'UTF-8')
weather_data4 <- fromJSON(wresponse4) 


weather5 <- GET(url = 'https://api.weather.com/v2/pws/history/daily', 
                query = list(stationId= 'KNYQUEEN31',
                             format='json',
                             units='m',
                             date='20201010',
                             apiKey='ba2302a18fc84a31a302a18fc84a3178'))

wresponse5 <- content(x = weather5, as = 'text', encoding = 'UTF-8')
weather_data5 <- fromJSON(wresponse5) 



warnings()
