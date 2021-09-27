library(httr)
library(jsonlite)
library(readr)
library(here)


## airports_to_collect <- c('ICAO', 'PWSID')
## airports_to_collect1 <- c('KJFK', 'KNYQUEEN31')
## airports_to_collect2 <- c('KLAX', 'KCAELSEG13')
## airports_to_collect3 <- c('KSFO', 'KCASANBR67')KCASANBR51, KCASANBR24,KCASOUTH97,KCAMILLB8,KCABURLI57,KCASANMA420,KCAHILLS25,KCABRISB21
## airports_to_collect4 <- c('KORD', 'KILROSEM2')


## Get weather data



start <- as.Date('20190101',format = '%Y%m%d')
end <- as.Date('20190131',format = '%Y%m%d')

iDate <- start
weatherdata_list<-list()

i = 1

while (iDate <= end) {
  
  iDateF <- format(iDate, '%Y%m%d')
  print(iDateF)
  
  weather_api_data <- GET(url = 'https://api.weather.com/v2/pws/history/daily', 
                          query = list(stationId= 'KFLMIAMI69',
                                       format='json',
                                       units='m',
                                       date=iDateF,
                                       apiKey='ba2302a18fc84a31a302a18fc84a3178'))
  
  wresponse <- content(x = weather_api_data, as = 'text', encoding = 'UTF-8')
  weather_data <- fromJSON(wresponse)
  
  iDate <- iDate + 1
  print(weather_data$observations)
  obs_with_airportName <- c('KORD', weather_data$observations) 
  
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
