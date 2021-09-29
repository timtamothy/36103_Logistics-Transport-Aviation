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
library(caret)
library(future)
plan(multisession)
# Load data
hot <- read_feather(here('mlm_dataset_3.feather'))
colnames(hot)
plan(multisession)
hot <- read_feather("C:/Users/tsetc/Downloads/mlm_dataset_3.feather")
airports <- read_csv("https://raw.githubusercontent.com/timothywallaby/36103_Logistics-Transport-Aviation/main/top50.csv")
  
# Select airports
airports <- fread(here('top50.csv'))
colnames(airports)
airport_list <- as.factor(airports$`AIRPORT CODES`)
View(airport_list)
airport_list[2]

hot_filtered <- hot %>% 
  filter(origin_airport_code %in% airport_list) %>% 
  filter(dest_airport_code %in% airport_list) %>% 
  droplevels

unique(hot_filtered$dest_airport_code)
drop_na(hot_filtered)
write_feather(hot_filtered, here('top50.feather'))

# Charlotte add (add data trimming and normalization)
hot_filtered$id <- seq.int(nrow(hot_filtered))

num_col <- select_if(hot_filtered, is.numeric)

num_col <- num_col %>% drop_na()

#i in seq_along(num_col)

num_col <- num_col %>% 
  select('dep_delay', 'dep_delay_new', 'taxi_out', 'taxi_in',
         'arr_delay', 'arr_delay_new', 'air_time', 'distance',
         'age', 'carrier_delay', 'weather_delay', 'nas_delay',
         'security_delay', 'late_aircraft_delay', 'total_add_gtime',
         'empfull', 'emppart', 'emptotal', 'empfte')

for (i in 1:9) {
  if (max(num_col[,i]) > (mean(unlist(num_col[,i])) + 4* sd(unlist(num_col[,i])))) {
    num_col <- num_col %>% arrange(num_col[,i])
    num_col <- num_col[round(nrow(num_col) *0.01, digits = 0):round(nrow(num_col)* (1-0.01) , digits = 0),]
  }
}

#Min-max transformation 
min_max_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}
num_col_norm <- as.data.frame(future_lapply(num_col, min_max_norm))

cat_col <- hot_filtered %>% select_if(negate(is.numeric))
cat_col$id <- seq.int(nrow(hot_filtered))

test <- left_join(num_col_norm, cat_col, by = 'id')

library(dplyr)
hot_filtered <- hot_filtered %>% dplyr::select(-c('id', 'arr_delay'))



#write a csv for test - before dummy variable but after transform 
fwrite(hot_filtered, here('num_transform.csv'))


# make dummies
hot_filtered <- fread(here 'num_transform.csv')
colnames(hot_filtered)

dmy <- dummyVars(" ~ . ", data=hot_filtered, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = hot_filtered))

########
transform <- function(dummy=dmy, original=hot) {
  transformed_data <- data.frame(predict(dummy, newdata = original))
  return(transformed_data)
}

hot_2 <- future_sapply()

colnames(dat_transformed)

write_feather(dat_transformed, here('dummy.feather'))

write_feather(dat_transformed, here('dummy_numtransform.feather'))

fwrite(dat_transformed, here('dummy.csv'))

str(dat_transformed)
