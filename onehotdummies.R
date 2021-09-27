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

# Load data

hot <- read_feather(here('mlm_dataset_3.feather'))

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
# make dummies
hot_filtered <- hot_filtered %>% 
  select(-arr_delay)

colnames(hot_filtered)

dmy <- dummyVars(" ~ . ", data=hot_filtered, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = hot_filtered))

transform <- function(dummy=dmy, original=hot) {
  transformed_data <- data.frame(predict(dummy, newdata = original))
  return(transformed_data)
}

hot_2 <- future_sapply()

colnames(dat_transformed)

write_feather(dat_transformed, here('dummy.feather'))

fwrite(dat_transformed, here('dummy.csv'))

str(dat_transformed)
