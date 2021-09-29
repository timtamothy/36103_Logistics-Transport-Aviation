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
library(lubridate)

# Load data
allmonths <- read_feather(here('mlm_dataset_3.feather'))

# Look at all columns
colnames(allmonths)
str(allmonths)
status(allmonths)
wrong_cols <- c('year', 'month', 'day_of_week', 'dep_time')

allmonths['year'] <- future_lapply(allmonths['year'], factor)
allmonths['month'] <- future_lapply(allmonths['month'], factor)
allmonths['day_of_week'] <- future_lapply(allmonths['day_of_week'], factor)


#x <- substr(as.POSIXct(sprintf("%04.0f", allmonths$dep_time), format = "%H%M"), 12, 16)

allmonths$dep_time <- as.POSIXct(sprintf("%04.0f", allmonths$dep_time), format = "%H%M")
#allmonths$dep_time <- str_replace(allmonths$dep_time, '2021-09-29 ', '')

format(allmonths$dep_time, "%H%M")
allmonths$dep_time <- hms::as_hms(allmonths$dep_time)

allmonths %>%
  sample_n(50000) %>% 
  ggplot(aes(x=dep_time, y=dep_delay)) +
  geom_point(alpha = 0.3)

# Write
allmonths['year'] <- future_lapply(allmonths['year'], factor)
allmonths['month'] <- future_lapply(allmonths['month'], factor)
write_feather(allmonths, here('mlm_dataset_4time.feather'))

  
