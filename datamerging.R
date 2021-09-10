library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)


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
