library(tidyverse)
library(here)
library(readr)
library(future.apply)


numCores <- detectCores()

#load the file names
files <- list.files(path=here('Dataset', 'Q1'), full.names = TRUE)


#this has done a multicore workload of loading all data in the files path
data <- files %>% 
  future_lapply(read_csv)


#Append each of the csvs
dataf <- files %>% 
  future_lapply(read_csv) %>% 
  bind_rows
