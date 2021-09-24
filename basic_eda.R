# Load Packages ----
library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)
library(data.table)
library(feather)
library(DataExplorer)
library(funModeling)
library(Hmisc)
library(janitor)

# Load Data ----
allmonths <- read_feather(here('clean_allairlines_allmonths.feather'))

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data)
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

basic_eda(allmonths)

# Check NA values 

# Check distribution -- justtify normalization in feature engineering 


# Map visualization based on states 
# route stat (need to merge)

