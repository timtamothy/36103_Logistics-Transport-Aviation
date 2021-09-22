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

# Load Data ----
allmonths <- read_feather(here('all_ontime.feather'))

colnames(allmonths)

clean_months <- clean_names(allmonths)

col_status <- status(clean_months)

write_csv(col_status, here('col_status.csv'))
