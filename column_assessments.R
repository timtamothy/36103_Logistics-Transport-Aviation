# load packages ----
library(tidyverse)
library(DataExplorer)
library(here)
library(funModeling)
library(Hmisc)
library(janitor)

# Load data ----
allmonths <- read_feather(here('clean_3airline_fleet_employ.feather'))

# Overall status
status(allmonths)

# Categorical overall
freq(allmonths, path_out = here('Column EDA', '.'))

# Numerical Overall
plot_num(allmonths, path_out = here('Column EDA', '.'))
     