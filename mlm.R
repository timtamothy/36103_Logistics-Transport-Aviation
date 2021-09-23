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


# Create first mlm ----
mlm1 <- lm(dep_delay ~ op_carrier, data=allmonths)
