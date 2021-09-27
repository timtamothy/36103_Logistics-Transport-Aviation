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
allmonths <- fread(here('dummy.csv'))

# Select airports
airports <- fread(here('top50.csv'))
airport_list <- colnames(airports)



# Create first mlm ----
mlm1 <- lm(dep_delay ~ ., data=dat_transformed)

first_ml <- summary(mlm1)

sink('first_summary.txt')
first_ml
sink()
