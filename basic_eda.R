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






# Separating individual functions from the basic_eda function:

# EDA1:use only a subset of your column variables ----
df2 <- df1 %>% select(los, death, age_27, severity, mi, os_sats, 
                      creatinine, sodium, glucose, alt, wbc, troponin,
)

glimpse(df2)

# CLEAN: change death, mi to categorical variables
df2$death = as.factor(df2$death)
str(df2)
df2$mi = as.factor(df2$mi)

# EDA2:get metrics about data type, zeros, infinite & missing values: ----
status(df2)
#note, no missing values q_na
# CHECK:
# - are all variables correct type?
# - are there variables with lots of zeros or NAs?
# - is there high cardinality variable?

# EDA3: analyse the categorical variables in one go & plots: ----
freq(df2)
#note, export plots to jpeg in current directory:
#freq(data, path_out = ".")
# CHECK:
# - do the cateogires make sense?
# - are there missing values?
# - check absolute and relative values

# EDA4: analyse the numerical values in one go:
plot_num(df2)
#CHECK:
# - high, unbalanced values
# - visual check for outliers

# creatinine, glucose, ALT, white blood cells, trop are all skewed left

#print out the numerical statistics
data_prof = profiling_num(df2)
data_prof
# - describe each variable based on distribution
# - pay attention to variables with high standard deviation
# - select metrics you are most familiar with
data_prof %>% select(variable, variation_coef, range_98)

#Analyzing both at same time ----
describe(df2)