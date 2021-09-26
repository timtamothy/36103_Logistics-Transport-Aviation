# Truly a very basic EDA workflow to view your column info
# And column distributions

# 1. # 1. Load your packages. These were used: ----
library(tidyverse)
library(here)
library(future.apply)
library(dplyr)
library(feather)
library(DataExplorer)
library(funModeling)
library(Hmisc)

# 2. Load Data ----
df <- read_feather(here('file.feather'))

# 3. EDA1:use only a subset of your column variables ----
df2 <- df %>% select(these, are, the, column, variables, you, want,
                     to, examine)

glimpse(df2)

# 4. EDA2:get metrics about data type, zeros, infinite & missing values: ----
status(df2)
  #note, no missing values q_na
  
  # CHECK:
  # - are all variables correct type?
  # - are there variables with lots of zeros or NAs?
  # - is there high cardinality variable?

# 5. EDA3: analyse the categorical variables in one go & plots: ----
freq(df2)
#freq(df2, path_out = ".") if you want to save plots as jpg
  
  # CHECK:
  # - do the cateogires make sense?
  # - are there missing values?
  # - check absolute and relative values

# 6. EDA4: analyse the numerical values in one go: ----
plot_num(df2)
#plot_num(df2, path_out = ".") if you want to save plots as jpg
  
  #CHECK:
  # - high, unbalanced values
  # - visual check for outliers

#print out the numerical statistics
data_prof = profiling_num(df2)
data_prof
  # - describe each variable based on distribution
  # - pay attention to variables with high standard deviation
  # - select metrics you are most familiar with
data_prof %>% select(variable, variation_coef, range_98)

# 7. Analyzing both numerican and categorical at same time ----
describe(df2)