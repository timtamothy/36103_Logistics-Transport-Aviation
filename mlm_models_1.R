library(tidyverse)
library(here)
library(readr)
library(future.apply)
library(dplyr)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(scales)
library(feather)
library(caret)
library(corrplot)


mlm_final <- read_feather(here('mlm_dataset_5.feather'))

# sqrt transoformation 
mlm_final$dep_delay <- sqrt(mlm_final$dep_delay)
  # NaNs produced and I don't know why

# Select California only as Subset

mlm_CA <- mlm_final %>% filter(origin_state_abr == 'CA')
names(mlm_CA)

mlm_all <- mlm_CA %>% 
  select(-origin_state_abr) %>% 
  droplevels()

str((mlm_all))


# MLM on Most logical variables

mlm1 <- lm(dep_delay ~ month + day_of_week + dest_state_abr + dep_time +
             airline + taxi_out + origin_airport_code + dest_airport_code +
             air_time + taxi_in + manufacturer + model + age +
             emptotal, data = mlm_all)
summary(mlm1) # R-Squared 0.03944 p < 2.2e-16
plot(mlm1)


mlm2 <- lm(dep_delay~ ., data = mlm_CA_sub3)
summary(mlm2)
plot(mlm2)

summary_test

summary_txt <- summary(mlm1)

sink('second_summary.txt')
summary_txt
sink()

# Model diagnoistic 

pairs(mlm_CA)
corrplot(cor(mlm_CA_sub3), method = "number")