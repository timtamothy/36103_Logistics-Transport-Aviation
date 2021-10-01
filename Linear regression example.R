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


mlm_final <- read_feather(here('Dataset', 'mlm_dataset_3.feather' ))

# sqrt transoformation 
mlm_final$dep_delay <- sqrt(mlm_final$dep_delay)

#filter variables/ subset it - Think of reasons why these subset variables 

mlm_CA <- mlm_final %>% filter(origin_state_abr == 'CA')

mlm_CA_sub1 <- mlm_CA %>% select(-origin_state_abr)

mlm_CA_sub2 <- mlm_CA_sub1 %>% select(dep_delay, origin_airport_code, dest_airport_code)

# drop levels to include only CA filter
mlm_CA_sub2 <- droplevels(mlm_CA_sub2)

str(mlm_CA_sub2)


mlm_CA_sub3 <- mlm_CA_sub1 %>% select(dep_delay, origin_airport_code)
mlm_CA_sub3 <- droplevels(mlm_CA_sub3)

cor(mlm_CA_sub3$dep_delay,mlm_CA_sub3$origin_airport_code)



cor(mlm_CA$dep_delay, mlm_CA$month)

lm()

# ========================
# Correlation test

mlm_num <- mlm_CA_sub1 %>% select(dep_delay, dep_time, arr_delay, air_time, air_time,taxi_out,taxi_in)
pairs(mlm_num)
uniquedep <- unique(mlm_num$dep_time)

plot(uniquedep)  
# =========================

# Chi Square test
data_chi <- data.frame(mlm_CA$manufacturer, mlm_CA$airline)
data_chi <- table(mlm_CA$manufacturer, mlm_CA$airline)

chisq.test(data_chi)

data_chi_w <- data.frame(mlm_final$manufacturer, mlm_final$airline)
data_chi_w <- table(mlm_final$manufacturer, mlm_final$airline)

chisq.test(data_chi_w)

# Correlation test notes 
# cor function only works among numerical values, 
# numerical variable + dichotomous variable (categorical variable after dummy) - called point biseral correlation test fyi

# Numerical data transformation 
data <- xyz#change this to your data name 
trimming <- FALSE #change this to true if you want to trim data 

check_num <- sapply(data, is.numeric)

if (TRUE %in% check_num){
  data$id <- seq.int(nrow(data))
  num_col <- select_if_(data, is.numeric) %>% drop_na_()
  
  if (trimming = TRUE){
    #This trims everything (need to change the condition if you only want to trim a subset of columns)
    for (i in (seq_along(data)-1)) {
      if (max(num_col[,i]) > (mean(unlist(num_col[,i])) + 4* sd(unlist(num_col[,i])))) {
        num_col <- num_col %>% arrange(num_col[,i])
        num_col <- num_col[round(nrow(num_col) *0.01, digits = 0):round(nrow(num_col)* (1-0.01) , digits = 0),]
      }
    }
  } else{
    num_col_norm <- as.data.frame(lapply(num_col, min_max_norm))
    cat_col <- data %>% select_if_(negate(is.numeric))
    cat_col$id <- seq.int(nrow(data))
    combine_data <- left_join(num_col_norm, cat_col, by = 'id')
    data <- combine_data %>% select_(-id)
  }
}

xyz <- data

#fit data 
mlm1 <- lm(dep_delay ~ ., data = mlm_CA_sub2)
summary(mlm1)
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