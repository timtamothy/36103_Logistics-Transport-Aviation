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


mlm_final <- read_feather(here('Dataset', 'mlm_dataset_3.feather' ))

#filter variables/ subset it - Think of reasons why these subset variables 

mlm_CA <- mlm_final %>% filter(origin_state_abr == 'CA')

mlm_CA_sub1 <- mlm_CA %>% select(-origin_state_abr)

mlm_CA_sub2 <- mlm_CA_sub1 %>% select(dep_delay, origin_airport_code, dest_airport_code)

# drop levels to include only CA filter
mlm_CA_sub2 <- droplevels(mlm_CA_sub2)

str(mlm_CA_sub2)

cor(mlm_CA$dep_delay, mlm_CA$month)

lm()


# Chi Square test
data_chi <- data.frame(mlm_CA$manufacturer, mlm_CA$airline)
data_chi <- table(mlm_CA$manufacturer, mlm_CA$airline)

chisq.test(data_chi)

data_chi_w <- data.frame(mlm_final$manufacturer, mlm_final$airline)
data_chi_w <- table(mlm_final$manufacturer, mlm_final$airline)

chisq.test(data_chi_w)



#Trim data + min-max transformation 
mlm_CA_sub2$id <- seq.int(nrow(mlm_CA_sub2))

num_col <- select_if(mlm_CA_sub2, is.numeric)

num_col <- num_col %>% drop_na()

#Optional for trim data 
for (i in 1:9) {
  if (max(num_col[,i]) > (mean(unlist(num_col[,i])) + 4* sd(unlist(num_col[,i])))) {
    num_col <- num_col %>% arrange(num_col[,i])
    num_col <- num_col[round(nrow(num_col) *0.01, digits = 0):round(nrow(num_col)* (1-0.01) , digits = 0),]
  }
}

min_max_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}
num_col_norm <- as.data.frame(future_lapply(num_col, min_max_norm))

cat_col <- mlm_CA_sub2 %>% select_if(negate(is.numeric))
cat_col$id <- seq.int(nrow(mlm_CA_sub2))

mlm_CA_sub2 <- left_join(num_col_norm, cat_col, by = 'id')

#dummy variable 
dmy <- dummyVars(" ~ . ", data=mlm_CA_sub2, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = mlm_CA_sub2))

#fit data 
mlm1 <- lm(dep_delay ~ ., data = dat_transformed)

summary_txt <- summary(mlm1)

sink('second_summary.txt')
summary_txt
sink()
