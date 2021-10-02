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
library(PerformanceAnalytics)
library(Hmisc)
# ========================

# Correlation test 
mlm_corr <- read_feather(here('Dataset', 'USE MLM 5 mlm_dataset_3.feather' ))
# get a random sample of 50,000 observations
mlm_corr_n <- mlm_corr %>% sample_n(50000)

#get numeric variables
mlm_corr_n <- mlm_corr_n %>% select(dep_delay,month,day_of_week, dep_time, arr_delay, air_time, air_time,taxi_out,taxi_in,age)

corr_matrix <- cor(mlm_corr_n, method = "pearson", use = "complete.obs")
corr_matrix <-round(corr_matrix,3)

corr_matrix

corrplot(corr_matrix, method = "ellipse")
corrplot(corr_matrix, method = "pie")

corr_matrix_r <- rcorr(corr_matrix, type = c("pearson","spearman"))
corrplot(corr_matrix_r$r, type = "upper", order = "hclust", 
         p.mat = corr_matrix_r$P, sig.level = 0.01, insig = "blank")

chart.Correlation(corr_matrix, histogram = TRUE, pch = 19)

# =========================