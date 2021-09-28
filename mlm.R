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
library(MASS)

# Load Data ----
allmonths <- fread(here('dummy.csv'))

allmonths <- fread("C:/Users/tsetc/Downloads/dummy.csv")

# Select airports
airports <- fread(here('top50.csv'))
airport_list <- colnames(airports)



# Create first mlm ----
mlm1 <- lm(dep_delay ~ ., data=dat_transformed)

first_ml <- summary(mlm1)

sink('first_summary.txt')
first_ml
sink()

#Second linear regression model 
#just for test - use the dataset before using dummy variable 
num_transform <- fread(here('num_transform.csv'))

num_col <- select_if(num_transform, is.numeric)
num_col <- num_col %>% sample_n(10000)


library(corrplot)
corrplot(cor(num_col), method = 'number')
corrplot(cor(num_col), method = 'ellipse')

#Chi-square independence test to check for association
trial <- num_transform %>% select_if(negate(is.numeric)) %>% sample_n(10000)

data_chi <- data.frame(trial$manufacturer, trial$airline)
data_chi <- table(trial$manufacturer, trial$airline)
print(data_chi)

chisq.test(data_chi)

small <- num_transform %>% sample_n(10000)

#remove origin_state_abr, dest_state_abr, manufacturer

#Tim - from here 
num_transform <- fread(here('num_transform.csv'))
reg_2 <- num_transform %>% dplyr:: select(-c('origin_state_abr', 'dest_state_abr', 'manufacturer'))

#change to dummy variables 
dmy <- dummyVars(" ~ . ", data=reg_2, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = reg_2))

#write file 
fwrite(dat_transformed, here('reg_2.csv'))

#Tim till here 

allmonths <- fread(here('reg_2.csv'))

#chi-square independence test, dep_delay against other variables 
library(broom)
CHIS <- lapply(allmonths[,-1], function(x) chisq.test(allmonths[,1], x))
chi_result <- do.call(rbind, lapply(CHIS,tidy))

sink('chi_result.txt')
chi_result
sink()

#correlation test (including everything)
corr_result_all <- round(cor(allmonths), digits = 2)

sink('corr_result_all.txt')
corr_result_all
sink()
