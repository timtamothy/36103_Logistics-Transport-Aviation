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
library(data.table)
library(regclass)
library(Boruta)

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

#remove destination airport 
names(allmonths)
allmonths <- allmonths[,-c(59:105)]


#daily flight at a certain airport, linear trend (time series data)
str(allmonths$dep_time)
#departure time is integer 

#specific ariport - LAX
allmonths <-  allmonths %>% filter(origin_airport_codeKLAX == 1)

#remove origin airport
allmonths <- allmonths[,-c(13:58)]

names(allmonths)
#remove time x variables 
reg_fit1 <- allmonths[,-c(5:7, 12)]

# make sure no negative values dep_delay for dataset 
reg_fit1 <- reg_fit1 %>% mutate(dep_delay_new = ifelse(dep_delay >0, dep_delay, 0)) %>% dplyr::select(-dep_delay)

#factor analysis 

#fit mlm1 model 
mlm1 <- lm(dep_delay_new ~ ., data=reg_fit1)

summary(mlm1)
#VIF 
1/VIF(mlm1)
#VIF cannot run because there is perfect multicollinearity 

alias(lm(dep_delay ~ ., data=reg_fit1))

#remove airline

reg_fit2 <- reg_fit1[,-c(5:6, 9:21)]


#fit mlm2 model 
mlm2 <- lm(dep_delay_new ~ ., data=reg_fit2)
summary(mlm2)
#VIF
1/VIF(mlm2)
#no multicollinearity


#resiudals
plan(multisession)
#plot(mlm1)
plot(mlm2)


#variable transformation on y
reg_fit3 <- reg_fit2
reg_fit3$dep_delay_new <- sqrt(reg_fit3$dep_delay_new)

#fit mlm3 model 
mlm3 <- lm(dep_delay_new ~ ., data=reg_fit3)

summary(mlm3)
plot(mlm3)

#even after sqrt transformation, variable transformation on x as well
reg_fit4 <- reg_fit2

reg_fit4 <- data.frame(sapply(reg_fit4, sqrt))


#fit mlm4 model
mlm4 <- lm(dep_delay_new ~ ., data = reg_fit4)
summary(mlm4)

plan(multisession)
plot(mlm4)


#Variable selection 
Boruta(dep_delay ~ ., data=reg_fit, doTrace = 2)



#chi-square independence test, dep_delay against other variables 
library(broom)
CHIS <- lapply(allmonths[,-1], function(x) chisq.test(allmonths[,1], x))
chi_result <- do.call(rbind, lapply(CHIS,tidy))


#correlation test (including everything)
corr_result_all <- round(cor(allmonths), digits = 2)

sink('corr_result_all.txt')
corr_result_all
sink()
