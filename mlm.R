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
allmonths <- fread("C:/Users/tsetc/Downloads/dummy_numtransform.csv")


num_col <- select_if(num_transform, is.numeric)
num_col <- num_col %>% sample_n(10000)


library(corrplot)
corrplot(cor(num_col), method = 'number')
corrplot(cor(num_col), method = 'ellipse')

#Chi-square independence test to check for association
trial <- num_transform %>% select_if(negate(is.numeric)) %>% sample_n(10000)

data_chi <- data.frame(trial$origin_airport_code, trial$origin_state_abr)
data_chi <- table(trial$origin_airport_code, trial$origin_state_abr)
print(data_chi)

chisq.test(trial$origin_airport_code, trial$origin_state_abr)

#Replace NA values with 0 (due to dummy variables)
allmonths[is.na(allmonths)] = 0

cat_col <- allmonths[,12:177]
cat_col <- cat_col %>% sample_n(10000)

CHIS <- lapply(cat_col[,-1], function(x) chisq.test(cat_col[,1], x))

do.call(rbind, future_lapply(CHIS, tidy))

