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
library(Boruta)
library(regclass)

# 1. Load Data -----
mlm_final <- read_feather(here('mlm_dataset_5.feather'))

summary(mlm_final)
#dep delay median is -3
#select delays greater than 0

blm_bino <- mlm_final %>% 
  filter(dep_delay > 0)

summary(blm_bino)
#dep delay median is 12, mean 20.8. 1Q 4, 3Q 30


# 2. Create new binomial delay

blm_bino <- blm_bino %>% 
  mutate(delay_new = dep_delay > 12)

summary(blm_bino)
#344677 true, #356513 false


# 3. Train/Test set data

blm_bino$id <- 1:nrow(blm_bino)
blm_bino.train <- blm_bino %>% dplyr::sample_frac(0.75)
blm_bino.test <- dplyr::anti_join(blm_bino, blm_bino.train, by ='id')


# 3. GLM model

glm1 <- glm(delay_new ~ month + dep_time +
              airline + taxi_out + 
              model + age, family = binomial(logit), data = mlm_bino.train)
summary(glm1)


# 4. Model only for AA

blm_aa.train <- blm_bino.train %>% 
  filter(airline == 'American Airlines Inc.')

blm_aa.test <- blm_bino.test %>% 
  filter(airline == 'American Airlines Inc.')

# 5. GLM model for AA

blm2 <- glm(delay_new ~ month + dep_time +
              taxi_out + model + age, 
              family = binomial(logit), data = blm_aa.train)
summary(blm2)


# 6. Model only for DL

blm_dl.train <- blm_bino.train %>% 
  filter(airline == 'Delta Air Lines Inc.')

blm_dl.test <- blm_bino.test %>% 
  filter(airline == 'Delta Air Line Inc.')


# 7. GLM model for DL

blm3 <- glm(delay_new ~ month + dep_time +
              taxi_out + model + age, 
            family = binomial(logit), data = blm_dl.train)
summary(blm3)

# 8. Look at VIF for models
VIF(glm1)
VIF(blm2)
VIF(blm3)

# 9 model validation

probability_aa <- predict(blm2, newdata = blm_aa.test, type='response')
prediction_aa <- ifelse(probability_aa > 0.5, 1, 0)
confusion_aa <- table(blm_aa.test$delay_new, prediction_aa)

confusion_aa
