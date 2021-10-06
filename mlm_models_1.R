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


mlm_final <- read_feather(here('mlm_dataset_5.feather'))

# sqrt transoformation 
#mlm_final$dep_delay <- sqrt(mlm_final$dep_delay)


# Select California only as Subset

mlm_CA <- mlm_final %>% filter(origin_state_abr == 'CA')
names(mlm_CA)

mlm_all <- mlm_CA %>% 
  select(-origin_state_abr) %>% 
  droplevels()

str((mlm_all))

View(mlm)


# Numerical data transformation 
data <- mlm_all #change this to your data name 
trimming <- FALSE #change this to true if you want to trim data 



min_max_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}

num_col <- select_if(mlm_all, is.numeric)

num_col_norm <- as.data.frame(lapply(num_col, min_max_norm))
cat_col <- mlm_all %>% select_if(negate(is.numeric))
num_col_norm$id <- seq.int(nrow(mlm_all))
cat_col$id <- seq.int(nrow(mlm_all))
combine_data <- left_join(num_col_norm, cat_col, by = 'id')
mlm_all <- combine_data %>% select(-id)

names(mlm_CA)
names(mlm_all)

# MLM on Most logical variables

mlm1 <- lm(dep_delay ~ month + day_of_week + dest_state_abr + dep_time +
             airline + taxi_out + origin_airport_code + dest_airport_code +
             air_time + taxi_in + manufacturer + model + age +
             emptotal, data = mlm_all)
summary(mlm1) # R-Squared 0.03944 p < 2.2e-16
plot(mlm1)

# MLM removal

mlm2 <- lm(dep_delay ~ month + day_of_week + dep_time +
             airline + taxi_out + origin_airport_code + dest_airport_code +
             air_time + model + age, data = mlm_all)
summary(mlm2) # R Squared 0.03184
plot(mlm2)


# MLM removal 2

mlm3 <- lm(dep_delay ~ month + day_of_week + dep_time +
             airline + taxi_out + origin_airport_code + 
            model + age, data = mlm_all)
summary(mlm3) # R-Squared 0.02715

# Run Boruta
boruta_sasuke <- Boruta(dep_delay ~ month + day_of_week + dep_time +
                        airline + taxi_out + origin_airport_code + 
                        model + age, data = mlm_all, doTrace = 2)

boruta_signif <- names(boruta_sasuke$finalDecision[boruta_sasuke$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif)
plot(boruta_sasuke, cex.axis = .7,
     las = 2, xlab ="", main = "Variable Importance")


# MLM boruta removal

mlm4 <- lm(dep_delay ~ month + dep_time +
             airline + taxi_out + origin_airport_code + 
             model + age, data = mlm_all)
summary(mlm4) # R-Squared  .02705


mlm5 <- lm(dep_delay ~ month + dep_time +
             airline + age, data = mlm_all)
summary(mlm5) # R-Squared .01202 p-value <2.2e-16



mlm6 <- lm(arr_delay ~ month + day_of_week + dep_time +
             airline + taxi_out + dest_airport_code +
             air_time + taxi_in + manufacturer + age,
             data = mlm_all)

summary(mlm6)

#improve upon mlm6 but added interaction varaible 
mlm_7 <- lm(dep_delay ~ month + dep_time +
              airline + age + month:age + 
              airline:age + airline:dep_time
            , data = mlm_all)

summary(mlm_7)
#interaction variable not significant --> no need to add interaction variable

#sqrt transformation 
mlm_final$dep_delay <- sqrt(mlm_final$dep_delay)

#VIF 
VIF(mlm5)
A1/VIF(mlm5)

summary_test

summary_txt <- summary(mlm1)

sink('second_summary.txt')
summary_txt
sink()

# Model diagnoistic 

pairs(mlm_CA)
corrplot(cor(mlm_CA_sub3), method = "number")



# Residual Plot on MLM 5

plot(mlm5)
