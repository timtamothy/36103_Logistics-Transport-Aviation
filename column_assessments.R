# load packages ----
library(tidyverse)
library(DataExplorer)
library(here)
library(funModeling)
library(Hmisc)
library(janitor)
library(feather)
library(future.apply)

# Load data ----
allmonths <- read_feather(here('clean_3airline_fleet_employ_2.feather'))

# Overall status
status(allmonths)

# Categorical overall
freq(allmonths, path_out = here('Column EDA', '.'))

# Numerical Overall
plot_num(allmonths, path_out = here('Column EDA', '.'))
     

Q1 <- quantile(allmonths$DEP_DELAY, .25)
Q3 <- quantile(allmonths$DEP_DELAY, .75)
IQR <- IQR(allmonths$DEP_DELAY)

allmonths2 <- subset(allmonths, allmonths$DEP_DELAY > (Q1 - 1.5*IQR) & 
                      allmonths$DEP_DELAY< (Q3 + 1.5*IQR))

Q1 <- quantile(allmonths$ARR_DELAY, .25)
Q3 <- quantile(allmonths$ARR_DELAY, .75)
IQR <- IQR(allmonths$ARR_DELAY)

allmonths2 <- subset(allmonths2, allmonths2$ARR_DELAY > (Q1 - 1.5*IQR) & 
                       allmonths2$ARR_DELAY< (Q3 + 1.5*IQR))

Q1 <- quantile(allmonths$ARR_DELAY_NEW, .25)
Q3 <- quantile(allmonths$ARR_DELAY_NEW, .75)
IQR <- IQR(allmonths$ARR_DELAY_NEW)

allmonths2 <- subset(allmonths2, allmonths2$ARR_DELAY_NEW > (Q1 - 1.5*IQR) & 
                       allmonths2$ARR_DELAY_NEW< (Q3 + 1.5*IQR))

plot_num(allmonths2)

# Charlotte code
num_col <- select_if(allmonths, is.numeric)
num_col <- num_col %>%

#i in seq_along(num_col)

for (i in 1:8) {
  if (max(num_col[,i]) > (mean(unlist(num_col[,i])) + 4* sd(unlist(num_col[,i])))) {
    num_col <- num_col %>% arrange(num_col[,i])
    num_col <- num_col[round(nrow(num_col) *0.01, digits = 0):round(nrow(num_col)* (1-0.01) , digits = 0),]
  }
}

#Min-max transformation 
min_max_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}
num_col_norm <- as.data.frame(future_lapply(num_col, min_max_norm))

plot_num(num_col_norm)
