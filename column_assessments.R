# load packages ----
library(tidyverse)
library(DataExplorer)
library(here)
library(funModeling)
library(Hmisc)
library(janitor)

# Load data ----
allmonths <- read_feather(here('clean_3airline_fleet_employ.feather'))

# Overall status
status(allmonths)

# Categorical overall
freq(allmonths, path_out = here('Column EDA', '.'))

# Numerical Overall
plot_num(allmonths, path_out = here('Column EDA', '.'))

# Check distribution -- justtify normalization in feature engineering 


#only numeric ones 

#will do so on 27th sep 
#create unique id 
num_col <- select_if(allmonths, is.numeric)

#i in seq_along(num_col)

for (i in 1:8) {
  if (max(num_col[,i]) > (mean(unlist(num_col[,i])) + 4* sd(unlist(num_col[,i])))) {
    num_col <- num_col %>% arrange(num_col[,i])
    num_col <- num_col[round(nrow(num_col) *0.01, digits = 0):round(nrow(num_col)* (1-0.01) , digits = 0),]
  }
}

#square root transformation 

#Min-max transformation 
min_max_norm <- function(x){
  (x-min(x)) / (max(x) - min(x))
}
num_col_norm <- as.data.frame(lapply(num_col, min_max_norm))

plot_num(num_col_norm)

#Pearson correlation test (heatmap)


#chi-square independence test 

#Point-biserial correlation test 

#validation (train,test, validation test)