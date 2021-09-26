# load packages ----
library(tidyverse)
library(DataExplorer)
library(here)
library(funModeling)
library(Hmisc)
library(janitor)

# Load data ----
allmonths <- read_feather(here('clean_3airline_fleet_employ.feather'))

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

<<<<<<< main
#Pearson correlation test (heatmap)
=======
  status(num_col_norm)
status(num_col)
status(allmonths)

# Individual Column Assessment
names(allmonths)

# Determine Outlier range for *delays only 1 min and greater*
onlydelay <- allmonths %>% 
  filter(dep_delay > 0)
Q3 <- quantile(onlydelay$dep_delay_new, .75)
IQR <- IQR(onlydelay$dep_delay_new)
Outlier <- Q3 + 1.5*IQR
Outlier
Q3

# Must use a package to make plotting faster
# filter out outliers of delays
allmonth_plot <- na.omit(allmonths)
allmonth_plot <- allmonth_plot %>% 
  filter(dep_delay <= 95)

allmonth_plot %>% 
  sample_n(50000) %>% 
  ggplot(aes(x=dep_delay, y=year)) +
  geom_point(alpha=0.3)

allmonth_plot %>% 
  sample_n(50000) %>% 
  ggplot(aes(x=dep_delay, y=month)) +
  geom_boxplot(alpha=0.2) +
  geom_point(alpha=0.3)

allmonth_plot %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=air_time)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')

allmonth_plot %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=distance)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')
# Check correlation to Air Time

allmonth_plot %>% 
  filter(air_time > 0) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=air_time, x=distance)) +
  geom_point(alpha = 0.3)


allmonth_plot %>% 
  filter(dep_delay > 0) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=age)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')

allmonth_plot %>% 
  filter(dep_delay < 0) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=age)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')

summary(allmonth_plot$age)
# median is 20 years of age anyway, no wonder we see most observations there.

allmonths %>% 
  filter(dep_delay > 95) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=age)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')
#even among outliers, not much else to say

allmonths %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=taxi_out)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')

allmonths %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=taxi_in)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')
#doesn't seem as though taxi in and out have much effect on delays
#perhaps longer taxi is because they were early?

allmonths %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=carrier_delay)) +
  geom_hex() +       
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')

allmonths %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=dep_delay, x=dep_time)) +
  scale_fill_viridis_c() +
  geom_point(alpha = 0.3)
#view when start of day is
#delays tend to increase later in the day























































>>>>>>> main


#chi-square independence test 

#Point-biserial correlation test 

#validation (train,test, validation test)