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
summary_all <-  summary(allmonths)
summary_all <- as.data.frame(summary_all)
write_csv(summary_all, here('column_summary_statistics.csv'))

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

num_col <- num_col %>% drop_na()

#i in seq_along(num_col)

num_col <- num_col %>% 
  select('dep_delay', 'dep_delay_new', 'taxi_out', 'taxi_in',
         'arr_delay', 'arr_delay_new', 'air_time', 'distance',
         'age', 'carrier_delay', 'weather_delay', 'nas_delay',
         'security_delay', 'late_aircraft_delay', 'total_add_gtime',
         'empfull', 'emppart', 'emptotal', 'empfte')

for (i in 1:9) {
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








































































