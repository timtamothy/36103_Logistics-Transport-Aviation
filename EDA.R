library(tidyverse)
library(here)

library(httr)
library(jsonlite)
library(dplyr)
library(magrittr)


# Load data ----

delays <- read_csv(here("Dataset", "delays.csv"))

# View columns

names(delays)

# Plot histogram of delays

delays %>% 
  filter(ARR_DELAY < 300) %>% 
  ggplot(aes(x=ARR_DELAY)) +
  geom_histogram()

# Plot some relationships

delays %>% # delay as a function of distance
  filter(ARR_DELAY < 300) %>% 
  ggplot(aes(y=ARR_DELAY, x=DISTANCE)) +
  geom_point() 

delays %>% # plot count of distances, histogram?
  filter(ARR_DELAY < 300) %>% 
  ggplot(aes(x="DISTANCE")) +
      #need to change distance to continuous variable
  geom_bar()

delays %>% # histogram of elapsed flight times
  filter(ARR_DELAY < 300) %>% 
  ggplot(aes(x=ACTUAL_ELAPSED_TIME)) +
  geom_histogram()

delays %>% # delay as a function of flight time
  filter(ARR_DELAY < 300) %>% 
  ggplot(aes(y=ARR_DELAY, x=ACTUAL_ELAPSED_TIME)) +
  geom_point() 

delays %>% # may have collinearity
  filter(ARR_DELAY < 300) %>% 
  ggplot(aes(y=ACTUAL_ELAPSED_TIME, x=DISTANCE)) +
  geom_point() 
    #appears to have pretty strong collinearity?


