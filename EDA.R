library(tidyverse)
library(here)
library(httr)
library(jsonlite)
library(dplyr)
library(magrittr)
library(feather)


# Load data ----

delays <- read_feather(here("USE MLM 5 mlm_dataset_3.feather"))
delays_full <- read_feather(here("clean_allairlines_allmonths.feather"))


delays_reduced <- delays %>% sample_n(500000)

# View columns

names(delays)

# delays less than 30

delays %>% 
  filter(dep_delay < 300, dep_delay > 15) %>%
  ggplot(aes(x=dep_delay)) +
  geom_histogram()

# delays between 15 and 90

delays %>% 
  filter(dep_delay < 90, dep_delay > 15) %>%
  ggplot(aes(x=dep_delay)) +
  geom_histogram()

# delays vs flight time

delays %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=arr_delay, x=air_time, col = airline)) +
  geom_point(alpha=0.1, show.legend = F) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal() +
  ylim(0, 60) +
  labs(title = 'Arrival Delay vs Flight Time',
       x = 'Flight Time in minutes',
       y = 'Arrival Delay in minutes')

##slm <- lm(arr_delay ~ taxi_out, data = delays3)
##coef(slm)
##summary(slm)

##plot(slm)

delays %>% # delay as a function of taxi_out
  #filter(arr_delay > 0) %>% 
  #filter(arr_delay < 60) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=arr_delay, x=taxi_out, col = airline)) +
  geom_point(alpha=0.1, show.legend = F) +
  geom_smooth(method='lm', show.legend = F) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal() +
  ylim(0, 150) +
  labs(title = 'Arrival Delay by Taxi Out Time',
       x = 'Taxi Out (mins)',
       y = 'Arrival Delay (mins)')


delays %>% # delay as a function of taxi_in
  #filter(arr_delay > 0) %>% 
  #filter(arr_delay < 60) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=arr_delay, x=taxi_in, col = airline)) +
  geom_point(alpha=0.1, show.legend = F) +
  #geom_smooth(method='lm', show.legend = F) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal() +
  ylim(0, 150) +
  labs(title = 'Arrival Delay by Taxi In Time',
       x = 'Taxi In (mins)',
       y = 'Arrival Delay (mins)')

# Plot some Categorical Relationships:

delays %>% 
  filter(dep_delay > 0) %>% 
  ggplot() +
  geom_violin(aes(x=manufacturer, y=dep_delay, col = airline)) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal()


delays_full %>% # me experiment
  filter(year == '2019' | year == '2020') %>%
  ggplot() +
  geom_bar(aes(x = month, fill = year)) +
  theme_minimal() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal() + 
  labs(title = 'Flight Frequency per month and year',
       x = 'Months',
       y = 'Number of flights')

delays %>% 
  ggplot() +
  geom_bar(aes(x = model, fill = airline)) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal() + 
  labs(title = 'Aircraft Model Count',
       x = 'Model',
       y = 'Number of flights')


## American Airlines Hubs

delays %>% 
  filter(origin_airport_code == 'KDFW',
         origin_airport_code == 'KCLT',
         origin_airport_code == 'KORD',
         origin_airport_code == 'KLAX',
         origin_airport_code == 'KMIA',
         origin_airport_code == 'KJFK',
         origin_airport_code == 'KLGA',
         origin_airport_code == 'KPHL',
         origin_airport_code == 'KPHX',
         origin_airport_code == 'KDCA') %>% 
  ggplot() +
  geom_bar(aes(x = origin_airport_code)) +
  theme_minimal() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal() 







