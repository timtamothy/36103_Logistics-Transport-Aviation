library(tidyverse)
library(here)

library(httr)
library(jsonlite)
library(dplyr)
library(magrittr)
library(feather)


# Load data ----

delays <- read_feather(here("mlm_dataset_4time.feather"))

delays_reduced <- delays %>% sample_n(500000)

# View columns

names(delays)

# Plot histogram of delays

delays %>% 
  filter(dep_delay < 300) %>% 
  ggplot(aes(x=dep_delay)) +
  geom_histogram()

# Plot some relationships

delays %>% # delay as a function of flight time
  #filter(arr_delay > 0) %>% 
  #filter(arr_delay < 60) %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=arr_delay, x=air_time, col = airline)) +
  geom_point(alpha=0.1, show.legend = F) +
  #geom_smooth(method='lm', show.legend = F) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal() +
  ylim(0, 150) +
  labs(title = 'Arrival Delay by Flight Time',
       x = 'Flight Time (mins)',
       y = 'Arrival Delay (mins)')

slm <- lm(arr_delay ~ taxi_out, data = delays)
coef(slm)
summary(slm)

plot(slm)

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

delays %>% 
  ggplot() +
  geom_bar(aes(x = manufacturer, fill = airline)) +
  theme_minimal() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_minimal() + 
  labs(title = 'Aircraft Manufacturer Count',
       x = 'Manufacturer',
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

  #remove embraer and mcdonnell as they represent very low values;
  #mcdonnell no longer in service

delays %>% 
  filter(manufacturer %in% c('boeing', 'airbus')) %>% 
  count(age, model) %>% 
  ggplot(aes(x = age, y = model)) +
  geom_tile(aes(fill = n))
  # 2 groups of ages appear. 
  # A320 proedominantly older craft
  # A321 predominatly younger craft
  # feature engineer buckets < 15, > 15
  
delays %>% # delay as a function of age
  filter(manufacturer %in% c('boeing', 'airbus')) %>%
  filter(model != '717') %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=arr_delay, x=age, col = airline)) +
  geom_point(alpha=0.1, show.legend = F) +
  geom_smooth(method='lm', show.legend = F) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal() +
  ylim(0, 150) +
  labs(title = 'Arrival Delay by Aircraft Age',
       x = 'Age (years)',
       y = 'Arrival Delay (mins)')

delays %>% # delay as a function of age
  filter(manufacturer %in% c('boeing', 'airbus')) %>%
  filter(model != '717') %>% 
  sample_n(50000) %>% 
  ggplot(aes(y=air_time, x=age, col = airline)) +
  geom_point(alpha=0.1, show.legend = F) +
  #geom_smooth(method='lm', show.legend = F) +
  facet_wrap(~airline) +
  scale_color_viridis_d() +
  theme_minimal() +
  ylim(0, 150) +
  labs(title = 'Flight time by Aircraft Age',
       x = 'Aircraft Age (years)',
       y = 'Flight Time (mins)')


delays %>% 
  filter(manufacturer %in% c('boeing', 'airbus')) %>% 
  filter(dep_delay > 0) %>% 
  filter(year %in% c('2019')) %>% 
  count(month, dep_delay) %>% 
  ggplot(aes(x = month, y = dep_delay)) +
  geom_tile(aes(fill = n)) +
  labs(title = 'Flight Delays by Month',
       x = 'Month',
       y = 'Departure Delay (mins)')

library(funModeling)
library(Hmisc)
library(DataExplorer)

freq(delays %>% filter(year == '2019'))
freq(delays %>% filter(year == '2020'))

delays %>% 
  filter(manufacturer %in% c('boeing', 'airbus')) %>% 
  filter(dep_delay > 0) %>% 
  filter(dest_state_abr == 'CA') %>% 
  #filter(year %in% c('2019')) %>% 
  #sample_n(50000) %>% 
  #count(month, dep_delay) %>% 
  ggplot(aes(x = dep_time, y = dep_delay)) +
  geom_hex() +
  #scale_fill_viridis_d() +
  #geom_point(shape = 'o', col = 'blue', alpha = 0.1) +
  facet_wrap(~airline) +
  theme_minimal() +
  labs(title = 'Flight Delays by Time of Day in California',
       x = 'Time',
       y = 'Departure Delay (mins)')

delays %>% 
  filter(manufacturer %in% c('boeing', 'airbus')) %>% 
  filter(dep_delay > 0) %>% 
  filter(dest_state_abr == 'TX') %>% 
  #filter(year %in% c('2019')) %>% 
  #sample_n(50000) %>% 
  #count(month, dep_delay) %>% 
  ggplot(aes(x = dep_time, y = dep_delay)) +
  geom_hex() +
  #scale_fill_viridis_d() +
  #geom_point(shape = 'o', col = 'blue', alpha = 0.1) +
  facet_wrap(~airline) +
  theme_minimal() +
  labs(title = 'Flight Delays by Time of Day in Texas',
       x = 'Time',
       y = 'Departure Delay (mins)')


delays %>% 
  filter(manufacturer %in% c('boeing', 'airbus')) %>% 
  filter(dep_delay > 0) %>% 
  filter(dest_state_abr == 'TX') %>% 
  #filter(year %in% c('2019')) %>% 
  #sample_n(50000) %>% 
  #count(month, dep_delay) %>% 
  ggplot(aes(x = dep_time, y = dep_delay)) +
  geom_hex() +
  #scale_fill_viridis_d() +
  #geom_point(shape = 'o', col = 'blue', alpha = 0.1) +
  facet_wrap(~airline) +
  theme_minimal() +
  labs(title = 'Flight Delays by Time of Day in Illinois',
       x = 'Time',
       y = 'Departure Delay (mins)')

library(funModeling)
library(Hmisc)
library(DataExplorer)
status(delays)
summary(delays)

#delays %>% # plot count of distances, histogram?
  #filter(ARR_DELAY < 300) %>% 
  #ggplot(aes(x="air_time")) +
      #need to change distance to continuous variable
  #geom_bar()

#delays %>% # histogram of elapsed flight times
 # filter(arr_delay < 300) %>% 
  #ggplot(aes(x=ACTUAL_ELAPSED_TIME)) +
  #geom_histogram()

#delays %>% # delay as a function of flight time
  #filter(ARR_DELAY < 300) %>% 
  #ggplot(aes(y=ARR_DELAY, x=ACTUAL_ELAPSED_TIME)) +
  #geom_point() 

#delays %>% # may have collinearity
  #filter(ARR_DELAY < 300) %>% 
  #ggplot(aes(y=ACTUAL_ELAPSED_TIME, x=DISTANCE)) +
  #geom_point() 
    #appears to have pretty strong collinearity?


