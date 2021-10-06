library(tidyverse)
library(here)
library(dplyr)
library(magrittr)
library(feather)

# Load latest data

delays_aircrafts <- read_feather(here("mlm_dataset_4time.feather"))

# View columns

names(delays_aircrafts)

# View manufacturers

unique(delays_aircrafts$manufacturer)

# Filter by manufacturer

delays_aircrafts <- delays_aircrafts %>% 
  filter(manufacturer %in% c('airbus', 'boeing')) %>% 
  droplevels()

# Check manufacturers

unique(delays_aircrafts$manufacturer)

# Check models

unique(delays_aircrafts$model)

# Save as file

write_feather(delays_aircrafts, here('mlm_dataset_5.feather'))

# Bin age

bin <- delays_aircrafts %>% 
  mutate(old_plane = age >= 15) %>% 
  mutate(new_plane = age < 15)

bin %>% 
  sample_n(50000) %>% 
  ggplot(aes(x = old_plane, y = dep_delay)) +
  geom_boxplot()
