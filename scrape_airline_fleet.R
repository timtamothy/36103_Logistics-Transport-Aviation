# Load Packages
library(tidyverse)
library(rvest)
library(feather)
library(here)

# Explicit URL
url <- 'https://www.planemapper.com/airlines/AAL'

AA <- url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="aircrafts"]') %>% 
  html_table()

AA_fleet <- AA[[1]]

View(AA_fleet)

# Write AA_fleet to feather
write_feather(AA_fleet, here('AA_fleet.feather'))

https://www.planemapper.com/airlines/SWA

# Explicit URL
swaurl <- 'https://www.planemapper.com/airlines/SWA'

SWA <- swaurl %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="aircrafts"]') %>% 
  html_table()

SWA_fleet <- SWA[[1]]

View(SWA_fleet)

# Write AA_fleet to feather
write_feather(AA_fleet, here('SWA_fleet.feather'))

https://www.planemapper.com/airlines/JBU
https://www.planemapper.com/airlines/DAL
https://www.planemapper.com/airlines/ASA
https://www.planemapper.com/airlines/HAL
https://www.planemapper.com/airlines/AAY
https://www.planemapper.com/airlines/EDV
https://www.planemapper.com/airlines/NKS
https://www.planemapper.com/airlines/SKW
https://www.planemapper.com/airlines/USA
https://www.planemapper.com/airlines/UAL
https://www.planemapper.com/airlines/QXE