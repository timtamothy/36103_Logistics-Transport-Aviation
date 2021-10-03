library(feather)
library(here)
library(tidyverse)
library(future)


feather_df <- read_feather("clean_3airline_fleet_employ.feather")

state_coord <- read.csv(file = "state_coord.csv")

air_coord <- read.csv(file = "air_coord.csv")

## Merge Origin latitude and Longitude by state

o_state_coord <- select(state_coord, ORIGIN_STATE_ABR, latitude, longitude)

d_state_coord <- select(state_coord, DEST_STATE_ABR, latitude, longitude)

leftJoin_origin <- left_join(x = feather_df, y = o_state_coord, by="ORIGIN_STATE_ABR")

names(leftJoin_origin)[names(leftJoin_origin) == "latitude"] <- "ORIGIN_STATE_LATITUDE"

names(leftJoin_origin)[names(leftJoin_origin) == "longitude"] <- "ORIGIN_STATE_LONGITUDE"

leftjoin_origin_1 <- leftJoin_origin %>% relocate(c("ORIGIN_STATE_LATITUDE","ORIGIN_STATE_LONGITUDE"), .after = "ORIGIN_STATE_ABR")

## Merge Destination latitude and Longitude by state

leftJoin_dest <- left_join(x = leftjoin_origin_1, y = d_state_coord, by="DEST_STATE_ABR")

names(leftJoin_dest)[names(leftJoin_dest) == "latitude"] <- "DEST_STATE_LATITUDE"

names(leftJoin_dest)[names(leftJoin_dest) == "longitude"] <- "DEST_STATE_LONGITUDE"

leftjoin_dest_1 <- leftJoin_dest %>% relocate(c("DEST_STATE_LATITUDE","DEST_STATE_LONGITUDE"), .after = "DEST_STATE_ABR")

## Write feather

write_feather(leftjoin_dest_1, "C:/Users/gerar/OneDrive/Escritorio/MDSI/STDS/AT2/EDA/EDA/clean.feather")


