library(feather)
library(here)
library(tidyverse)
library(future.apply)


feather2_df <- read_feather("clean.feather")

air_coord <- read.csv(file = "air_coord2.csv")

## Merge Origin latitude and Longitude by AIRPORT

o_air_coord <- select(air_coord, ORIGIN_AIRPORT_CODE, latitude, longitude)

d_air_coord <- select(air_coord, DEST_AIRPORT_CODE, latitude, longitude)

leftJoin_origin_air <- left_join(x = feather2_df, y = o_air_coord, by="ORIGIN_AIRPORT_CODE")

names(leftJoin_origin_air)[names(leftJoin_origin_air) == "latitude"] <- "ORIGIN_AIRPORT_LATITUDE"

names(leftJoin_origin_air)[names(leftJoin_origin_air) == "longitude"] <- "ORIGIN_AIRPORT_LONGITUDE"

leftjoin_origin_air_1 <- leftJoin_origin_air %>% relocate(c("ORIGIN_AIRPORT_LATITUDE","ORIGIN_AIRPORT_LONGITUDE"), .after = "ORIGIN_AIRPORT_CODE")

## Merge Destination latitude and Longitude by AIRPORT

leftJoin_dest_air <- left_join(x = leftjoin_origin_air_1, y = d_air_coord, by="DEST_AIRPORT_CODE")

names(leftJoin_dest_air)[names(leftJoin_dest_air) == "latitude"] <- "DEST_AIRPORT_LATITUDE"

names(leftJoin_dest_air)[names(leftJoin_dest_air) == "longitude"] <- "DEST_AIRPORT_LONGITUDE"

leftjoin_dest_air_1 <- leftJoin_dest_air %>% relocate(c("DEST_AIRPORT_LATITUDE","DEST_AIRPORT_LONGITUDE"), .after = "DEST_AIRPORT_CODE")

write_feather(leftjoin_dest_air_1, "C:/Users/gerar/OneDrive/Escritorio/MDSI/STDS/AT2/EDA/EDA/clean_3airline_fleet_employ_coord.feather")

