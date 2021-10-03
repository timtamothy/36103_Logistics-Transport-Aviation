library(tidyverse)
library(DataExplorer)
library(here)
library(funModeling)
library(Hmisc)
library(janitor)
library(feather)
library(future.apply)
library(data.table)
library(ggplot2)
library(dplyr)

# Load data ----
allmonths <- read_feather(here('Dataset', 'USE MLM 5 mlm_dataset_3.feather'))

# Overall status
status(allmonths)
summary_all <- summary(allmonths)
summary_all <- as.data.frame(summary_all)
write_csv(summary_all, here('column_summary_statistics.csv'))

hist(allmonths$dep_delay,
     main = "Histogram of Departure Delay",
     xlab = "Departure Delay",
     breaks = 20)
boxplot(allmonths$dep_delay)

hist(allmonths$day_of_week,
     main = "Histogram of Day of the week",
     xlab = "Day of the week")

agg <- count_(allmonths, names(allmonths))

head(agg)

barplot(count(allmonths$origin_state_abr),
     main = "Barplot of Origin State Abbr",
     xlab = "Origin State Abbr")


boxplot(allmonths$day_of_week)

ggplot(allmonths,mapping = aes(x = allmonths$dep_delay)) +
  geom_density() +
  geom_vline(aes(xintercept=mean(allmonths$dep_delay)),
             color = "blue", linetype = "dashed", size = 1) +
  xlab("Departure Delay")


hist(allmonths$arr_delay,
     main = "Histogram of Arrival Delay",
     xlab = "Arrival Delay")


boxplot(allmonths$arr_delay)

hist(allmonths$air_time,
     main = "Histogram of Air Time",
     xlab = "Air Time")


boxplot(allmonths$air_time)


hist(allmonths$taxi_out,
     main = "Histogram of Taxi Out",
     xlab = "Taxi Out")


hist(allmonths$taxi_in,
     main = "Histogram of Taxi In",
     xlab = "Taxi In")

view(summary_all)