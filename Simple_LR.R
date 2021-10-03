library(feather)
library(here)
library(tidyverse)
library(magrittr)
library(corrplot)
library(psych)


feather_df <- read_feather("clean_allairlines_allmonths.feather")
feather_mlm_df <- read_feather("mlm_dataset_4.feather")

feather_mlm_df_delay <- feather_mlm_df %>%
  filter(dep_delay > 0)

feather_mlm_df_delay15 <- feather_mlm_df %>%
  filter(dep_delay > 15)

names(feather_mlm_df)

summary(feather_mlm_df)

ggplot(data = feather_mlm_df, aes(x = day_of_week, y = dep_delay)) + geom_point()

ggplot(data = feather_mlm_df, aes(x = dep_time, y = dep_delay)) + geom_point()

ggplot(data = feather_mlm_df, aes(x = age, y = dep_delay)) + geom_point()

ggplot(data = feather_mlm_df, aes(x = air_time, y = dep_delay)) + geom_point()

ggplot(data = feather_mlm_df, aes(x = arr_delay, y = dep_delay)) + geom_point()

ggplot(data = feather_mlm_df_delay, aes(x = arr_delay, y = dep_delay)) + geom_point()

ggplot(data = feather_mlm_df_delay15, aes(x = arr_delay, y = dep_delay)) + geom_point()

cor(feather_mlm_df$day_of_week, feather_mlm_df$dep_delay)

cor(feather_mlm_df$dep_time, feather_mlm_df$dep_delay)

cor(feather_mlm_df$age, feather_mlm_df$dep_delay)

cor(feather_mlm_df$air_time, feather_mlm_df$dep_delay)

cor(feather_mlm_df$arr_delay, feather_mlm_df$dep_delay)

cor(feather_mlm_df_delay$arr_delay, feather_mlm_df_delay$dep_delay)

cor(feather_mlm_df_delay15$arr_delay, feather_mlm_df_delay15$dep_delay)

slm <- lm(day_of_week ~ dep_delay, data = feather_mlm_df)

slm2 <- lm(dep_time ~ dep_delay, data = feather_mlm_df)

slm3 <- lm(age ~ dep_delay, data = feather_mlm_df)

slm4 <- lm(air_time ~ dep_delay, data = feather_mlm_df)

slm5 <- lm(arr_delay ~ dep_delay, data = feather_mlm_df)

slm6 <- lm(arr_delay ~ dep_delay, data = feather_mlm_df_delay)

slm7 <- lm(arr_delay ~ dep_delay, data = feather_mlm_df_delay15)

coef(slm)

coef (slm2)

coef (slm3)

coef (slm4)

coef (slm5)

coef (slm6)

coef (slm7)

ggplot(data = feather_mlm_df_delay15, aes(x = arr_delay, y = dep_delay)) + geom_point()+ 
  geom_smooth(method = 'lm')

plot(slm7)

feather_mlm_df_delay15$predicted <- predict(slm7) 
feather_mlm_df_delay15$residuals <- residuals(slm7)

ggplot(feather_mlm_df_delay15, aes(x = arr_delay, y = dep_delay)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = arr_delay, yend = predicted), alpha = .2) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  geom_point(aes(x = mean(arr_delay), y = mean(dep_delay)), color="red")

# SOME EDA

pairs(feather_mlm_df_delay15)
corrplot(cor(feather_mlm_df_delay15), method = "number")
corrplot(cor(feather_mlm_df_delay15), method = "ellipse")


