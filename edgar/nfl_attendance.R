
library(tidyverse)
library(tidymodels)
library(ranger)
library(corrplot)
library(vip)

# Set seed and import data

set.seed(52)

source("edgar/plot_tuning_metrics.R")


attendance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv")
standings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv")

attendance_joined <- attendance %>%
  left_join(standings,
            by = c("year", "team_name", "team")
  ) %>%
  filter(!is.na(weekly_attendance)) %>%
  select(
    weekly_attendance, team_name, year, week,
    margin_of_victory, strength_of_schedule, playoffs
  ) %>% 
  mutate(weekly_attendance = ifelse(weekly_attendance > 80000, "high", "other")) %>% 
  mutate(weekly_attendance = as.factor(weekly_attendance),
         team_name = as.factor(team_name),
         playoffs = as.factor(playoffs)) %>% 
  mutate(tech_factor1 = margin_of_victory * 0.5 + rnorm(1, 5),
         tech_factor2 = margin_of_victory * 0.5 + rnorm(1, 10)) 

na_sample <- sample(c(1:nrow(attendance_joined)), 
                    0.1 * nrow(attendance_joined), replace = FALSE)

attendance_joined[na_sample, "strength_of_schedule"] <- NA


## Start here: build a random forest model to predict whether a game will have high attendance or not. 
# Note: there are missing values in the strenght_of_schedule variable. Initially, remove them using the code below

attendance_clean <- attendance_joined %>% 
  na.omit()

