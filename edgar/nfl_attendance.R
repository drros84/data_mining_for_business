
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

attendance_clean <- attendance_joined
 # na.omit()

attendance_joined%>%
  na.omit() %>%
  select(year,week, margin_of_victory, strength_of_schedule, tech_factor1, tech_factor2) %>%
  cor()%>%
  corrplot()
  
attendance_split <- initial_split(attendance_clean,
                                prop = 0.75,
                                strata = "weekly_attendance")

train <- training(attendance_split)
test <- testing(attendance_split)

attendance_recipe <- recipe(weekly_attendance ~ ., data = attendance_clean) %>%
  step_impute_median(strength_of_schedule) %>%
  step_corr(all_numeric(), threshold = 0.9)

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(attendance_recipe)

rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 10)
attendance_folds <- vfold_cv(train, v = 3,
                           strata = "weekly_attendance")

rf_cv_results <- rf_wf %>% 
  tune_grid(resamples = attendance_folds,
            grid = rf_grid,
            metrics = metrics_list)

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = attendance_split)

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

rf_final_perf <- metrics_list(rf_predictions,
                              truth = weekly_attendance,
                              estimate = .pred_class,
                              .pred_high)
base_perf 

corr_perf

rf_final_fit %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")
rf_final_fit
