

library(tidyverse)
library(glmnet)
library(tidymodels)
library(roxygen2)
# library(finetune)

source("lesson_2022_02_18/chocolate_functions.R")

# Set seed
set.seed(1234)

# Import data
chocolate <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
chocolate
# Clean the data
chocolate_clean <- chocolate %>% 
  mutate(obs_n = row_number()) %>% 
  mutate(high_rating = ifelse(rating >= 3.5, "high", "other")) %>% 
  mutate(cocoa_percent = str_remove(cocoa_percent, "%") %>% as.numeric()) %>% 
  clean_ingredients() %>% 
  clean_characteristics() %>% 
  clean_companies() %>% 
  clean_company_locations() %>% 
  clean_bean_origins() %>% 
  clean_review_dates() %>% 
  select(-ref, -specific_bean_origin_or_bar_name, -cocoa_percent, -rating, -obs_n)

chocolate_clean

###############################################################################
# Start analysis for your homework here.
# The task is to run several competing models on the chocolate data
# and report the results in a markdown document.
# Please write which model is the best and why.
###############################################################################

#Random Forest Model

head(chocolate_clean)
summary(chocolate_clean)


chocolate_split<- initial_split(chocolate_clean,
                                 prop = 0.75,
                                 strata = high_rating)

train <- training(chocolate_split)
test <- testing(chocolate_split)

chocolate_recipe <- recipe(high_rating ~ ., data = chocolate_clean) 

chocolate_recipe

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(chocolate_recipe)

chocolate_folds <- vfold_cv(train, v = 3,
                             strata = "high_rating")

rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 20)

rf_cv_results <- rf_wf %>% 
  tune_grid(resamples = chocolate_folds,
            grid = rf_grid,
            metrics = metrics_list)

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "mtry", multiple = TRUE) 

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "trees", multiple = TRUE) 

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = chocolate_split)

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

conf_mat(rf_predictions,
         truth = high_rating,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

rf_predictions %>% 
  roc_curve(truth = high_rating, .pred_high) %>% 
  autoplot()

rf_final_perf <- metrics_list(rf_predictions,
                              truth = high_rating,
                              estimate = .pred_class,
                              .pred_high)

rf_final_perf

# Decision Tree Model

chocolate_split <- initial_split(chocolate_clean,
                                 prop = 0.75,
                                 strata = high_rating)

train <- training(chocolate_split)
test <- testing(chocolate_split)

chocolate_recipe <- recipe(high_rating ~ ., data = chocolate_clean)

chocolate_recipe

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

dt_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_model <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_wf <- workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(chocolate_recipe)

chocolate_folds <- vfold_cv(train, v = 3,
                            strata = high_rating)

chocolate_folds

dt_grid <- grid_random(parameters(tree_depth(), min_n()), size = 20)

head(dt_grid)

dt_cv_results <- dt_wf %>% 
  tune_grid(resamples = chocolate_folds,
            grid = dt_grid,
            metrics = metrics_list)

dt_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

best_dt_model <- dt_cv_results %>% 
  select_best(metric = "roc_auc")

best_dt_model

dt_final_fit <- dt_wf %>% 
  finalize_workflow(best_dt_model) %>% 
  last_fit(split = chocolate_split)

dt_predictions <- dt_final_fit %>% 
  collect_predictions()

dt_predictions %>% 
  head()

conf_mat(dt_predictions,
         truth = high_rating,
         estimate = .pred_class)

conf_mat(dt_predictions,
         truth = high_rating,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

conf_mat(dt_predictions,
         truth = high_rating,
         estimate = .pred_class) %>% 
  autoplot(type = "mosaic")

dt_predictions %>% 
  roc_curve(truth = high_rating, .pred_high) %>% 
  autoplot()

dt_final_perf <- metrics_list(dt_predictions,
                              truth = high_rating,
                              estimate = .pred_class,
                              .pred_high)

dt_final_perf
