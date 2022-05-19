
library(tidyverse)
library(tidymodels)
library(ranger)
library(glmnet)
library(corrplot)
library(vip)

# Set seed and import data

set.seed(52)




alco <- read.csv("alcohol-consumption-vs-gdp-per-capita.csv")

# Clean the data

alco <- alco %>% 
  clean_names() %>% # clean up column names
  mutate(alcohol_consumption = total_alcohol_consumption_per_capita_liters_of_pure_alcohol_projected_estimates_15_years_of_age) %>% # shortening column name
  mutate(gdp_per_cap = gdp_per_capita_ppp_constant_2017_international) %>% # shortening column name
  mutate(population_est = population_historical_estimates) %>% # shortening column name
  select(entity, year, alcohol_consumption, gdp_per_cap, population_est, continent) %>% # selecting wanted columns
  drop_na()

################################################################################
# Start here
################################################################################

# Examine the data using View()
View(alco)

# From the code above, you can see that there is some missing data in the 'depth' column.
# The code below removes it. This is something you are allowed to change otherwise
alco_clean <- alco%>%   
na.omit()

# The dataframe shows data from IKEA, the furniture company. Some items have a high price ('high'), some
# have a normal price ('other'). Your goal is to predict which furniture has a high price.

# You have access to the following variables to help you predict:
# - sellable_online: whether the item can be sold online or not (True / False)
# - other_colors: whether several colors are available for this item (Yes / No)
# - some numeric data relating to the size of the item:
#   - depth
#   - height
#   - height_width
#   - double_height
#   - triple_height
# - some dummy variables that tell you the type of item sold (eg bed, chairs, bookcases & shelving units, etc)

# Good luck!


alco%>%
  na.omit() %>%
  select(gdp_per_cap, alcohol_consumption) %>%
  cor()%>%
  corrplot()

alco_split <- initial_split(alco_clean,
                            prop = 0.75,
                            strata = "alcohol_consumption")

train <- training(alco_split)
test <- testing(alco_split)

#We told R what variable we were predicting and what features to use with the ``recipe()`` function. The code below tells R we want to predict ``price`` using all other columns of the ``ikea_clean`` dataframe as features


alco_recipe <- recipe(alcohol_consumption ~ ., data = alco_clean) %>%
  step_impute_median(depth) 

# I create an object to store performance metrics. Use ``accuracy``, ``precision``, ``recall``, ``roc_auc``

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(alco_recipe)

rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 10)
alco_folds <- vfold_cv(train, v = 3,
                       strata = "alcohol_consumption")

rf_cv_results <- rf_wf %>% 
  tune_grid(resamples = alco_folds,
            grid = rf_grid,
            metrics = metrics_list)

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = alco_split)

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

rf_final_perf <- metrics_list(rf_predictions,
                              truth = alcohol_consumption,
                              estimate = .pred_class,
                              .pred_high)



rf_final_perf 



#decision tree model



#The first step for our modelling is to split the data between a training and test set using the
alco_split <- initial_split(alco_clean,
                            prop = 0.75,
                            strata = "alcohol_consumption")

train <- training(alco_split)
test <- testing(alco_split)

alco_recipe <- recipe(alcohol_consumption ~ ., data = alco_clean)

alco_recipe

# I create an object to store performance metrics. Use ``accuracy``, ``precision``, ``recall``, ``roc_auc``

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

dt_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_model <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_wf <- workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(alco_recipe)

alco_folds <- vfold_cv(train, v = 3,
                       strata = "alcohol_consumption")

alco_folds

dt_grid <- grid_random(parameters(tree_depth(), min_n()), size = 20)

head(dt_grid)

dt_cv_results <- dt_wf %>% 
  tune_grid(resamples = alco_folds,
            grid = dt_grid,
            metrics = metrics_list)

#Plot the performance for different values of ``min_n()`` using the ``plot_tuning_metrics()`` function

dt_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

best_dt_model <- dt_cv_results %>% 
  select_best(metric = "roc_auc")

best_dt_model

dt_final_fit <- dt_wf %>% 
  finalize_workflow(best_dt_model) %>% 
  last_fit(split = alco_split)

dt_predictions <- dt_final_fit %>% 
  collect_predictions()

dt_predictions %>% 
  head()

conf_mat(dt_predictions,
         truth = price,
         estimate = .pred_class)

conf_mat(dt_predictions,
         truth = price,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

conf_mat(dt_predictions,
         truth = price,
         estimate = .pred_class) %>% 
  autoplot(type = "mosaic")

dt_predictions %>% 
  roc_curve(truth = alcohol_consumption, .pred_high) %>% 
  autoplot()

dt_final_perf <- metrics_list(dt_predictions,
                              truth = alcohol_consumption,
                              estimate = .pred_class,
                              .pred_high)

dt_final_perf
