
library(tidyverse)
library(tidymodels)
library(ranger)
library(glmnet)
library(corrplot)
library(vip)

# Set seed and import data

set.seed(52)

source("edgar/plot_tuning_metrics.R")


ikea <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv", row.names = 'X')

# Clean the data

category_frame <- ikea %>% 
  filter(!is.na(width), !is.na(height)) %>% 
  select(item_id, category) %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = category, values_from = value) %>% 
  mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
  mutate_all(as.factor)

ikea <- ikea %>% 
  filter(!is.na(width), !is.na(height)) %>% 
  select(item_id, price, sellable_online, other_colors, 
         depth, height, width) %>% 
  unique() %>% 
  mutate(sellable_online = as.factor(sellable_online),
           other_colors = as.factor(other_colors),
         ) %>% 
  mutate(price = ifelse(price > 1545, "high", "other")) %>% 
  mutate(height_width = height * width,
         double_height = height * 2,
         triple_height = height * 3) %>% 
  mutate(item_id = as.character(item_id)) %>% 
  left_join(category_frame %>% mutate(item_id = as.character(item_id)), by = "item_id") %>% 
  select(-item_id)


################################################################################
# Start here
################################################################################

# Examine the data using View()
View(ikea)

# From the code above, you can see that there is some missing data in the 'depth' column.
# The code below removes it. This is something you are allowed to change otherwise
ikea_clean <- ikea   
  #na.omit()

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


ikea%>%
  na.omit() %>%
  select(depth,height, height_width, double_height, triple_height) %>%
  cor()%>%
  corrplot()

ikea_split <- initial_split(ikea_clean,
                                  prop = 0.75,
                                  strata = "price")

train <- training(ikea_split)
test <- testing(ikea_split)

ikea_recipe <- recipe(price ~ ., data = ikea_clean) %>%
  step_impute_median(depth) 
  

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(ikea_recipe)

rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 10)
ikea_folds <- vfold_cv(train, v = 3,
                             strata = "price")

rf_cv_results <- rf_wf %>% 
  tune_grid(resamples = ikea_folds,
            grid = rf_grid,
            metrics = metrics_list)

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = ikea_split)

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

rf_final_perf <- metrics_list(rf_predictions,
                              truth = price,
                              estimate = .pred_class,
                              .pred_high)



rf_final_perf 

# Final Results
# for the Accuracy I got 0.923 which means that 92.3% of my predictions were correct 
# for the precision I got 0.885 which means 88.5 of the prices we predicted were high were correctly predicted 
# Recall result was 0.817 from which we can conclude that my model detected correctly 81.7% of the prices that were actually high in reality

#decision tree model

ikea_split <- initial_split(ikea_clean,
                                 prop = 0.75,
                                 strata = "price")

train <- training(ikea_split)
test <- testing(ikea_split)

ikea_recipe <- recipe(price ~ ., data = ikea_clean)

ikea_recipe

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

dt_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_model <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_wf <- workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(ikea_recipe)

ikea_folds <- vfold_cv(train, v = 3,
                            strata = price)

ikea_folds

dt_grid <- grid_random(parameters(tree_depth(), min_n()), size = 20)

head(dt_grid)

dt_cv_results <- dt_wf %>% 
  tune_grid(resamples = ikea_folds,
            grid = dt_grid,
            metrics = metrics_list)

dt_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

best_dt_model <- dt_cv_results %>% 
  select_best(metric = "roc_auc")

best_dt_model

dt_final_fit <- dt_wf %>% 
  finalize_workflow(best_dt_model) %>% 
  last_fit(split = ikea_split)

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
  roc_curve(truth = price, .pred_high) %>% 
  autoplot()

dt_final_perf <- metrics_list(dt_predictions,
                              truth = price,
                              estimate = .pred_class,
                              .pred_high)

dt_final_perf

# Final Results
# for the Accuracy I got 0.910 which means that 91% of my predictions were correct 
# for the precision I got 0.885 which means 88.5 of the prices we predicted were high were correctly predicted 
# Recall result was 0.761 from which we can conclude that my model detected correctly 76.1% of the prices that were actually high in reality



# If we compare this two models we shall see that RF model is more preferable than DT model for our prediction
#because it`s accuracy, recall, roc_auc metrics are more correct when precision is the same.



