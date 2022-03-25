
library(tidyverse)
library(tidymodels)
library(ranger)
library(glmnet)
library(corrplot)
library(vip)

# Set seed and import data

set.seed(52)

source("maria/plot_tuning_metrics.R")


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
ikea_clean <- ikea %>%   
  na.omit()
View(ikea_clean)

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

# Decision tree Model

# to get the look and have overall idea i used head and summary functions and I also used dim function to look at the number of observations

head(ikea_clean)
summary(ikea_clean)
dim(ikea_clean)

# by using initial_split function I split the data between train and test sets, I allocated 75% percent to training set
# and again I used dim functions to look at the number of observations in each of the sets

ikea_split<- initial_split(ikea_clean,
                                prop = 0.75,
                                strata = price)

train <- training(ikea_split)
test <- testing(ikea_split)
dim(train)
dim(test)

# Recipe function used below tells are to predict "price" using all other columns of the dataframe

ikea_recipe <- recipe(price ~ ., data = ikea_clean) 

ikea_recipe

# by using the metric_set function I defined the what metrics i need to be used to understand the performance of my model

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

# below function helped to actually build the model / I chose Decision tree model

dt_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# I tuned some hyperparameters to get better results

dt_model <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# then I created the workflow

dt_wf <- workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(ikea_recipe)

# next step is to create cross-validation sets and tune the hyperparameters

ikea_folds <- vfold_cv(train, v = 3,
                            strata = price)

ikea_folds

# by below code I created random grid 

dt_grid <- grid_random(parameters(tree_depth(), min_n()), size = 25)

head(dt_grid)

dt_cv_results <- dt_wf %>% 
  tune_grid(resamples = ikea_folds,
            grid = dt_grid,
            metrics = metrics_list)

#To plot the performance I used plot_tuning_metrics function

dt_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

#next I selected best model to by adding ROC_AUC as an additional argument with select_best function

best_dt_model <- dt_cv_results %>% 
  select_best(metric = "roc_auc")

best_dt_model

# Now I can test my model on the training set 

dt_final_fit <- dt_wf %>% 
  finalize_workflow(best_dt_model) %>% 
  last_fit(split = ikea_split)

# to collect my predictions I used collect_predictions function

dt_predictions <- dt_final_fit %>% 
  collect_predictions()

dt_predictions %>% 
  head()

# to create a confusion matrix for my model I applied conf_mat function 
#and to present the confusion matrix in a heatmap I added autoplot function

conf_mat(dt_predictions,
         truth = price,
         estimate = .pred_class)%>%
  autoplot(type = "heatmap")

# by below alteration instead of heatmap i got mosaic plot

conf_mat(dt_predictions,
         truth = price,
         estimate = .pred_class) %>% 
  autoplot(type = "mosaic")

# and to draw the ROC line i used below code

dt_predictions %>% 
  roc_curve(truth = price, .pred_high) %>% 
  autoplot()

# by below code I calculated metrics of interests

dt_final_perf <- metrics_list(dt_predictions,
                              truth = price,
                              estimate = .pred_class,
                              .pred_high)

dt_final_perf

# Final Results
# for the Accuracy I got 0.905 which means that 90.5% of my predictions were correct 
# for the precision I got 0.903 which means 90.3% of the prices we predicted were high were correctly predicted 
# Recall result was 0.743 from which we can conclude that my model detected correctly 74.3% of the prices that were actually high in reality


# Random Forest Model


head(ikea_clean)
summary(ikea_clean)


# by using initial_split function I split the data between train and test sets, I allocated 75% percent to training set
# and again I used dim functions to look at the number of observations in each of the sets

ikea_split<- initial_split(ikea_clean,
                                prop = 0.75,
                                strata = price)

train <- training(ikea_split)
test <- testing(ikea_split)

# Recipe function used below tells are to predict "price" using all other columns of the dataframe

ikea_recipe <- recipe(high_rating ~ ., data = chocolate_clean) 

ikea_recipe

# by using the metric_set function I defined the what metrics i need to be used to understand the performance of my model


metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

# below function helped to actually build the model 

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# # then I created the workflow

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(ikea_recipe)

# next step is to create cross-validation sets and tune the hyperparameters

ikea_folds <- vfold_cv(train, v = 3,
                            strata = "price")

# by below code I created random grid 
rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 20)

# by below I tuned my  hyperparameters

rf_cv_results <- rf_wf %>% 
  tune_grid(resamples = ikea_folds,
            grid = rf_grid,
            metrics = metrics_list)

# #To plot the performance I used plot_tuning_metrics function

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "mtry", multiple = TRUE) 

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "trees", multiple = TRUE) 

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

#next I selected best model to by adding ROC_AUC as an additional argument with select_best function

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")

best_rf_model

# Now I can test my model on the training set 

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = ikea_split)

# to collect my predictions I used collect_predictions function

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

# to create a confusion matrix for my model I applied conf_mat function 
#and to present the confusion matrix in a heatmap I added autoplot function

conf_mat(rf_predictions,
         truth = price,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

# and to draw the ROC line i used below code

rf_predictions %>% 
  roc_curve(truth = price, .pred_high) %>% 
  autoplot()

# by below code I calculated metrics of interests

rf_final_perf <- metrics_list(rf_predictions,
                              truth = price,
                              estimate = .pred_class,
                              .pred_high)

rf_final_perf

# Final Results
#By Running Random Forest tree I generated below results
# for the Accuracy I got 0.925 which means that 92.5% of my predictions were correct 
# for the precision I got 0.895 which means that 89.5% of the prices we predicted were high were correctly predicted 
# Recall result was 0.832 from which we can conclude that my model detected correctly 83.2% of the prices that were actually high in reality
