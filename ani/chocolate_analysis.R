

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

###############################################################################
# Start analysis for your homework here.
# The task is to run several competing models on the chocolate data
# and report the results in a markdown document.
# Please write which model is the best and why.
###############################################################################

head(chocolate)
summary(chocolate)
chocolate_split <- initial_split(chocolate_clean,
                                 prop = 0.8,
                                 strata = high_rating)

train <- training(chocolate_split)
test <- testing(chocolate_split)

chocolate_recipe <- recipe(high_rating ~ ., data = chocolate_clean) 

chocolate_recipe
metrics_list <- metric_set(roc_auc)
lasso_model <- logistic_reg() %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

lasso_model <- logistic_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

lasso_wf <- workflow() %>% 
  add_model(lasso_model) %>% 
  add_recipe(chocolate_recipe)

chocolate_folds <- vfold_cv(train, v = 3,
                            strata = high_rating)
lasso_grid <- grid_random(parameters(penalty(), mixture()), size = 20)

lasso_cv_results <- lasso_wf %>% 
  tune_grid(resamples = chocolate_folds,
            grid = lasso_grid,
            metrics = metrics_list)



late_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

late_model <- decision_tree (tree_depth(), min_n() ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

late_wf <- workflow() %>% 
  add_model(late_model) %>% 
  add_recipe(chocolate_recipe)

chocolate_folds2 <- vfold_cv (train, v = 5,
                            strata = high_rating)
late_grid <- grid_random(parameters(tree_depth(), min_n() ), size = 20)

late_cv_results <- late_wf %>% 
  tune_grid(resamples = chocolate_folds2,
            grid = late_grid,
            metrics = metrics_list)
