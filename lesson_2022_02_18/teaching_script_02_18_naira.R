
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

# Exploratory data analysis
head(chocolate)

str(chocolate)

summary(chocolate)
chocolate %>% 
  count(country_of_bean_origin) %>% 
ggplot(aes(x = reorder (country_of_bean_origin, n), y = n))+
  geom_col()

chocolate %>% 
  count(country_of_bean_origin) %>%
  arrange(desc(n)) %>% 
  head(20) %>% 
  ggplot(aes(x=reorder(country_of_bean_origin, n), y=n))+
  geom_col(fill = "darkblue")+
coord_flip()+
  theme_bw()+
  xlab("country")+
  ylab("Number of Observations")
  


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


# Split the data between training and testing, assigning 80% to training and 20% to testing.
# Additionally, stratify by the predictor variable to ensure there are roughly similar shares
# in each group.
chocolate_split <- initial_split(chocolate_clean,
                                 prop = 0.8,
                                 strata = "high_rating")

train <- training(chocolate_split)
test <- testing(chocolate_split)

# Split the training data into 5 groups for 5-fold cross-validation
chocolate_folds <- vfold_cv(train, v = 5,
                            strata = high_rating)

# Create the "recipe", defining the predictor and the features
chocolate_recipe <- recipe(high_rating ~ ., data = chocolate_clean) 

# Create a list of performance metrics
metrics_list <- metric_set(accuracy, precision, recall, roc_auc)


####################
# Build a ridge model

# Create the model, defining the package ("engine") and the mode ("classification" or "regression")
ridge_model <- logistic_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

ridge_grid <- grid_regular(penalty(), levels = 50)

# Create the workflow, that brings together the model and the recipe
ridge_wf <- workflow() %>% 
  add_model(ridge_model) %>% 
  add_recipe(chocolate_recipe)

# Fit the workflow to the cross-validation groups
ridge_cv_results <- ridge_wf %>% 
  tune_grid(resamples = chocolate_folds,
            grid = ridge_grid,
            metrics = metrics_list)

# Plot the hyperparameter tuning
ridge_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "penalty", multiple = FALSE) +
  scale_x_log10()

# Show the top 5 best models by AUC
ridge_cv_results %>% 
  show_best(metric = "roc_auc")

# Select the model wit the best AUC
best_ridge_model <- ridge_cv_results %>% 
  select_best(metric = "roc_auc") 

# Fit the best model to the data
ridge_final_fit <- ridge_wf %>% 
  finalize_workflow(best_ridge_model) %>% 
  last_fit(split = chocolate_split)

# Collect the predictions
ridge_predictions <- ridge_final_fit %>% 
  collect_predictions()

# Draw a heatmap for the confusion matrix:
conf_mat(ridge_predictions,
         truth = high_rating,
         estimate = .pred_class) %>% 
  # Create a heat map
  autoplot(type = "heatmap")

# Draw a ROC curve
ridge_predictions %>% 
  roc_curve(truth = high_rating, .pred_high) %>% 
  autoplot()

# Calculate metrics
ridge_final_perf <- metrics_list(ridge_predictions,
                                 truth = high_rating,
                                 estimate = .pred_class,
                                 .pred_high)

ridge_final_perf

# Build a ridge regression model

# Create the model, defining the package ("engine") and the mode ("classification" or "regression")
ridge_model <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

ridge_grid <- grid_regular(penalty(), levels = 50)

# Create the workflow, that brings together the model and the recipe
ridge_wf <- workflow() %>% 
  add_model(ridge_model) %>% 
  add_recipe(chocolate_recipe)

# Fit the workflow to the cross-validation groups
ridge_cv_results <- ridge_wf %>% 
  tune_grid(resamples = chocolate_folds,
            grid = ridge_grid,
            metrics = metrics_list)

# Plot the hyperparameter tuning
ridge_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "penalty", multiple = FALSE) +
  scale_x_log10()

# Show the top 5 best models by AUC
ridge_cv_results %>% 
  show_best(metric = "roc_auc")

# Select the model wit the best AUC
best_ridge_model <- ridge_cv_results %>% 
  select_best(metric = "roc_auc") 

# Fit the best model to the data
ridge_final_fit <-ridge_wf %>% 
  finalize_workflow(best_ridge_model) %>% 
  last_fit(split = chocolate_split)

# Collect the predictions
ridge_predictions <- ridge_final_fit %>% 
  collect_predictions()

# Draw a heatmap for the confusion matrix:
conf_mat(ridge_predictions,
         truth = high_rating,
         estimate = .pred_class) %>% 
  # Create a heat map
  autoplot(type = "heatmap")

# Draw a ROC curve
ridge_predictions %>% 
  roc_curve(truth = high_rating, .pred_high) %>% 
  autoplot()

# Calculate metrics
ridge_final_perf <- metrics_list(ridge_predictions,
                                 truth = high_rating,
                                 estimate = .pred_class,
                                 .pred_high)

