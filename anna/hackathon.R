
library(tidyverse)
library(tidymodels)
library(ranger)
library(glmnet)
library(corrplot)
library(vip)

# Set seed and import data

set.seed(52)

source("anna/plot_tuning_metrics.R")


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
           other_colors = as.factor(other_colors)
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

# Trying to draw a correlation plot between the selected variables

ikea %>% 
  select(depth, height, width, height_width, double_height, triple_height) %>%
  cor() %>% 
  corrplot()

## Here for our model I need to split the data between a training and test set using the ``initial_split()`` function with 80% data in training set.

ikea_split <- initial_split(ikea_clean,
                                  prop = 0.8,
                                  strata = price)


train <- training(ikea_split)
test <- testing(ikea_split)

# Examining the dimensions of my test and train portions.
dim(train)
dim(test)


# Here we tell R to use "recipe" to predict the high price using all the other columns ikea_clean dataset.

ikea_recipe <- recipe(price ~ ., data = ikea_clean) %>% 
  step_impute_median(depth) %>% 
  step_corr(all_numeric(), threshold = 0.9)

# Examining the model

ikea_recipe

# I want to define what metrics to use to assess the model performance. Here are chosen.

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)


price_folds <- vfold_cv(train, v = 5,
                             strata = price)

# Creating model with random_forest

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")

# Here I combine the model and recipe.

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(ikea_recipe)

# I am creating a grid of values to run the model with different randomly chosen combinations of randomly chosen parameters. Here I gave to choose 15 parameters.


rf_grid <- grid_random(parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), size = 15)

head(rf_grid)

# Tuning the model

rf_cv_results <- rf_wf %>%
  tune_grid(resamples = price_folds,
            grid = rf_grid,
            metrics = metrics_list)

rf_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "min_n", multiple = TRUE) 

# Choosing the best model by AUC

dt_cv_results %>% 
  show_best(metric = "roc_auc")

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")


# Applying the best model to see its performance on the whole dataset.

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = ikea_split)

# Collecting the predictions from rf_final_fit 

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

rf_final_perf <- metrics_list(rf_predictions,
                              truth = price,
                              estimate = .pred_class,
                              .pred_high)

rf_predictions %>% 
  head()

# Creating Confusion matrix

conf_mat(rf_predictions,
         truth = price,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

# Drawing Roc Curve

rf_predictions %>% 
  roc_curve(truth = price, .pred_high) %>% 
  autoplot()


rf_final_perf <- metrics_list(rf_predictions,
                              truth = price,
                              estimate = .pred_class,
                              .pred_high)

rf_final_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point")
