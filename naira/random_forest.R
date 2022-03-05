
#########################################################
# Ignore this section                                   #
#########################################################

library(tidyverse)
library(tidymodels)
library(rpart)
library(ranger)

set.seed(1234)

source("plot_tuning_metrics.R")

turbines <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv")

turbines_df <- turbines %>%
  transmute(
    turbine_capacity = turbine_rated_capacity_k_w,
    rotor_diameter_m,
    hub_height_m,
    commissioning_date = parse_number(commissioning_date),
    province_territory = fct_lump_n(province_territory, 10),
    model = fct_lump_n(model, 10)
  ) %>%
  filter(!is.na(turbine_capacity)) %>%
  mutate_if(is.character, factor)

turbines_recoded <- turbines_df %>% 
  mutate(turbine_capacity = ifelse(turbine_capacity > 2300, "high", "other")) %>% 
  mutate(turbine_capacity = as.factor(turbine_capacity)) 

#########################################################
# Start here                                            #
#########################################################

# First, split the data between a training and test set, 
# allocating 75% percent of the data to training, and stratifying by ``turbine_capacity``:




# Create the 'recipe' by using ``turbine_capacity`` as the predictor and all other variables as the features:




# Create an object to store performance metrics. Use ``accuracy``, ``precision``, ``recall``, ``roc_auc``:




# Tell R to create a model. Use the code you wrote to create ``dt_model``, but with the following changes:
#   
# * Call the model ``rf_model`` instead of ``dt_model``;
# * Use the ``rand_forest()`` function instead of ``logistic_reg()`` to tell R this is a random forest model;
# * Tune the following hyperparameters: ``mtry()``, ``trees()``, ``min_n()``
# * Use the ``"ranger"`` package instead of the ``"rpart"`` package;




# Create a workflow. Call it ``rf_wf`` instead of ``dt_wf``:




# Create your cross-validation folds. Use 3-fold cross-validation.




# Create a random grid of size 20 to tune your hyperparameters. Call your new object rf_grid. Also, instead of just writing ``mtry()``, 
# use ``mtry() %>% range_set(c(4, 12))`` (because for some reason this needs to be specified or else it will not work):




# Tune your hyperparameters, saving your results as ``rf_cv_results``:




# Plot the performance for different values of ``mtry()`` using the ``plot_tuning_metrics()`` function:




# Plot the performance for different values of ``trees()`` using the ``plot_tuning_metrics()`` function:




# Plot the performance for different values of ``min_n()`` using the ``plot_tuning_metrics()`` function:




# Select the best model using ``select_best()``, using AUC as the performance metric:




# Apply your best model to the data. Save the final fit as ``rf_final_fit`:




# Collect the predictions and save them as ``rf_predictions``:



# Create a confusion matrix with a heatmap:




# Draw a ROC curve:



# Collect the list of metrics using ``metrics_list()`` and save it as ``dt_final_perf``:



# Print your final performance metrics:


