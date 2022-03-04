
#########################################################
# Ignore this section                                   #
#########################################################

library(tidyverse)
library(tidymodels)
library(tabnet)
library(torch)

set.seed(1234)

source("edgar/plot_tuning_metrics.R")

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

# Save a density plot of rotor diameter
jpeg(file="edgar/rotor_diameter_density.jpeg")

turbines %>% 
  ggplot(aes(x = rotor_diameter_m)) +
  geom_density()

dev.off()

#########################################################
# Start here                                            #
#########################################################

# First, split the data between a training and test set:


# Create the 'recipe' by using ``turbine_capacity`` as the predictor and all other variables as the features:



# Create an object to store performance metrics. Use ``roc_auc`` and anything else you think would be useful:



# Tell R to create a neural network model:
#   
# * Call the model ``nn_model``;
# * Use the ``tabnet()`` function to tell R this is a neural network model;
# * Tune the following hyperparameter: `learn_rate``;
# * Use the ``torch`` package;



# Create a workflow:



# Create your cross-validation folds. Use 3-fold cross-validation.



# Create a regular grid of with 5 levels to tune your hyperparameters:



# Tune your hyperparameters:



# Plot the performance for different values of ``learn_rate`` using the ``plot_tuning_metrics()`` function:




# Show the best models using ``show_best()``, using AUC as the performance metric:



# Select the best model using ``select_best()``, using AUC as the performance metric:




# Apply your best model to the data:



# Collect the predictions and save them as ``nn_predictions``:



# Create a confusion matrix with a heatmap:


# Draw a ROC curve:



# Collect the list of metrics using ``metrics_list()`` and save it as ``dt_final_perf``:



# Print your final performance metrics:



