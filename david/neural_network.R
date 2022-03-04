
#########################################################
# Ignore this section                                   #
#########################################################

library(tidyverse)
library(tidymodels)
library(tabnet)
library(torch)

set.seed(1234)

source("david/plot_tuning_metrics.R")

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

# First, split the data between a training and test set:
turbines_split <- initial_split(turbines_recoded,
                                prop = 0.75,
                                strata = turbine_capacity)

train <- training(turbines_split)
test <- testing(turbines_split)

# Create the 'recipe' by using ``turbine_capacity`` as the predictor and all other variables as the features:

turbines_recipe <- recipe(turbine_capacity ~ ., data = turbines_recoded) 


# Create an object to store performance metrics. Use ``roc_auc`` and anything else you think would be useful:

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)


# Tell R to create a neural network model:
#   
# * Call the model ``nn_model``;
# * Use the ``tabnet()`` function to tell R this is a neural network model;
# * Tune the following hyperparameter: `learn_rate``;
# * Use the ``torch`` package;

nn_model <- tabnet(learn_rate = tune(), num_steps = tune()) %>% 
  set_engine("torch") %>% 
  set_mode("classification")


# Create a workflow:

nn_wf <- workflow() %>% 
  add_model(nn_model) %>% 
  add_recipe(turbines_recipe)


# Create your cross-validation folds. Use 3-fold cross-validation.

turbines_folds <- vfold_cv(train, v = 3,
                           strata = "turbine_capacity")


# Create a regular grid of with 5 levels to tune your hyperparameters:

nn_grid <- grid_random(parameters(learn_rate(), num_steps() %>% range_set(c(2, 6))), size = 5)


# Tune your hyperparameters:

nn_cv_results <- nn_wf %>% 
  tune_grid(resamples = turbines_folds,
            grid = nn_grid,
            metrics = metrics_list)


# Plot the performance for different values of ``learn_rate`` using the ``plot_tuning_metrics()`` function:

nn_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "learn_rate", multiple = TRUE)  +
  scale_x_log10()

nn_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "num_steps", multiple = TRUE)


# Show the best models using ``show_best()``, using AUC as the performance metric:

nn_cv_results %>% 
  show_best(metric = "roc_auc")

# Select the best model using ``select_best()``, using AUC as the performance metric:

best_nn_model <- nn_cv_results %>% 
  select_best(metric = "roc_auc")


# Apply your best model to the data:

nn_final_fit <- nn_wf %>% 
  finalize_workflow(best_nn_model) %>% 
  last_fit(split = turbines_split)

# Collect the predictions and save them as ``nn_predictions``:

nn_predictions <- nn_final_fit %>% 
  collect_predictions()

# Create a confusion matrix with a heatmap:

jpeg(file="david/nn_conf_matrix1.jpeg")

conf_mat(nn_predictions,
         truth = turbine_capacity,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")
  
dev.off()

# Draw a ROC curve:

nn_predictions %>% 
  roc_curve(truth = turbine_capacity, .pred_high) %>% 
  autoplot()

# Collect the list of metrics using ``metrics_list()`` and save it as ``dt_final_perf``:

nn_final_perf <- metrics_list(nn_predictions,
                              truth = turbine_capacity,
                              estimate = .pred_class,
                              .pred_high)

# Print your final performance metrics:

nn_final_perf

