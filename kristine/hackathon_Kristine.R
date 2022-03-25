
library(tidyverse)
library(tidymodels)
library(ranger)
library(glmnet)
library(corrplot)
library(vip)

# Set seed and import data

set.seed(52)

source("kristine/plot_tuning_metrics.R")


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

View (ikea_clean)
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

summary(ikea_clean) 

ikea_clean %>%
  ggplot(aes(x = height))+
  geom_density()

ikea_clean %>% 
  ggplot(aes( x = width))+
  geom_density()

ikea_clean %>% 
  ggplot( aes(x=depth)) +
  geom_point(aes(y = height),col ="red") +
  labs(title = "Dependency of depth and height", x = "depth", y = "height")


ggplot(ikea_clean, aes(x=width)) + 
  geom_histogram(binwidth=1, color="blue")

ikea_clean %>% 
  ggplot( aes(x=width)) +
  geom_point(aes(y = height),col ="green") +
  labs(title = "Dependency of Rating and Income", x = "width", y = "height")

unique(ikea_clean$price)

ikea_clean %>% count(price)


num_var <- data.frame(ikea_clean$depth, ikea_clean$height, ikea_clean$width,ikea_clean$double_height, ikea_clean$triple_height, ikea_clean$height_width )


str(num_var)

num_var %>% 
  cor() %>% 
  corrplot()

ikea_more_clean <- ikea_clean %>% 
  select(-double_height, -triple_height)

str(ikea_more_clean)

ikea_split <- initial_split(ikea_more_clean,
                            prop = 0.75,
                            strata = "price")

train <- training(ikea_split)

test <- testing(ikea_split)


dim(ikea_more_clean)
dim(train)
dim(test)

price_recipe <- recipe(price ~ ., data = ikea_more_clean) 

rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(price_recipe)

rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 10)


price_folds <- vfold_cv(train, v = 3,
                             strata = "price")

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

metrics_list

rf_cv_results <- rf_wf %>% 
  tune_grid(resamples = price_folds,
            grid = rf_grid,
            metrics = metrics_list)

best_rf_model <- rf_cv_results %>% 
  select_best(metric = "roc_auc")

head(best_rf_model)

rf_final_fit <- rf_wf %>% 
  finalize_workflow(best_rf_model) %>% 
  last_fit(split = ikea_split)

rf_predictions <- rf_final_fit %>% 
  collect_predictions()

rf_final_perf <- metrics_list(rf_predictions,
                              truth =price ,
                              estimate = .pred_class,
                              .pred_high)

rf_predictions %>% 
  roc_curve(truth = price, .pred_high) %>% 
  autoplot()

conf_mat(rf_predictions,
         truth = price,
         estimate = .pred_class) %>% 
  autoplot(type = "heatmap")

base_perf 

### As I see from my model's results, my model detected 95.5% of the prices are correctly classified, 
#In other words 95.5%  prices instances over the total number of data instances were correctly predicted.

#From the precision score I can state that 47.7% of the prices were high price were correctly predicted.
#It means model doesn't have good  false positive rate.

#Recall score tells me that my model detected  only 18.4% of the prices which were high in reality. 
#Therefore only  18.4% of the True Positives were discovered, which is terrible result.


# 1 accuracy  binary         0.955
# 2 precision binary         0.477
# 3 recall    binary         0.184
# 4 roc_auc   binary         0.805


##########Decision tree model####

dt_model <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_model <- decision_tree(tree_depth = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

dt_wf <- workflow() %>% 
  add_model(dt_model) %>% 
  add_recipe(price_recipe)

price_folds_dt <- vfold_cv(train, v = 3,
                           strata = "price")

price_folds_dt

dt_grid <- grid_random(parameters(tree_depth(), min_n()), size = 10)
head(dt_grid)

dt_cv_results <- dt_wf %>% 
  tune_grid(resamples = price_folds_dt,
            grid = dt_grid,
            metrics = metrics_list)

dt_cv_results %>% 
  plot_tuning_metrics(hyperparameter = "tree_depth", multiple = TRUE) 

dt_cv_results %>% 
  show_best(metric = "roc_auc")

best_dt_model <- dt_cv_results %>% 
  select_best(metric = "roc_auc")

dt_final_fit <- dt_wf %>% 
  finalize_workflow(best_dt_model) %>% 
  last_fit(split =ikea_split)

dt_predictions <- dt_final_fit %>% 
  collect_predictions()

dt_predictions %>% 
  head()

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


 # 1 accuracy  binary         0.888
#2 precision binary         0.84 
#3 recall    binary         0.743
#4 roc_auc   binary         0.899

#After running decision tree model, we can notice that some variables are essential has changed.

## Accuracy result get smaller, which mean that in this case my model detected 88.8% of the prices are correctly classified, 
# And  88.8%  prices instances over the total number of data instances were correctly predicted.

#Precision score has improved significantly, as for now  84% of the prices were high price were correctly predicted. 
#And the model has quiet good enough false positive rate.

#Similar to precision the recall score also got improved. Now we can see from the model that detected 74.3% of the prices  were high in reality. 
#Therefore only 74.3% of the True Positives were discovered, which is more better result than previous model.

# From the scores I will prefer to use Decision Tree model as it's results  better in 3 scores (precision, recall, ROC_AUC).
