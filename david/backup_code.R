

ikea_split <- initial_split(ikea_clean,
                            prop = 0.75,
                            strata = price)

train <- training(ikea_split)
test <- testing(ikea_split)

ikea_recipe <- recipe(price ~ ., data = ikea_clean) %>% 
  step_impute_median(depth) %>% 
  step_corr(all_numeric(), threshold = 0.9)

metrics_list <- metric_set(accuracy, precision, recall, roc_auc)

ikea_folds <- vfold_cv(train, v = 3,
                       strata = price)


rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("classification")


rf_wf <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(ikea_recipe)

rf_grid <- grid_random(
  parameters(mtry() %>% range_set(c(4, 12)), trees(), min_n()), 
  size = 10)

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

base_perf
impute_perf 
corr_perf
five_fold_perf 
grid_ten

rf_final_fit %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point")