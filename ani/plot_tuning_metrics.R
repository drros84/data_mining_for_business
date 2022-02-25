

#' Plot tuning metrics
#' 
#' This functions plots performance indicators from tuning hyperparameters
#' 
#' @param tuning_results the resulting dataframe from applying tune_grid to the workflow
#' 
#' @return a ggplot showing performance indicators
plot_tuning_metrics <- function(tuning_results, hyperparameter, multiple = FALSE){
  
  if(multiple == FALSE) {
    
    tuning_results %>%
      collect_metrics() %>%
      ggplot(aes(x = eval(parse(text = hyperparameter)), y = mean, color = .metric)) +
      geom_errorbar(aes(
        ymin = mean - std_err,
        ymax = mean + std_err
      ),
      alpha = 0.5
      ) +
      geom_line(size = 1.5) +
      facet_wrap(~.metric, scales = "free", nrow = 2) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab(hyperparameter)
    
  } else {
    
    tuning_results %>% 
      collect_metrics() %>%
      ggplot(aes(x = eval(parse(text = hyperparameter)), y = mean)) +
      geom_point() +
      facet_wrap(~.metric, scales = "free", nrow = 2) +
      theme_bw()  +
      xlab(hyperparameter)
    
  }
  
}