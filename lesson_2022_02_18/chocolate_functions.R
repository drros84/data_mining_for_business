
#' Clean ingredients
#' 
#' A function for one-hot-encoding of ingredients
#' 
#' @param chocolate_clean the chocolate dataframe with some preprocessing
#' 
#' @return chocolate_clean, the original dataframe with the new cleaned columns for ingredients
clean_ingredients <- function(chocolate_clean) {
  
  # Get unique list of ingredients
  ingredients_list <- chocolate_clean %>% 
    pull(ingredients) %>% 
    str_remove("^[0-9]-") %>% 
    str_trim() %>% 
    str_split(",") %>% 
    unlist() %>% 
    unique() 
  
  # Create dataframe of dummy variables
  ingredients_frame <- data.frame(ingredients = ingredients_list[!is.na(ingredients_list)],
                                  values = NA) %>% 
    pivot_wider(names_from = ingredients, values_from = values) %>% 
    bind_cols(chocolate_clean %>% 
                select(obs_n, ingredients)) %>% 
    pivot_longer(names_to = "individual_ingredients", values_to = "values", -c(obs_n, ingredients)) %>% 
    mutate(values = case_when(str_detect(ingredients, individual_ingredients) ~ 1,
                              TRUE ~ 0)) %>% 
    select(-ingredients) %>% 
    mutate(individual_ingredients = paste0("ing_", individual_ingredients)) %>% 
    pivot_wider(names_from = individual_ingredients, values_from = values) 
  
  # Bind back into the chocolate dataframe
  chocolate_clean <- chocolate_clean %>% 
    select(-ingredients) %>% 
    left_join(ingredients_frame, by = "obs_n")
  
  return(chocolate_clean)
  
}


#' Clean characteristics
#' 
#' A function for one-hot-encoding the characteristics
#' 
#' @param chocolate_clean the chocolate dataframe after cleaning ingredients
#' 
#' @return chocolate_clean a dataframe with dummy variables for characteristics
clean_characteristics <- function(chocolate_clean) {
  
  # Get unique list of characteristics
  unique_characteristics <- chocolate_clean %>% 
    pull(most_memorable_characteristics) %>% 
    str_split(",") %>% 
    unlist() %>% 
    str_trim() %>% 
    unique() %>% 
    str_replace_all(" ", "_")
  
  # Remove an empty value
  unique_characteristics <- unique_characteristics[unique_characteristics != ""]
  
  # Create dataframe of dummy variables
  characteristics_frame <- data.frame(characteristics = unique_characteristics[!is.na(unique_characteristics)],
                                      values = NA) %>% 
    pivot_wider(names_from = characteristics, values_from = values) %>% 
    bind_cols(chocolate_clean %>% 
                select(obs_n, most_memorable_characteristics)) %>% 
    pivot_longer(names_to = "characteristics", values_to = "values", -c(obs_n, most_memorable_characteristics)) %>% 
    
    # This section is necessary because of overlaps in characteristics, eg "cocoa" and "rich cocoa",
    # so this ensures they don't get mixed up
    mutate(most_memorable_characteristics = str_replace_all(most_memorable_characteristics, ", ", ",")) %>% 
    mutate(most_memorable_characteristics = str_replace_all(most_memorable_characteristics, " ", "_")) %>% 
    mutate(most_memorable_characteristics = paste0(",", most_memorable_characteristics),
           characteristics = paste0(",", characteristics)) %>% 
    mutate(values = case_when(str_detect(most_memorable_characteristics, characteristics) ~ 1,
                              TRUE ~ 0)) %>% 
    mutate(characteristics = str_remove(characteristics, "^,")) %>% 
    select(-most_memorable_characteristics) %>% 
    mutate(characteristics = paste0("char_", characteristics)) %>% 
    pivot_wider(names_from = characteristics, values_from = values) 
  
  chocolate_clean <- chocolate_clean %>% 
    select(-most_memorable_characteristics) %>% 
    left_join(characteristics_frame, by = "obs_n")
  
  return(chocolate_clean)
  
}


#' Clean companies
#' 
#' A function for one-hot-encoding of company manufacturers
#' 
#' @param chocolate_clean a cleaned version of the chocolate dataframe
#' 
#' @return chocolate_clean the dataframe with dummy varirables for company names
clean_companies <- function(chocolate_clean) {
  
  # Get unique company names
  unique_companies <- chocolate_clean %>% 
    pull(company_manufacturer) %>% 
    unique()
  
  # Create dataframe with dummy variables for company names
  companies_frame <- data.frame(companies = unique_companies[!is.na(unique_companies)],
                                values = NA) %>% 
    pivot_wider(names_from = companies, values_from = values) %>% 
    bind_cols(chocolate_clean %>% 
                select(obs_n, company_manufacturer)) %>% 
    pivot_longer(names_to = "companies", values_to = "values", -c(obs_n, company_manufacturer)) %>% 
    mutate(values = case_when(str_detect(company_manufacturer, companies) ~ 1,
                              TRUE ~ 0)) %>% 
    select(-company_manufacturer) %>% 
    mutate(companies = paste0("manuf_", companies)) %>% 
    pivot_wider(names_from = companies, values_from = values)
  
  # Bind back to main dataframe
  chocolate_clean <- chocolate_clean %>% 
    select(-company_manufacturer) %>% 
    left_join(companies_frame, by = "obs_n")
  
  return(chocolate_clean)
  
}


#' Clean company locations
#' 
#' A function for one-hot-encoding of company locations
#' 
#' @param chocolat_clean the chocolate data with some cleaning done
#' 
#' @return chocolate_clean the dataframe with one-hot encoding of locations
clean_company_locations <- function(chocolate_clean) {
  
  # Get unique company locations
  unique_locations <- chocolate_clean %>% 
    pull(company_location) %>% 
    unique()
  
  # Create dataframe with dummy variables for company names
  locations_frame <- data.frame(location = unique_locations[!is.na(unique_locations)],
                                values = NA) %>% 
    pivot_wider(names_from = location, values_from = values) %>% 
    bind_cols(chocolate_clean %>% 
                select(obs_n, company_location)) %>% 
    pivot_longer(names_to = "location", values_to = "values", -c(obs_n, company_location)) %>% 
    mutate(values = case_when(str_detect(company_location, location) ~ 1,
                              TRUE ~ 0)) %>% 
    select(-company_location) %>% 
    mutate(location = paste0("company_", location)) %>% 
    pivot_wider(names_from = location, values_from = values)
  
  # Bind back to main dataframe
  chocolate_clean <- chocolate_clean %>% 
    select(-company_location) %>% 
    left_join(locations_frame, by = "obs_n")
  
  return(chocolate_clean)
  
}

#' Clean bean origins
#' 
#' A function for one-hot-encoding of bean origin
#' 
#' @param chocolat_clean the chocolate data with some cleaning done
#' 
#' @return chocolate_clean the dataframe with one-hot encoding of locations
clean_bean_origins <- function(chocolate_clean) {
  
  # Get unique company locations
  unique_origins <- chocolate_clean %>% 
    pull(country_of_bean_origin) %>% 
    unique()
  
  # Create dataframe with dummy variables for company names
  origins_frame <- data.frame(origin = unique_origins[!is.na(unique_origins)],
                                values = NA) %>% 
    pivot_wider(names_from = origin, values_from = values) %>% 
    bind_cols(chocolate_clean %>% 
                select(obs_n, country_of_bean_origin)) %>% 
    pivot_longer(names_to = "origin", values_to = "values", -c(obs_n, country_of_bean_origin)) %>% 
    mutate(values = case_when(str_detect(country_of_bean_origin, origin) ~ 1,
                              TRUE ~ 0)) %>% 
    select(-country_of_bean_origin) %>% 
    mutate(origin = paste0("bean_", origin)) %>% 
    pivot_wider(names_from = origin, values_from = values)
  
  # Bind back to main dataframe
  chocolate_clean <- chocolate_clean %>% 
    select(-country_of_bean_origin) %>% 
    left_join(origins_frame, by = "obs_n")
  
  return(chocolate_clean)
  
}

#' Clean review date
#' 
#' A function for one-hot-encoding of review dates
#' 
#' @param chocolat_clean the chocolate data with some cleaning done
#' 
#' @return chocolate_clean the dataframe with one-hot encoding of review dates
clean_review_dates <- function(chocolate_clean) {
  
  # Get unique company locations
  unique_dates <- chocolate_clean %>% 
    pull(review_date) %>% 
    unique()
  
  # Create dataframe with dummy variables for company names
  dates_frame <- data.frame(date = unique_dates[!is.na(unique_dates)],
                              values = NA) %>% 
    pivot_wider(names_from = date, values_from = values) %>% 
    bind_cols(chocolate_clean %>% 
                select(obs_n, review_date)) %>% 
    pivot_longer(names_to = "date", values_to = "values", -c(obs_n, review_date)) %>% 
    mutate(values = case_when(str_detect(review_date, date) ~ 1,
                              TRUE ~ 0)) %>% 
    select(-review_date) %>% 
    mutate(date = paste0("date_", date)) %>% 
    pivot_wider(names_from = date, values_from = values)
  
  # Bind back to main dataframe
  chocolate_clean <- chocolate_clean %>% 
    select(-review_date) %>% 
    left_join(dates_frame, by = "obs_n")
  
  return(chocolate_clean)
  
}


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


