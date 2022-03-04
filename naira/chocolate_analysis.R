

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

