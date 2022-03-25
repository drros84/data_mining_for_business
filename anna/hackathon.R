
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








