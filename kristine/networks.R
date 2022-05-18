

library(igraph)
library(tidyverse)
library(countrycode)
library(visNetwork)

set.seed(1234)

# Import data
erasmus <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv") %>% 
  filter(mobility_duration >= 7)  %>%
  count(sending_country_code, receiving_country_code, wt = participants, name = "students") %>%
  filter(sending_country_code != receiving_country_code) %>%
  mutate(across(contains("country_code"), countrycode::countrycode,
                origin = "eurostat", destination = "country.name")) %>%
  filter(students > 20)

# For data viz
nodes <- data.frame(label = unique(c(erasmus$sending_country_code,
                                     erasmus$receiving_country_code))) %>% 
  mutate(id = row_number())

links <- erasmus %>% 
  left_join(nodes, by = c("sending_country_code" = "label")) %>% 
  select(from = id, receiving_country_code, students) %>% 
  left_join(nodes, by = c("receiving_country_code" = "label")) %>% 
  select(from, to = id, students) 



