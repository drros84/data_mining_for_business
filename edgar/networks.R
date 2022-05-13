

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
  mutate(id = row_number())%>%
  mutate(color = case_when(label == "France" ~ "red",
                           label == "German" ~ "yellow,
                           TRUE == "grey"))

links <- erasmus %>% 
  left_join(nodes, by = c("sending_country_code" = "label")) %>% 
  select(from = id, receiving_country_code, students) %>% 
  left_join(nodes, by = c("receiving_country_code" = "label")) %>% 
  select(from, to = id, students) %>%
  mutate(arrows = "to") %>%
  mutate(smooth = FALSE)%>%
  mutate(width = students/10)%>%
  mutate(color = "red")

erasmus_graph <- erasmus %>%
  graph_from_data_frame(directed = TRUE)

plot(erasmus_graph)

degree(erasmus_graph)

data.frame(degree = degree(erasmus_graph, mode = "in"))%>%
  rownames_to_column(var = "country") %>%
  ggplot(aes(x = reorder(country, degree), y = degree))+
  geom_col()+
  coord_flip()

shortest_paths(erasmus_graph, "Hungary", "Germany")

visNetwork(nodes, links)

