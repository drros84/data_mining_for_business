
library(tidyverse)
library(dendextend)

# Import data

economic_data <- read_csv("https://raw.githubusercontent.com/drros84/euro_cluster_app/master/macro_dataset.csv") %>% 
  filter(Year == 2014) %>% 
  select(country = region, GDP, wages = average_annual_wage, inflation = cpi, 
         productivity, unemployment = unemployment_rate, research_investment = GERD, govt_debt) %>% 
  na.omit() %>% 
  column_to_rownames("country")


# Plot cluster dendrogram for hierarchical clustering
economic_data %>% 
  scale() %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() %>%
  set("labels_col", k = 5) %>%
  set("branches_k_color", k = 5)%>%
  as.ggdend() %>%
  ggplot(horiz = TRUE) 
  #coord_polar(theta = "x")







