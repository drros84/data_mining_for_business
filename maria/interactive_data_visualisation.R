

library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(plotly)
library(dygraphs)
library(xts)

economic_data <- read_csv("https://raw.githubusercontent.com/drros84/euro_cluster_app/master/macro_dataset.csv") %>% 
  select(country = region, year = Year, GDP, wages = average_annual_wage, inflation = cpi, 
         productivity, unemployment = unemployment_rate, research_investment = GERD, govt_debt) %>% 
  na.omit() 

economic_data %>% 
  filter(year == 2014) %>%
  arrange(desc(unemployment)) %>% 
  mutate(country = ifelse(row_number() <= 7, country, "Other")) %>% 
  ggplot(aes(x = unemployment, y = inflation, col = country)) +
  geom_point(size = 4) +
  theme_bw()




