

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

xts_data <- economic_data %>% 
  filter(year >= 2000) %>% 
  # filter(country == "Greece") %>% 
  filter(country %in% c("Greece", "Italy")) %>%
  select(year, country, inflation) %>% 
  mutate(year = as.Date(paste0(year, "-01-01"))) %>% 
  pivot_wider(names_from = country, values_from = inflation)

p <- economic_data %>% 
  # filter(year == 2014) %>%
  filter(year >= 2000) %>% 
  arrange(desc(unemployment)) %>% 
  mutate(country = ifelse(row_number() <= 7, country, "Other")) %>% 
  ggplot(aes(x = unemployment, y = inflation, 
             # col = country, 
             frame = year,
             text = paste0("Country: ", country,
                           "<br><b>Unemployment</b>: ", round(unemployment, 2), "%",
                           "<br>Inflation: ", round(inflation, 2), "%"))) +
  geom_point(size = 4, col = "darkblue") +
  theme_bw()

ggplotly(p, tooltip = "text") %>% 
  animation_opts(1000, easing = "elastic")


xts_data <- xts(x = xts_data[-1], 
                order.by = xts_data$year)

dygraph(xts_data) %>% 
  dyRangeSelector()

