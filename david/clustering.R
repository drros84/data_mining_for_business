
library(tidyverse)
library(dendextend)

economic_data <- read_csv("https://raw.githubusercontent.com/drros84/euro_cluster_app/master/macro_dataset.csv") %>% 
  filter(Year == 2014) %>% 
  select(country = region, GDP, wages = average_annual_wage, inflation = cpi, 
         productivity, unemployment = unemployment_rate, research_investment = GERD, govt_debt) %>% 
  na.omit() %>% 
  column_to_rownames("country")

economic_data %>% 
  scale() %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() %>%
  set("branches_k_color", k = 5) %>%
  set("labels_col", k = 5) %>% 
  as.ggdend() %>% 
  ggplot(horiz = TRUE) +
  coord_polar(theta="x")

economic_data %>% 
  rownames_to_column("country") %>% 
  head(10) %>% 
  mutate(GDP = GDP / 1000000) %>% 
  ggplot(aes(x = reorder(country, GDP), y = GDP)) +
  geom_col() +
  theme_bw() +
  xlab("Country") +
  ylab("GDP ($ trillion)") +
  coord_flip() +
  theme(axis.text = element_text(face = "bold"))

read_csv("https://raw.githubusercontent.com/drros84/euro_cluster_app/master/macro_dataset.csv") %>% 
  filter(Year %in% c(2008, 2014)) %>% 
  select(country = region, GDP, wages = average_annual_wage, inflation = cpi, 
         productivity, unemployment = unemployment_rate, research_investment = GERD, govt_debt, Year) %>% 
  na.omit() %>% 
  select(country, Year, GDP) %>% 
  pivot_wider(names_from = Year, values_from = GDP) %>% 
  arrange(desc(`2014`)) %>% 
  head(10) %>% 
  mutate(row_n = row_number()) %>% 
  pivot_longer(names_to = "Year", values_to = "GDP", -c(country, row_n)) %>% 
  ggplot(aes(x = reorder(country, -row_n), y = GDP)) +
  geom_line(aes(group = row_n)) +
  geom_point(aes(group = Year, colour = Year), size = 4) +
  coord_flip() +
  theme_bw() +
  scale_colour_manual(values = c("lightblue", "darkblue")) +
  xlab("Country") +
  ggtitle("GDP in 2008 and 2014")

economic_data %>% 
  rownames_to_column("country") %>% 
  mutate(country = ifelse(row_number() > 6, "Other", country)) %>% 
  # head(10) %>% 
  ggplot(aes(x = GDP, y = wages, col = country)) +
  geom_point(size = 4) +
  theme_bw() +
  # scale_colour_continuous("blues")
  scale_colour_brewer(palette = "Paired")


read_csv("https://raw.githubusercontent.com/drros84/euro_cluster_app/master/macro_dataset.csv") %>% 
  # filter(Year %in% c(2008, 2014)) %>% 
  select(country = region, GDP, wages = average_annual_wage, inflation = cpi, 
         productivity, unemployment = unemployment_rate, research_investment = GERD, govt_debt, Year) %>% 
  na.omit() %>% 
  ggplot(aes(x = GDP, y = wages, group = country)) +
  geom_point()

data.frame(var1 = rnorm(1000), var2 = rnorm(1000),
           country = rep(c("Armenia", "Georgia", "USA", "UK", "Other"), 200)) %>% 
  ggplot(aes(x = var1, y = var2, group = country, col = country)) +
  geom_point(size = 2.5, alpha = 0.6) +
  theme_bw() 

set.seed(123)

random_ts <- data.frame(example = rnorm(100) %>% sin(), 
                        dates = c(1901:2000)) %>% 
  mutate(example = example + dates /2000) %>% 
  mutate(example = ifelse(dates > 1950 & example <0.2, 1, example))

# data.frame(example = rnorm(100) %>% sin(), 
#            dates = c(1901:2000)) %>% 
#   mutate(example = example + dates /2000) %>% 
random_ts %>% 
  ggplot(aes(x = dates, y = example)) +
  geom_line(size = 1) +
  theme_bw() +
  expand_limits(y = c(-0.5, 2.5))



# data.frame(example = rnorm(100) %>% sin(), 
#            dates = c(1901:2000)) %>% 
#   mutate(example = example + dates /2000) %>% 
random_ts %>% 
  ggplot(aes(x = dates, y = example)) +
  geom_point(alpha = 0.5) +
  geom_smooth(size = 1) +
  theme_bw() +
  expand_limits(y = c(-0.5, 2.5))
