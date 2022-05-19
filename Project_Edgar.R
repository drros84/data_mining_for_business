











library(tidyverse) 
library(janitor) 
library(ggridges) 
library(patchwork) 
library(dendextend)



# import data
alc <- read_csv ("alcohol-consumption-vs-gdp-per-capita.csv")

# clean data and rename vars 
alc <- alc %>% 
  clean_names() %>% # clean up column names
  mutate(alcohol_consumption = total_alcohol_consumption_per_capita_liters_of_pure_alcohol_projected_estimates_15_years_of_age) %>% # shortening column name
  mutate(gdp_per_cap = gdp_per_capita_ppp_constant_2017_international) %>% # shortening column name
  mutate(population_est = population_historical_estimates) %>% # shortening column name
  select(entity, year, alcohol_consumption, gdp_per_cap, population_est, continent) %>% # selecting wanted columns
  drop_na() %>% # remove n/a's 
  filter(year == 2015) # filter for year - 2015


# view data 
head(alc)


# visualize / explore data

# alc consumption plot
p1 <- alc %>% 
  ggplot(aes(x = alcohol_consumption, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03)) +
  theme(legend.position = "none")

# gdp plot
p2 <- alc %>% 
  ggplot(aes(x = gdp_per_cap, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03)) +
  theme(legend.position = "none")

# population plot
p3 <- alc %>% 
  ggplot(aes(x = population_est, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03)) +
  theme(legend.position = "none")
p1 / p2 / p3



alc %>%
  ggplot(aes(x = log(population_est), y = gdp_per_cap)) +
  geom_point(aes(color = alcohol_consumption)) + 
  facet_wrap(~continent)


#analysis
alc <- alc %>%
  mutate(alcohol_use = case_when(
    alcohol_consumption < 5 ~ "low",
    alcohol_consumption < 10 ~ "medium",
    alcohol_consumption < 12 ~ "medium_high",
    alcohol_consumption > 12 ~ "high"
  ))


head(alc)



alc %>%
  drop_na() %>%
  ggplot(aes(x = log(population_est), y = gdp_per_cap)) +
  geom_jitter(aes(color = alcohol_use)) + 
  facet_wrap(~alcohol_use, nrow = 2)


# which countries in 2015 had high alcohol use?
alc %>%
  filter(alcohol_use == "high") %>%
  select(entity, continent)

# which countries in 2015 had medium_high alcohol use?
alc %>%
  filter(alcohol_use == "medium_high") %>%
  select(entity, continent)

# which countries in 2015 had medium alcohol use?
alc %>%
  filter(alcohol_use == "medium") %>%
  select(entity, continent)

# what countries had low alcohol use?

alc %>%
  filter(alcohol_use == "medium") %>%
  select(entity, continent) %>%
  write.csv("alc_m.csv")



# Plot cluster dendrogram for hierarchical clustering
  
  
  alc %>% 
  filter(alcohol_use == "medium_high") %>%
  select(region = entity, gdp_per_cap, alcohol_consumption, population_est) %>% 
  na.omit() %>% 
  column_to_rownames("region")%>%
  scale() %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() %>%
  set("labels_col", k =6) %>%
  set("branches_k_color", k = 6 )%>%
  as.ggdend() %>%
  ggplot(horiz = TRUE) 
  
  alc %>% 
    filter(alcohol_use == "high") %>%
    select(region = entity, gdp_per_cap, alcohol_consumption, population_est) %>% 
    na.omit() %>% 
    column_to_rownames("region")%>%
    scale() %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram() %>%
    set("labels_col", k =4) %>%
    set("branches_k_color", k = 4 )%>%
    as.ggdend() %>%
    ggplot(horiz = TRUE) 

  alc %>% 
    filter(alcohol_use == "medium") %>%
    select(region = entity, gdp_per_cap, alcohol_consumption,population_est) %>% 
    na.omit() %>% 
    column_to_rownames("region")%>%
    scale() %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram() %>%
    set("labels_col", k =10) %>%
    set("branches_k_color", k = 10 )%>%
    as.ggdend() %>%
    ggplot(horiz = TRUE) 
  
  
    
  alc %>% 
    filter(alcohol_use == "low") %>%
    select(region = entity, gdp_per_cap, alcohol_consumption,population_est) %>% 
    na.omit() %>% 
    column_to_rownames("region")%>%
    scale() %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram() %>%
    set("labels_col", k =10) %>%
    set("branches_k_color", k = 10 )%>%
    as.ggdend() %>%
    ggplot(horiz = TRUE) 
  
 
  
  alc %>% 
    select(region = continent, gdp_per_cap, alcohol_consumption,population_est) %>% 
    na.omit() %>% 
    group_by(region) %>%
    summarise(gdp_per_cap = mean(gdp_per_cap),
              alcohol_consumption = mean(alcohol_consumption),
              population_est = mean(population_est)) %>%
    ungroup() %>%
    column_to_rownames("region")%>%
    scale() %>% 
    dist() %>% 
    hclust() %>% 
    as.dendrogram() %>%
    set("labels_col", k = 4) %>%
    set("branches_k_color", k = 4 )%>%
    as.ggdend() %>%
    ggplot(horiz = TRUE) 
  
  
  
  
  
