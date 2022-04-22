











library(tidyverse) 
library(janitor) 
library(ggridges) 
library(patchwork) 



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
  geom_density_ridges(aes(alpha = .03))

# gdp plot
p2 <- alc %>% 
  ggplot(aes(x = gdp_per_cap, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03))

# population plot
p3 <- alc %>% 
  ggplot(aes(x = population_est, y = continent, fill = continent)) +
  geom_density_ridges(aes(alpha = .03))
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
    alcohol_consumption > 15 ~ "high"
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

# what countries had low alcohol use?

alc %>%
  filter(alcohol_use == "low") %>%
  select(entity, continent)

