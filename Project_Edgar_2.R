
#Company owns a chain of stores across Russia that sell a variety of alcoholic drinks. 
#The company recently ran a wine promotion in Saint Petersburg that was very successful. 
#Due to the cost to the business, it isn’t possible to run the promotion in all regions.
#The marketing team would like to target 10 other regions that have similar buying habits 
 #to Saint Petersburg where they would expect the promotion to be similarly successful.

#The marketing team has sourced you with historical sales volumes per capita for several different drinks types.

"year" - year (1998-2016)
"region" - name of a federal subject of Russia. It could be oblast, 
            republic, krai, autonomous okrug, federal city and a single autonomous oblast
"wine" - sale of wine in litres by year per capita
"beer" - sale of beer in litres by year per capita
"vodka" - sale of vodka in litres by year per capita
"champagne" - sale of champagne in litres by year per capita
"brandy" - sale of brandy in litres by year per capita

# Packages
library(tidyverse)
library(patchwork)
library(knitr)

# Importing data
data <- read_csv("russian_alcohol_consumption.csv")

# Removing warns
options(warn=-1)

# Controling plots
fig <- function(width){
  options(repr.plot.width = width, repr.plot.height = width * 0.618, repr.plot.res = 200, warn=-1)
}

fig(8)

#Data Preparation
#This scenario is perfect to apply unsupervised machine learning technique hierarchical cluster. 
#On this way, we need to be sure that we do not have any missing values. 
#Let’s start our analysis with this. 
#It is important to say that we will use svc for sales volume per capita, the basic unit that we will work.


# Reshaping
data_clean <- data %>%
  pivot_longer(
    wine:brandy,
    names_to = "drink",values_to = "svc"
  ) %>% # svc = sales volumes per capita
  mutate(
    region = as.factor(region),
    drink = as.factor(str_to_title(drink))
  )

# Finding regions with data missing
region_missing <-
  data_clean %>%
  filter(is.na(svc)) %>%
  distinct(region) %>%
  select(region)

# Analyzing region data missing
data_clean %>%
  semi_join(region_missing, by = "region") %>%
  ggplot(aes(x = year, y = svc, color = drink)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ region) +
  labs(
    x = "Year",
    y = "SVC (litres by year per capita)",
    title = "There are four regions with missing values...",
    color = "Drinks"
  )

#Chechen Republic and Republic of Ingushetia cannot be use on our model because them have a lot of 
#missing values.
#On the other hand, Republic of Crimea and Sevastopol only can be used if we consider the last 
#three years and we plan to use more than this period, so, let's drop all data of them.

# Droping Republic of Ingushetia and Chechen Republic
data_clean <-
  data_clean %>%
  anti_join(region_missing, by = "region")

#Analyzing Saint Petersburg Sales
#Before we dig into all region data, let’s visualize Saint Petersburg curves

# Sales at Saint Petersburg
data_st_petersburg <-
  data_clean %>%
  filter(region == "Saint Petersburg")

data_st_petersburg_tot <- data_st_petersburg %>%
  group_by(year, region) %>%
  summarize(svc = sum(svc)) %>%
  ungroup() %>%
  mutate(drink = "Total")

data_st_petersburg %>%
  bind_rows(data_st_petersburg_tot) %>%
  mutate(drink = fct_reorder(drink, svc)) %>% 
  ggplot(aes(x = year, y = svc, color = drink)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "blank") + 
  facet_wrap(~ drink, scales = "free_y") +
  theme(legend.position = "None") +
  labs(
    x = "Year",
    y = "SVC (litres by year per capita)",
    title = "Saint Petersburg drink sales...",
    subtitle = "...some notes are presented after the plot.",
    color = "Drinks"
  )

#brandy, champagne, and wine show similar sales. 
#However, vodka and beer show an increase some year ago and them fall. 
#Considering that vodka and beer sales are higher than the rest of drinks,
#them behavior is more influential and the total sales show almost the same shape of them.

#Modeling Data
#Our strategy to apply the clustering technique will be split our model into two steeps:
#1st: region selection considering Saint Petersburg market size and behavior; and,
#2nd: drink selection considering Wine in Saint Peterburg on the region of 1st steep.
#On each steep we will try to find the closer number of regions and drinks that our marketing team 
#ask plus one (10 + 1), considering Saint Petersburg is the plus one. Let's do some data wrangling.

# Reshaping the data
data_cluster_drink <- data_clean %>%
  pivot_wider(names_from = year, values_from = svc)

data_cluster_tot <- data_cluster_drink %>%
  group_by(region) %>%
  summarize(across(where(is.numeric), ~ sum(.x))) %>%
  ungroup()

data_cluster_tot_ <- data_cluster_tot %>% select(-1)

#our data now is prepared to apply the cluster techniques. 

# Creating the Hierarchical Cluster
dist_mdl <- dist(data_cluster_tot_, method = "euclidean")

hc_mdl <- hclust(dist_mdl, method = "complete")

# Cutting the model until we can analyze the cluster
k = 1
n = 81

while (n > 11) {
  
  k = k + 1
  
  hc_mdl_cut <- cutree(hc_mdl, k = k)
  
  data_cluster_tot_hc <- data_cluster_tot %>% transmute(region, cluster = hc_mdl_cut)
  
  data_cluster_tot_hc_filter <- data_cluster_tot_hc %>%
    semi_join(
      filter(data_cluster_tot_hc, region == "Saint Petersburg"),
      by = "cluster")
  
  n = pull(count(data_cluster_tot_hc_filter))
  
}

# Selecting the correct model
hc_mdl_cut <- cutree(hc_mdl, k = k)

data_cluster_tot_hc <- data_cluster_tot %>% transmute(region, cluster = hc_mdl_cut)

data_cluster_tot_hc_filter <- data_cluster_tot_hc %>%
  semi_join(
    filter(data_cluster_tot_hc, region == "Saint Petersburg"),
    by = "cluster")

# Ploting the results
region_result <- data_cluster_tot %>%
  semi_join(data_cluster_tot_hc_filter, by = "region") %>%
  pivot_longer("1998":"2016", names_to = "year", values_to = "svc") %>%
  mutate(year = as.numeric(year))

region_result %>% ggplot(aes(x = year, y = svc)) +
  geom_line(data = rename(region_result, region2 = region), aes(group = region2), color = "grey") +
  geom_line(color = "red") + 
  geom_line(data = filter(rename(region_result, region2 = region), region2 == "Saint Petersburg"), 
            aes(group = region2), color = "black") +
  geom_hline(aes(yintercept = 0), linetype = "blank") +
  facet_wrap(~ region) +
  labs(
    x = "Year",
    y = "Sales Volume per Capita (SVC)",
    title = "Sales Volume per Capita by Regions",
    subtitle = "Red line: region sales / Black line: Saint Peterburg sales"
  )

#The regions names

kable(unique(region_result$region), col.names = "Regions", caption = "Regions select on 1st steep")

#After some analysis, we found 10 regions with the same behavior and market size then Saint Petersburg.
#Now, let's find one or more drinks per region that match with wine at Saint Petersburg

# Reshaping the data
data_cluster_drink_filter <- data_cluster_drink %>%
  semi_join(data_cluster_tot_hc_filter, by = "region") %>%
  filter(region == "Saint Petersburg" & drink == "Wine" | region != "Saint Petersburg")

data_cluster_drink_filter_ <- data_cluster_drink_filter %>% select(-1:-2)

# Creating the Hierarchical Cluster
dist_mdl2 <- dist(data_cluster_drink_filter_, method = "euclidean")

hc_mdl2 <- hclust(dist_mdl2, method = "average")

# Cutting the model until we can analyze the cluster
k = 2
n = 51

while (n > 11) {
  
  k = k + 1
  
  hc_mdl_cut2 <- cutree(hc_mdl2, k = k)
  
  data_cluster_drink_filter_hc <- data_cluster_drink_filter %>%
    transmute(region, drink,cluster = hc_mdl_cut2)
  
  data_cluster_drink_filter_hc_filter <- data_cluster_drink_filter_hc %>%
    semi_join(
      filter(data_cluster_drink_filter_hc, region == "Saint Petersburg"),
      by = "cluster")
  
  n = pull(count(data_cluster_drink_filter_hc_filter))
}

# Selecting the correct model
hc_mdl_cut2 <- cutree(hc_mdl2, k = k)

data_cluster_drink_filter_hc <- data_cluster_drink_filter %>% 
  transmute(region, drink,  cluster = hc_mdl_cut2)

data_cluster_drink_filter_hc_filter <- data_cluster_drink_filter_hc %>%
  semi_join(
    filter(data_cluster_drink_filter_hc, region == "Saint Petersburg"),
    by = "cluster")

# Ploting the results
drink_result <- data_cluster_drink %>%
  semi_join(data_cluster_drink_filter_hc_filter, by = c("region","drink")) %>%
  pivot_longer("1998":"2016", names_to = "year", values_to = "svc") %>%
  mutate(year = as.numeric(year),
         region_drink = str_c(region, drink, sep = " - "))

drink_result %>% ggplot(aes(x = year, y = svc)) +
  geom_line(data = rename(drink_result, region_drink2 = region_drink),
            aes(group = region_drink2), color = "grey") +
  geom_line(data = filter(rename(drink_result, region_drink2 = region_drink),
                          region == "Saint Petersburg"), aes(group = region_drink2), color = "black") +
  geom_line(color = "red")+
  geom_hline(aes(yintercept = 0), linetype = "blank") +
  facet_wrap(~ region_drink) +
  labs(
    x = "Year",
    y = "Sales Volume per Capita (SVC)",
    title = "Sales Volume per Capita by Regions and Drink",
    subtitle = "Red line: region - drink sales / Black line: Saint Peterburg - Wine sales"
  )

#Summarizing, on the first steep we select some regions then, on the second step, we select some drinks 
#based on wine sales in Saint Petersburg. Let's present the regions and drink names below.

kable(unique(select(drink_result, region, drink)), 
      col.names = c("Regions","Drink"), caption = "Regions and drink select on 2nd steep")

#Results
#We select ten pairs of region and drink to run our promotion 

drink_result %>% 
  filter(region != "Saint Petersburg") %>%
  ggplot(aes(x = year, y = svc)) +
  geom_line(color = "red") +
  geom_line(data = filter(rename(drink_result, region_drink2 = region_drink), 
                          region == "Saint Petersburg"), aes(group = region_drink2), color = "black")+
  geom_hline(aes(yintercept = 0), linetype = "blank") +
  facet_wrap(~ region_drink) +
  labs(
    x = "Year",
    y = "Sales Volume per Capita (SVC)",
    title = "Regions to run the drink promotion sales",
    subtitle = "Red line: region - drink sales / Black line: Saint Peterburg - Wine sales"
  )
