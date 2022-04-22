
library(tidyverse)
library(rvest)

aua_path <- "https://people.aua.am/team_member/"

aua_names <- c("Mane Beglaryan",
               "Michael Kouchakdjian",
               "Fanis Varvoglis",
               "Robert Aboolian",
               "Francisco Bastida",
               "Mohamed Ezz")

webpage_path <- paste0(aua_path, aua_names[1] %>% 
         tolower() %>% 
         str_replace_all(" ", "-"),
         "/")






