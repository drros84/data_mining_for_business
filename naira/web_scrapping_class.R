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
saved_webpage <- read_html(webpage_path)

team_position <- saved_webpage %>% 
  html_element(".team-position") %>% 
  html_text() %>% 
  str_squish()


single_team_content <- saved_webpage %>% 
  html_element(".single-team-content") %>% 
  html_elements("p") %>% 
  html_text()

background <- single_team_content[1]

education_and_courses <- saved_webpage %>% 
  html_element(".single-team-content") %>% 
  html_elements("ul") %>% 
  html_text()
education <- education_and_courses[1]

courses_taught <- education_and_courses[2]

publications_row <- single_team_content %>% 
  str_detect("Publications:") %>% 
  which()

email_row <- single_team_content %>% 
  str_detect("E-mail:") %>% 
  which()

single_team_content [(publications_row + 1): (email_row - 1)]
