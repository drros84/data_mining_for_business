

library(tidytext)
library(wordcloud)
library(tm)
library(textstem)
library(textdata)

user_reviews <- read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv")

# Have a look at the data using head()
head(user_reviews)

# Tokenise the text
tokenised_reviews <- user_reviews %>% 
  unnest_tokens(word, text)

# Look at the top 10 words in the text
tokenised_reviews %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(10)

