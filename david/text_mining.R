
library(tidyverse)
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

user_reviews %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) 

# Look at the top 10 words in the text
tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  # filter(word != "game") %>% 
  # mutate(word = stem_strings(word)) %>% 
  mutate(word = lemmatize_strings(word)) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  # theme_classic() +
  # theme_dark() +
  xlab("Word") +
  ylab("Frequency") +
  theme(axis.text = element_text(face = "bold",
                                 size = 12))

wordcloud_frame <- tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_strings(word)) %>%
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(30)

wordcloud(wordcloud_frame$word,
          wordcloud_frame$n)



tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "avg",
                           TRUE ~"high")) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  # filter(word != "game") %>% 
  # mutate(word = stem_strings(word)) %>% 
  mutate(word = lemmatize_strings(word)) %>%
  count(grade, word) %>% 
  group_by(grade) %>% 
  slice_max(n, n = 5) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  # theme_classic() +
  # theme_dark() +
  xlab("Word") +
  ylab("Frequency") +
  theme(axis.text = element_text(face = "bold",
                                 size = 12)) +
  facet_wrap(~grade, scales = "free")



tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "avg",
                           TRUE ~"high")) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  # filter(word != "game") %>% 
  # mutate(word = stem_strings(word)) %>% 
  mutate(word = lemmatize_strings(word)) %>%
  count(grade, word) %>%
  bind_tf_idf(word, grade, n) %>% 
  group_by(grade) %>% 
  slice_max(tf_idf, n = 5) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  xlab("Word") +
  ylab("Frequency") +
  theme(axis.text = element_text(face = "bold",
                                 size = 12)) +
  facet_wrap(~grade, scales = "free")
