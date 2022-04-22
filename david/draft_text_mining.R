
library(tidytext)
library(wordcloud)
library(tm)
library(textstem)
library(textdata)
library(topicmodels)
library(tidyverse)

user_reviews <- read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv")

# Have a look at the data using head()
head(user_reviews)

# Tokenise the text
tokenised_reviews <- user_reviews %>% 
  unnest_tokens(word, text)

tokenised_reviews %>% 
  filter(word %in% c("villager", "dog", "cat")) %>% 
  count(word)

# Tokenise into bigrams
user_reviews %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  count(word) %>% 
  arrange(desc(n)) 

# Look at the most frequent words
tokenised_reviews %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  xlab("Keywords") +
  ylab("Frequency")

# Now remove stop words and look at the most frequent words again
tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% View()
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  xlab("Keywords") +
  ylab("Frequency")

# Now stem the keywords
tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  mutate(word = stem_strings(word)) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  xlab("Keywords") +
  ylab("Frequency")

# Now lemmatise the strings
tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw() +
  xlab("Keywords") +
  ylab("Frequency")

# Make a wordcloud
wordcloud_frame <- tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(30)


wordcloud(wordcloud_frame$word, wordcloud_frame$n)


tokenised_reviews %>% 
  # mutate(grade = ifelse(grade == 0, 0, "other")) %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  mutate(grade = as.factor(grade)) %>% 
  count(grade, word) %>% 
  bind_tf_idf(word, grade, n) %>% 
  group_by(grade) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = grade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~grade, ncol = 4, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_bw()

tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "avg",
                           TRUE ~ "high")) %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  mutate(grade = as.factor(grade)) %>% 
  mutate(grade = fct_relevel(grade, levels = c("low", "avg", "high"))) %>% 
  count(grade, word) %>% 
  group_by(grade) %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  ggplot(aes(n, reorder_within(word, n, grade), fill = grade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~grade, ncol = 4, scales = "free") +
  labs(x = "frequency", y = NULL) +
  # scale_y_reordered() +
  theme_bw()

# Plot top tf-idf by low, avg or high
tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "average",
                           TRUE ~ "high")) %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  mutate(grade = as.factor(grade)) %>% 
  mutate(grade = fct_relevel(grade, levels = c("low", "average", "high"))) %>% 
  count(grade, word) %>% 
  bind_tf_idf(word, grade, n) %>% 
  group_by(grade) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = grade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~grade, ncol = 4, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_bw()

get_sentiments("afinn") %>% 
  tail()

tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "average",
                           TRUE ~ "high")) %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  mutate(grade = as.factor(grade)) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(grade) %>% 
  summarise(sentiment = mean(value)) %>% 
  ungroup()

tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  mutate(grade = as.factor(grade)) %>% 
  # mutate(date = format(date, "%Y %b")) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(date) %>% 
  summarise(sentiment = mean(value)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(aes()) +
  theme_bw()
  


contributions_frame <- tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  mutate(grade = as.factor(grade)) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(word) %>% 
  summarise(contr = sum(value)) %>% 
  ungroup()

contributions_frame %>% 
  arrange(desc(contr)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, contr), y = contr)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw()

contributions_frame %>% 
  arrange(desc(contr)) %>% 
  tail(10) %>% 
  ggplot(aes(x = reorder(word, contr), y = contr)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  theme_bw()
  


tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  mutate(grade = as.factor(grade)) %>% 
  group_by(grade, user_name, date) %>% 
  summarise(sentiment = mean(value)) %>% 
  ungroup() %>% 
  ggplot(aes(x = grade, y = sentiment)) +
  # geom_point(alpha = 0.5) +
  geom_boxplot(aes(fill = grade)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

# Topic modelling

reviews_dtm <- tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  filter(!(word %in% c("game", "animal", "crossing", "play", "player", "island"))) %>% 
  count(user_name, date, word) %>% 
  mutate(username_date = paste0(user_name, "_", "date")) %>% 
  cast_dtm(username_date, word, n)

reviews_lda <- LDA(reviews_dtm, k = 10, control = list(seed = "1234", alpha = 0.001))

reviews_topics <- tidy(reviews_lda, matrix = "beta")

reviews_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  ungroup() %>% 
  arrange(topic, -beta) %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(x = term, y = beta, fill = as.factor(topic))) +
  geom_col() +
  facet_wrap(~topic, scales = "free", ncol = 5) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

topic_titles <- reviews_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 3) %>% 
  mutate(row_n = row_number()) %>% 
  ungroup() %>% 
  select(-beta) %>% 
  pivot_wider(names_from = "row_n", values_from = "term") %>% 
  # select(-`4`) %>% 
  mutate(title = paste(`1`, `2`, `3`, sep = "_")) %>% 
  select(topic, title)


reviews_docs <- tidy(reviews_lda, matrix = "gamma")

reviews_gamma_sum <- reviews_docs %>% 
  mutate(document = str_extract(document, "[a-z]+")) %>% 
  group_by(document, topic) %>% 
  summarise(mean = mean(gamma),
            median = median(gamma),
            sd = sd(gamma)) %>% 
  ungroup()


topic_order <- reviews_gamma_sum %>% 
  left_join(topic_titles, by = "topic") %>% 
  select(-topic) %>% 
  rename(topic = title) %>% 
  mutate(topic = as.factor(topic)) %>% 
  group_by(topic) %>% 
  summarise(order_n = sum(mean)) %>% 
  ungroup() %>% 
  arrange(desc(order_n)) 


reviews_gamma_sum %>% 
  left_join(topic_titles, by = "topic") %>% 
  select(-topic) %>% 
  rename(topic = title) %>% 
  mutate(topic = as.factor(topic)) %>% 
  left_join(topic_order, by = "topic") %>% 
  ggplot(aes(x = document, y = reorder(topic, -order_n), fill = mean)) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  theme(legend.position = "None") +
  xlab("Readiness Assessment tool") +
  ylab("Topic") +
  ggtitle("Average topic intensity for Readiness Assessment tools")

