install.packages("topicmodels")
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(textstem)
library(textdata)
library(topicmodels)

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

tokenised_reviews %>%
  head() 

tokenised_reviews %>% 
  filter(word == "dog")

user_reviews %>% 
  filter(str_detect(text, "dog"))


tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  #filter(word != "game") %>% 
  #mutate( word = stem_strings (word)) %>% 
  #mutate(word = lemmatize_strings(word)) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 

  
#ggplot(aes(x = word, y = n ))+
#geom_col()

#Make by order 
ggplot(aes(x = reorder (word, n) ,  y = n ))+
  geom_col(fill="024530")+
  coord_flip()+ 
  #theme_bw()+
  xlab("Word")+
  ylab("Frequency")+
  theme(axis.text = element_text(face = "bold", size = 12))

wordcloud_frame <- tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  head(30) 
 
  wordcloud (wordcloud_frame$word,
           wordcloud_frame$n)       
  
  
tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "average",
                           TRUE ~ "high")) %>% 
    anti_join(stop_words) %>% 
  filter(!str_detect (word, "[0-9]")) %>% 
    #filter(word != "game") %>% 
    #mutate( word = stem_strings (word)) %>% 
    #mutate(word = lemmatize_strings(word)) %>% 
    count(grade , word) %>% 
  group_by(grade) %>% 
  slice_max(n, n= 5 ) %>% 
  ungroup() %>% 
    ggplot(aes(x = reorder (word, n) ,  y = n ))+
    geom_col(fill="524541")+
    coord_flip()+ 
    #theme_bw()+
    xlab("Word")+
    ylab("Frequency")+
    theme(axis.text = element_text(face = "bold", size = 12))+
  facet_wrap(~grade, scales = "free")
  

tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "average",
                           TRUE ~ "high")) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect (word, "[0-9]")) %>% 
  #filter(word != "game") %>% 
  #mutate( word = stem_strings (word)) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  count(grade , word) %>%
  bind_tf_idf(word, grade, n) %>% 
  group_by(grade) %>% 
  slice_max(tf_idf, n= 5 ) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = reorder (word, tf_idf) ,  y = tf_idf ))+
  geom_col(fill="424010")+
  coord_flip()+ 
  #theme_bw()+
  xlab("Word")+
  ylab("Frequency")+
  theme(axis.text = element_text(face = "bold", size = 12))+
  facet_wrap(~grade, scales = "free")

#Sentiment analysis
tokenised_reviews %>% 
  mutate(grade = case_when(grade < 4 ~ "low",
                           grade < 8 ~ "average",
                           TRUE ~ "high")) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect (word, "[0-9]")) %>% 
  #filter(word != "game") %>% 
  #mutate( word = stem_strings (word)) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(grade) %>% 
  summarise(average = mean(value))


#topic modelling
reviews_dtm <- tokenised_reviews %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect (word, "[0-9]")) %>% 
  mutate(word = lemmatize_strings(word)) %>% 
  filter(!(word %in% c("game",
                         "animal",
                         "crossing",
                         "play",
                         "player",
                         "island"))) %>% 
count(user_name, date, word) %>% 
  mutate(username_date = paste0(user_name,
                                date)) %>% 
  cast_dtm(username_date, word, n)

reviews_lda <- LDA(reviews_dtm,k =10,
                     control = list(seed = "1234",
                                    alpha = 0.001))

reviews_topics <- tidy(reviews_lda, matrix = "beta")

reviews_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder_within(term, beta, topic), y = beta,
             fill = as.factor(topic))) +
  geom_col()+
  facet_wrap(~topic, scales = "free")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none")

