
#Spotify is one of the biggest audio streaming apps worldwide having almost half 
#a billion users monthly, having the iTunes(Apple) as its main competitor Spotify
#must always have up to date analysis to understand the preferances of their users
#and based on that improve the recomendation system within the service.

#In this report I will try to explain through Visualisation the overal picture and the corelaations
#between different variables


library(tidyverse)
install.packages("here")
library(here)
library(janitor)
install.packages("skimr")
library(skimr)
library(data.table)
library(RColorBrewer)
library(learnr)
library(ggthemes)
library(janeaustenr)
library(tidytext)
library(wordcloud2)
library(readxl)
library(fastDummies)
library(reshape2)
library(factoextra)
library(jiebaR)
library(tm)



spotify2020 <- read_csv("data_mining_for_business/maria/spotify_dataset.csv")
view(spotify2020)

# First of all we should start with the examining the overall data

head(spotify2020)

ncol(spotify2020)
nrow(spotify2020)
n_distinct(spotify2020)
is.na(spotify2020)
is.null(spotify2020)

spotify2020 %>% is.na() %>% sum()
spotify2020 %>% is.null() %>% sum()

str(spotify2020)
skim_without_charts(spotify2020)

spotify2020 %>% 
  summary()

#Exporing data Through visualizationz
# First of all below is shown the most streamed Artists

spotify2020 %>% 
  arrange(desc(Streams)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(Artist, -Streams), y = Streams)) +
  geom_col(fill="#1DB954")+
  coord_flip()+
  ggtitle("Artists with Most Streams")
  
#As a rsult we can Clearly See that the most Streames artist is Olivia Rodrigo, 
#followed by Maneskin and Lil Nas X

#Next We will look through the most sreamed genres

spotify2020 %>% 
  arrange(desc(Streams)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(Genre, -Streams), y = Streams)) +
  geom_col(fill="#1DB954")+
  coord_flip()+
  ggtitle("Genre with Most Streams")

# As we can see over the last 2 years the genres that broght the most streams were
# Pop and Italian inde=ie rock (Indie rock Italiano), which matches with the findings from previous 
#bar chart, Olivia Rodrigo sings in pop genre and Maneskin is italian indie rock band
#while Lil Nas x main genre is [lgbtq+ hip hop', 'pop rap'] which is 3rd most streamed
#genre.


#let's look through specipic songs as well

spotify2020 %>% 
  arrange(desc(Streams)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(`Song Name`, -Streams), y = Streams)) +
  geom_col(fill="#1DB954")+
  coord_flip()+
  ggtitle("Songs with Most Streams")

# In this 'Beggin' by Maneskin has .5% of all streams on Spotify. 
#With 48,633,449 streams total. The average number of streams on this list 
#is 6,369,877. However Olivia Rodrigo has 3 songs in this bar chart counting in total 
# 78,214,99 streams.

#As both Olivia Rodriguez and Maneskin are relatevely new artist and this success may be
# just temporary being "one hit" artis let's look at the most followed artists

spotify_followers <- spotify2020 %>%
  arrange(desc(`Artist Followers`)) %>%
  head(20)

ggplot(spotify_followers) + 
  geom_col(mapping = aes(x=`Artist Followers`, y=Artist)) + 
  labs(title = "Artists with Most Followers")

# In these figure we can see that the most followed artists are ariana grande 
# Ed Sheeran, While they do not have the most streams the past two years, 
# Arianna Grande and Ed Sheeran still dominate the list in followers.

ggplot(spotify_followers) + geom_col(mapping = aes(x=Genre, y=`Artist Followers`)) + 
  labs(title = "Genre with Most Followers")

#In The same logic the most followed genres are ['dance pop', 'pop', 'post-teen pop'] 
# and ['pop', 'uk pop'] which are respective genres of Arina Grande and Ed sheeran

# Already having the averall picture in terms of artists and genres let's look at 
# specific features of the songs

spotify2020 %>% 
  ggplot(aes(x = Tempo, y = Danceability)) +
  geom_point() +
  geom_smooth(aes(x=Tempo, y=Danceability))+
  ggtitle("Song Tempo vs Danceability")
  

# as we look at the chart we can see that there is middle point in Tempo where  
#song is considered "danceable". Too much or too little tempo does not work


spotify2020 %>% 
  ggplot(aes(x = Tempo, y = Popularity)) +
  geom_point() +
  geom_smooth(aes(x=Tempo, y=Popularity))+
  ggtitle("Song Tempo vs Song Popularity")

#However as seen in above figure tempo does not have much effect on the popularity 
#of a song. Although much like the last visualization, somewhere in the middle 
#of tempo you will find a good portion of the most popular.

spotify2020 %>% 
  ggplot(aes(x = Speechiness, y = Popularity)) +
  geom_point() +
  geom_smooth(aes(x = Speechiness, y = Popularity))+
  ggtitle("Speechiness vs Song Popularity")


#The popularity is completely effected though, based on how much speechiness
#(words spoken) is in the song.

#However the data for last two years may be misleading, so let's take a look
#at the same metrics throght the 2010-2019

spotify <- read_excel("data_mining_for_business/maria/Spotify 2010 - 2019 Top 100 Songs.xlsx")
View(spotify)

# Firs let's find out the top streaming artistist through the last decade

top_artist<-spotify%>%
  group_by(`top year`)%>%
  count(artist)%>%
  mutate(prop=n/sum(n))

top_artist[order(top_artist$n, decreasing=TRUE)[1:30], ]%>%
  ggplot(aes(as_factor(`top year`), prop, fill=artist))+
  geom_bar(stat='identity',  color = 'white', show.legend = F)+
  geom_text(aes(label=paste(artist)), size=2.5, color='black',
            position = position_stack(vjust = .5))+
  theme_bw()+
  labs(title='Top artists in each year', y='Percent', x='Year')

# We can see that the top artist are changing from year to year, so every year
# there is different leaders

#next let's look at the genres

top_genre<-spotify%>%
  group_by(`top year`)%>%
  count(`top genre`)%>%
  mutate(prop=n/sum(n))

top_genre[order(top_genre$n, decreasing=TRUE)[1:40], ]%>%
  ggplot(aes(as_factor(`top year`), prop, fill=`top genre` ))+
  geom_bar(stat='identity',  color = 'white', show.legend = F)+
  geom_text(aes(label=paste(`top genre` )), size=2.5, color='black',
            position = position_stack(vjust = .5))+
  theme_bw()+
  labs(title='Top genres in each year', y='Percent', x='Year')

# So everry Year the biggest portions of streamed songs were in the dance pop genre
# However from the below graph it can be seen that the number of songs in this genre
#is constantly decreasing 

spotify%>%
  group_by(`top year`)%>%
  count(`top genre`)%>%
  filter(`top genre` == 'dance pop')%>%
  ggplot(aes(as_factor(`top year`), n))+
  geom_point(color='#1DB954', size = 2)+
  geom_line(group=1, color='#1DB954', size=1)+
  theme_bw()+
  labs(title='Number of Dance pop in each year', y='Number of Dance pop', x='Year')

#while songs in Latin genre are constantly increasing having almost no proportion in 2018

spotify%>%
  group_by(`top year`)%>%
  count(`top genre`)%>%
  filter(`top genre` == 'latin')%>%
  ggplot(aes(as_factor(`top year`), n))+
  geom_point(color='pink', size = 2)+
  geom_line(group=1, color='Pink', size=1)+
  theme_bw()+
  labs(title='Number of Latin Songs in each year', y='Number of Latin', x='Year')
view(spotify2020)

###Conclusion

#Music industry is the most rapidly changing clusters in the world and we saw that
#in the above charts and graphs there is no constant leader who dominates or even the
#genre. In recent years it is apparent that music industry becomes more inclusive
#It can be seen withe rise of genres like latin, reggaetton and k-pop which were
# not considered traditionaly popular in terms of popularity in US.
# Before chances to be globally successful were quite low if you were ot of English speaking 
# world. But now with the rise of streaming services and social media industry is 
# Becoming more inclusive and diverse. That is especially vissible when we look 
#at the recent data where in 2020-2021 the 2nd most streamed artist is Italian band Maneskin.


