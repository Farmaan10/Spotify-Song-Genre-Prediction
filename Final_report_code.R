library(tidyverse)
library(skimr)
library(e1071)
library(knitr)
library(dplyr)
library(corrplot)
library(rsample)
library(recipes)
library(parsnip)


#Reading the data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

#Understanding the Data
spotify_songs %>% head()
spotify_songs %>% skim_without_charts()

#Dropping the null values seen when data was skimmed
spotify_songs <- spotify_songs %>% drop_na()

#Dropping columns which act like index but don't add meaning
spotify_songs <- spotify_songs %>% 
  select(-c(track_id,track_album_id,track_album_name, 
            playlist_name,playlist_id))

spotify_songs$playlist_genre <- factor(spotify_songs$playlist_genre)

#We don't know genre, so subgenre also will be unkown. 
#Not needed for prediction
spotify_songs <- spotify_songs %>% 
  select(-playlist_subgenre)

#Getting meaning from Date
#Convert to year and then to decade
#Music genre popularity doesn't go away in a year thus decade is a better measure.
spotify_songs$track_album_release_date <- 
  as.Date(spotify_songs$track_album_release_date, format = "%Y")
spotify_songs$year <- year(spotify_songs$track_album_release_date)

summary(spotify_songs$year)
spotify_songs %>% ggplot(aes(x=year))+geom_histogram()
spotify_songs %>% ggplot(aes(x=year))+geom_boxplot()

spotify_songs <- spotify_songs %>% 
  mutate(decade = case_when(year <= 1979 ~ 'Pre-1980s',
                            year <= 1989 ~  '1980s',
                            year <= 1999 ~ '1990s',
                            year <= 2009 ~ '2000s',
                            year <= 2020 ~ '2010s',
                            TRUE ~ 'NA'))
spotify_songs$decade <- 
  factor(spotify_songs$decade, 
         levels = c('Pre-1980s', '1980s', '1990s', '2000s', '2010s'), 
         ordered = TRUE)

#Count of songs in each decade
spotify_songs %>% count(decade) %>% knitr::kable()
spotify_songs %>% ggplot(aes(x=decade))+geom_bar()

#No need for the date variable
spotify_songs <- spotify_songs %>% select(-c(track_album_release_date))

#Duration data cleaning
#Millisecond to Seconds
spotify_songs <- spotify_songs %>% 
  mutate(duration_sec = duration_ms*0.001)

#Dropping duration_ms
spotify_songs <- spotify_songs %>% 
  select(-c(duration_ms))

summary(spotify_songs$duration_sec)
spotify_songs %>% ggplot(aes(x=duration_sec))+geom_boxplot()

#Just the song being just 4 second long is unrealistic and extreme outlier
# but the max duration of around 518 seconds (8.6 minutes) is realistic. 
#Remove song that is 4 sec long
spotify_songs <- spotify_songs %>% filter(duration_sec>4)

#Now we have a realistic range 29.49 sec to 517.81 sec
summary(spotify_songs$duration_sec)

#We see that specific type of genre are either extremely long/short
#Thus can't remove these outliers, will take away meaningful information
quartiles <- quantile(spotify_songs$duration_sec, probs=c(.25, .75))
IQR <- IQR(spotify_songs$duration_sec)

Lower <- quartiles[1] - 1.5*IQR
Lower

Upper <- quartiles[2] + 1.5*IQR 
Upper

#Meaning gained from outliers in duration_sec
spotify_songs %>% filter(duration_sec<Lower) %>% 
  group_by(playlist_genre) %>% count() %>% kable()
#Rap genre has a short duration compared to others, 
#most outliers on the lower end are rap tracks

spotify_songs %>% filter(duration_sec>Upper) %>% 
  group_by(playlist_genre) %>% count() %>% kable()
#edm and rock genre have a long duration compared to others, 
#most outliers on the upper end are edm followed by rock tracks

#As we discussed above, mean of rap is lowest, edm highest and rock 2nd highest
spotify_songs %>% 
  group_by(playlist_genre) %>% 
  summarise(mean = mean(duration_sec)) %>% 
  kable()

spotify_songs %>% 
  group_by(playlist_genre) %>% 
  summarise(median = median(duration_sec)) %>%
  kable()

'
spotify_songs %>% 
  ggplot(aes(x=duration_sec)) + geom_histogram()
skewness(spotify_songs$duration_sec)

spotify_songs %>% 
  ggplot(aes(y= duration_sec, x= playlist_genre, fill=playlist_genre)) + 
  geom_boxplot()
'

'
Code to delete duration outliers
duration.bounds = quantile(spotify_songs$duration_sec, c(.1,.9))
df_spotify = spotify_songs %>%
  filter(duration_sec >= duration.bounds[1] & 
           duration_sec <= duration.bounds[2])
df_spotify %>% ggplot(aes(x=duration_sec))+geom_boxplot()
df_spotify %>% ggplot(aes(x=duration_sec))+geom_histogram()
skewness(df_spotify$duration_sec)
summary(df_spotify$duration_sec)
df_spotify %>% 
  ggplot(aes(y= duration_sec, x= playlist_genre, fill=playlist_genre)) + 
  geom_boxplot()
'

spotify_songs %>% 
    filter(track_popularity==0)

#Removing songs where popularity = 0, 
#this must have happened due to some error during collection.
#Songs of famous artists like Eminem, Maroon5, DjSnake, 50 Cent etc. is in the list.
#These can't have popularity = 0

spotify_songs <- spotify_songs %>% 
  filter(track_popularity>0)
spotify_songs

#Dropping Duplicates 
#Some songs have different genre for different entry
#Since arranging in descending, we are choosing the genre 
#based on the most popular track
spotify_songs <- spotify_songs %>%
  arrange(desc(track_popularity)) %>%
  group_by(track_name, track_artist) %>%
  dplyr::slice(1) %>%
  ungroup()
  
spotify_songs
#No Duplicates left

'
#Mode Minor or Major, 
spotify_songs$mode <- factor(spotify_songs$mode)
'

#Changing column names to easier form
names(spotify_songs) <- c("name", "artist", "popularity", "genre", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumantalness", "liveness", "valence", "tempo", "year", "decade", "duration")
spotify_songs %>% skim_without_charts()
summary(spotify_songs) %>% kable()

spotify_songs %>%
  count(genre) %>%
  knitr::kable()

# Boss' questions
#How the following factors can predict the song genre

#year song was released
spotify_songs %>% ggplot(aes(y=year, x = genre, fill=genre)) + geom_boxplot()
spotify_songs %>% ggplot(aes(x=year)) + 
  geom_histogram() + facet_wrap(~genre)

#Based on decade
spotify_songs %>% ggplot(aes(x=decade, fill = genre)) + 
  geom_bar(position = "fill", col = "black")

#Speechy 
spotify_songs %>% ggplot(aes(y=speechiness, x = genre)) + geom_boxplot()
spotify_songs %>% ggplot(aes(x=speechiness)) + 
  geom_histogram() + facet_wrap(~genre)

#Danceable
spotify_songs %>% ggplot(aes(y=danceability, x = genre)) + 
  geom_boxplot()
spotify_songs %>% ggplot(aes(x=danceability)) + 
  geom_histogram() + facet_wrap(~genre) 

#Tempo
spotify_songs %>% ggplot(aes(y=tempo, x = genre)) + geom_boxplot()
spotify_songs %>% ggplot(aes(x=tempo)) + 
  geom_histogram() + facet_wrap(~genre) +
  

# Selecting 1000 random samples from each genre
set.seed(1875837)
spotify_songs <- spotify_songs %>%
  group_by(genre) %>%
  sample_n(size=1000) %>% ungroup()
spotify_songs %>% head()

summary_data <- spotify_songs %>%
  group_by(genre) %>%
  summarise(Count = n())
summary_data

#Analysis
#During your analysis, the Spotify founders would like you to specifically explore the following questions:
#All features vs genre
track_features <- c(names(spotify_songs)[5:15],
                    names(spotify_songs)[18])

spotify_songs %>%
  select(c('genre', track_features)) %>%
  pivot_longer(cols = track_features) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = genre), alpha = 0.5) +
  facet_wrap(~name, ncol = 3, scales = 'free') +
  labs(title = 'Spotify Audio Feature Density - by Genre',
       x = '', y = 'density') +
  theme(axis.text.y = element_blank())

#Correlation Plot
spotify_songs %>%
  select(5:15) %>% #these are the song attributes
  scale() %>%
  cor() %>%
  corrplot(method = 'color', 
           type = 'full',
           addCoef.col = "grey",
           number.cex = 0.5,
           tl.cex = .6,
           main = 'Correlation Plot of track features',
           mar = c(1,0,2,0))
#Energy and loudness highly correlated
#Accoustic negatively correlated with energy.
#we can drop here or further in recipe

#Does the popularity of songs differ between genres?
#Popularity vs genre
spotify_songs %>% ggplot(aes(x = popularity)) + geom_histogram()
skewness(spotify_songs$popularity)
#popularity is approximately symmetric

#Yes popularity of songs differs between genre.
#Pop tends to be the most popular, edm the least, r&b the 2nd least 
#while the other three (rap, rock and latin) pretty close
spotify_songs %>% 
  ggplot(aes(y = popularity, x = genre, fill = genre)) + 
  geom_boxplot()

mean.popularity <- spotify_songs %>%
  group_by(genre) %>%
  summarize(mean.pop = mean(popularity))

spotify_songs %>% ggplot() +
  geom_histogram(aes(popularity, col=popularity), 
                 color = 'blue', alpha = .75) +
  geom_vline(data = mean.popularity, col = 'red',
             mapping = aes(xintercept = mean.pop))+
  geom_label(data = mean.popularity, col = 'red',
             aes(x = mean.pop, y = -5, 
                 label = paste('Mean:', round(mean.pop,1)))) +
  facet_wrap(~genre) +
  ggtitle('Genre based Song Popularity') + 
  labs(caption = "We can see that pop music is the most popular while edm the least")


#Is there a difference in speechiness for each genre?
#Speechiness vs genre
spotify_songs %>% ggplot(aes(x = speechiness)) + geom_histogram()
skewness(spotify_songs$speechiness)
# Highly right skewed, thus need transformation
# Inverse transform works best here


spotify_songs %>% 
  ggplot(aes(y = 1/speechiness, x = genre, fill = genre)) + 
  geom_boxplot()


spotify_songs$speechiness <- log(spotify_songs$speechiness)
spotify_songs %>% ggplot(aes(x = speechiness)) + geom_histogram()
skewness(spotify_songs$speechiness)

spotify_songs %>% 
  ggplot(aes(y = speechiness, x = genre, fill = genre)) + 
  geom_boxplot()

# We see, rap is the most speechy, much more than others.
# Rock is the least speechy, pop second least
# edm, latin and r&b almost same

mean.speechiness <- spotify_songs %>%
  group_by(genre) %>%
  summarize(mean.speech = mean(speechiness))

spotify_songs %>% ggplot() +
  geom_histogram(aes(speechiness, col=speechiness), 
                 color = 'blue', alpha = .75) +
  geom_vline(data = mean.speechiness, col = 'red',
             mapping = aes(xintercept = mean.speech))+
  geom_label(data = mean.speechiness, col = 'red',
             aes(x = mean.speech, y = -5, 
                 label = paste('Mean:', 
                               round(mean.speech,3)))) +
  facet_wrap(~genre) +
  ggtitle('Genre based Song Speechiness') + 
  labs(caption = "We can see that rap tracks have the most words, rock the least")

#How does track popularity change over time?
#Track popularity vs year
#Yearly average popularity of the tracks
# Initial years has less songs, hence skewed value
# Since then pretty constant
spotify_songs %>% group_by(year) %>%
  summarise(Average_Popularity = mean(popularity)) %>% 
  ggplot(aes(x=year, y= Average_Popularity)) + geom_point() +
  geom_smooth()
#Music in general/average is as popular now as it was previously

#Decade vs genre
#With time how popularity of each genre changes
spotify_songs %>% group_by(decade) %>%
  filter(n()>2) %>% ggplot() +
  geom_boxplot(aes(x=decade, y = popularity, fill = decade))+
  facet_wrap(~genre)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle('Genre based popularity each decade') + 
  labs(caption = "We can see that edm was pretty much nonexistent pre 2000, rap very less pre 1980s")
#EDM came into existance in late 1990s, and has always been one of the least popular.
#Rap gained popularity in 1990s, but had very few tracks before 1990.
#Rock used to be very popular but lesser now.

#Other variables
#Danceability
spotify_songs %>% ggplot(aes(x = danceability)) + geom_histogram()
skewness(spotify_songs$danceability)

spotify_songs %>% 
  ggplot(aes(y = danceability, x = genre, fill = genre)) + 
  geom_boxplot()

spotify_songs$danceability <- (spotify_songs$danceability)^2
spotify_songs %>% ggplot(aes(x = danceability)) + geom_histogram()
skewness(spotify_songs$danceability)

spotify_songs %>% 
  ggplot(aes(y = danceability, x = genre, fill = genre)) + 
  geom_boxplot()

#Energy
spotify_songs %>% ggplot(aes(x = energy)) + geom_histogram()
skewness(spotify_songs$energy)

spotify_songs %>% 
  ggplot(aes(y = energy, x = genre, fill = genre)) + 
  geom_boxplot()

spotify_songs$energy <- (spotify_songs$energy)^2
spotify_songs %>% ggplot(aes(x = energy)) + geom_histogram()
skewness(spotify_songs$energy)
spotify_songs %>% 
  ggplot(aes(y = energy, x = genre, fill = genre)) + 
  geom_boxplot()

#Key
spotify_songs %>% ggplot(aes(x = key)) + geom_histogram()
skewness(spotify_songs$key)

spotify_songs %>% 
  ggplot(aes(y = energy, x = genre, fill = genre)) + 
  geom_boxplot()

#loudness
spotify_songs %>% ggplot(aes(x = loudness)) + geom_histogram()
skewness(spotify_songs$loudness)

spotify_songs %>% 
  ggplot(aes(y = loudness, x = genre, fill = genre)) + 
  geom_boxplot()

'log transform second best'
spotify_songs$loudness <- (abs(spotify_songs$loudness))^(1/3)
spotify_songs %>% ggplot(aes(x = loudness)) + geom_histogram()
skewness(spotify_songs$loudness)
spotify_songs %>% 
  ggplot(aes(y = loudness, x = genre, fill = genre)) + 
  geom_boxplot()

#Mode
spotify_songs %>% ggplot(aes(x = mode)) + geom_histogram()
skewness(spotify_songs$mode)

#Acousticness
spotify_songs %>% ggplot(aes(x = acousticness)) + geom_histogram()
skewness(spotify_songs$acousticness)

spotify_songs %>% 
  ggplot(aes(y = acousticness, x = genre, fill = genre)) + 
  geom_boxplot()

spotify_songs$acousticness <- (spotify_songs$acousticness)^(1/3)
spotify_songs %>% ggplot(aes(x = acousticness)) + geom_histogram()
skewness(spotify_songs$acousticness)

#Instrumentalness Check
spotify_songs %>% ggplot(aes(x = instrumantalness)) + geom_histogram()
skewness(spotify_songs$instrumantalness)

spotify_songs %>% 
  ggplot(aes(y = instrumantalness, x = genre, fill = genre)) + 
  geom_boxplot()

#edm full of instrumentalness. Others lack and mostly 0
spotify_songs$instrumantalness <- (spotify_songs$instrumantalness)^(1/3)
spotify_songs %>% ggplot(aes(x = instrumantalness)) + geom_histogram()
skewness(spotify_songs$instrumantalness)
spotify_songs %>% 
  ggplot(aes(y = instrumantalness, x = genre, fill = genre)) + 
  geom_boxplot()

#Liveness
spotify_songs %>% ggplot(aes(x = liveness)) + geom_histogram()
skewness(spotify_songs$liveness)

spotify_songs %>% 
  ggplot(aes(y = liveness, x = genre, fill = genre)) + 
  geom_boxplot()

spotify_songs$liveness <- log(spotify_songs$liveness)
spotify_songs %>% ggplot(aes(x = liveness)) + geom_histogram()
skewness(spotify_songs$liveness)

spotify_songs %>% 
  ggplot(aes(y = liveness, x = genre, fill = genre)) + 
  geom_boxplot()

#Valence
spotify_songs %>% ggplot(aes(x = valence)) + geom_histogram()
skewness(spotify_songs$valence)

spotify_songs %>% 
  ggplot(aes(y = valence, x = genre, fill = genre)) + 
  geom_boxplot()

#Tempo
spotify_songs %>% ggplot(aes(x = tempo)) + geom_histogram()
skewness(spotify_songs$tempo)

spotify_songs %>% 
  ggplot(aes(y = tempo, x = genre, fill = genre)) + 
  geom_boxplot()

spotify_songs$tempo <- log(spotify_songs$tempo)
spotify_songs %>% ggplot(aes(x = tempo)) + geom_histogram()
skewness(spotify_songs$tempo)

spotify_songs %>% 
  ggplot(aes(y = tempo, x = genre, fill = genre)) + 
  geom_boxplot()

#Duration
spotify_songs %>% ggplot(aes(x = duration)) + geom_histogram()
skewness(spotify_songs$duration)

spotify_songs %>% 
  ggplot(aes(y = duration, x = genre, fill = genre)) + 
  geom_boxplot()

spotify_songs$duration <- log(spotify_songs$duration)
spotify_songs %>% ggplot(aes(x = duration)) + geom_histogram()
skewness(spotify_songs$duration)

spotify_songs %>% 
  ggplot(aes(y = duration, x = genre, fill = genre)) + 
  geom_boxplot()

#Year
#mostly new songs in data
spotify_songs <- spotify_songs%>%select(-c(year, name, artist))
'
spotify_songs %>% ggplot(aes(x = year)) + geom_histogram()
skewness(spotify_songs$year)

spotify_songs %>% 
  ggplot(aes(y = year, x = genre, fill = genre)) + 
  geom_boxplot()
'

'
spotify_songs <- spotify_songs %>% 
select(track_album_release_date, speechiness, danceability, tempo, playlist_genre)
spotify_songs %>% head()
'
spotify_songs %>% skim_without_charts()
#15 columns, 14 predictor, 1 (genre) outcome


##PCA (do in recipe (step_pca))

predictors <- 
  spotify_songs %>% 
  select(-genre)
predictors

#PCA wants numbers can be done in recipe
predictors <- 
  predictors %>% 
  mutate_all(as.numeric)
predictors
sp_copy<- spotify_songs
PCA <- princomp(predictors, scores = TRUE)
sp_copy$PC1 <- PCA$scores[,1]
sp_copy$PC2 <- PCA$scores[,2]
sp_copy$PC3 <- PCA$scores[,3]
sp_copy$PC4 <- PCA$scores[,4]
sp_copy$PC5 <- PCA$scores[,5]
sp_copy$PC6 <- PCA$scores[,6]

spotify_recipe <- recipe(genre~., data = songs_final) %>%
  step_dummy(decade) %>%
  step_mutate(danceability = max(danceability+1)-danceability, 
              energy = max(energy+1)-energy,
              loudness = max(loudness+1)-loudness) %>%
  step_log(speechiness, danceability, energy, 
           loudness, liveness,
           tempo, duration)%>%
  step_log(instrumantalness,offset = 1) %>%
  step_sqrt(acousticness) %>%
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  step_pca(all_predictors()) %>%
  prep()

spotify_recipe
