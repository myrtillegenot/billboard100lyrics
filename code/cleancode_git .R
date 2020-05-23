# Author: Myrtille Genot
# Code : Final Top 100 Dataset 
# Date: 04/14/2020





# Clean Code 
library('haven') 
library('dplyr') # Data wrangling & manipulation
library('AER')   
library('plm')
library('stargazer')
library('ggplot2') # For data visualizations & graphs
library('forecast')
library('tseries')
library('zoo')
library('naniar') # For dealing with NA values
library('lubridate')
library('readr')
library('tidytext') # For unnest_tokens
library('stringr') # For managing text
library('spotifyr') #For accessing spotify data
library('data.table') 
library('fuzzyjoin')
library('purrr')
library('rvest') # For web scraping
library('fuzzyjoin')
library("httr")
library("jsonlite") 
library("stringr")


# Get Billboard data set 
billboard <- read_csv("~/Desktop/portfolio/billboard/data/billboard_full.csv")  
# Get Genius data set 
genius_full <- read_csv("~/Desktop/portfolio/billboard/data/genius_fullsong.csv")
genius <- genius_full %>% 
  select(31, 18, 20) %>% 
  rename( artist = result.primary_artist.name,
          song = result.title, 
          url = result.url)

#Get Spotify data set 
spotify_full <- read_csv("~/Desktop/portfolio/billboard/data/spotify_fullsongs.csv")
spotify <- spotify_full %>% 
  select(3,2,7,8,9) %>% 
  rename(artist =1,
         artistid =2,
         song =3,
         songid =4,
         date = 5)

#Keep what I need from each data frame and make sure it is in the correct format. 

#Data Types 
sapply(billboard, class)
sapply(genius, class)
sapply(spotify, class)

# I want a dataset that has the features & lyrics for as many songs in Billboard as possible.

# Try by pairing 
billboard$pair <- paste(billboard$artist, "-", billboard$song)
genius$genius_pair <- paste(genius$artist, "-", genius$song)
spotify$spotify_pair <-  paste(spotify$artist, "-", spotify$song)

write_csv(billboard, "billboardpairs.csv")
write_csv(genius, "geniuspairs.csv")
write_csv(spotify, "spotifypairs.csv")

#Load pairs that are matched 

billboard_genius <- read_csv("~/Desktop/portfolio/billboard/data/billboard_genius_df.csv")  
billboard_spotify <- read_csv("~/Desktop/portfolio/billboard/data/billboard_spotify_df.csv") 

billboard_append1 <- cbind(billboard, billboard_genius$`Candidate Title`) %>% 
  rename(genius_pair = 6) %>% 
  cbind(billboard_spotify$`Candidate Title`) %>% 
  rename(spotify_pair =7)

# Add the url for genius to get lyrics, and the song ID for spotify to get song features.

billboard_append2 <- merge(billboard_append1, genius, by="genius_pair") %>% 
  distinct() %>% 
  select(-8,-9,-11) %>% 
  rename(pair =pair.x)

billboard_append3 <- merge(billboard_append2, spotify, by="spotify_pair") %>% 
  rename( billboard_year = date.x,
          spotify_year = date.y) %>%
  select(-9,-10-14) 

#billboard date
billboard_append3$billboard_year <- substr(billboard_append3$billboard_year, 1,4) 
billboard_append3$billboard_year <- ymd(billboard_append3$billboard_year, truncated = 2L)
#spotify date
billboard_append3$spotify_year <- substr(billboard_append3$spotify_year, 1,4) 
billboard_append3$spotify_year <- ymd(billboard_append3$spotify_year, truncated = 2L)

final_df <- billboard_append3 %>% 
  group_by(spotify_pair, billboard_year, rank) %>% 
  arrange(spotify_year) %>% 
  slice(1L) %>% 
  select(artist.x,song.x,pair.x,genius_pair,spotify_pair,billboard_year,rank,url,artistid,songid) %>% 
  rename( artist = 1,
          song =2,
          billboard_pair = 3)

# Get lyrics 

#function to scrape lyrics 

get_lyrics <- function(url) {
  input <- paste0(url) 
  chart_page <- xml2::read_html(input)
  
  lyrics <- chart_page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'lyrics')]") %>% 
    rvest::html_text()%>% 
    as.data.frame() %>% 
    slice(1L) 
}

#for loop to scrape lyrics 

lyrics <- list()
urls <- final_df$url

for (i in 1:length(urls)) {
  lyrics[[i]] <-  get_lyrics(urls[[i]])
}


lyrics_df <- purrr::map(lyrics, as.data.table) %>% 
  rbindlist(fill=TRUE)  %>% 
  cbind(urls) %>% 
  rename(url = urls)

write_csv(lyrics_df, "lyrics_genius.csv")


# Get Features
Sys.setenv(SPOTIFY_CLIENT_ID = 'fill in')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'fill in')
access_token <- get_spotify_access_token()

features <- list()
ids <- final_df$songid

for (i in 1:length(ids)) {
  features[[i]] <- get_track_audio_features(ids[[i]])
}

features_df <- purrr::map(features, as.data.table) %>% 
  rbindlist(fill=TRUE) %>% 
  cbind(ids) %>% 
  select(1:11, 19) %>% 
  rename(songid = ids)

final_df2 <- merge(final_df, lyrics_df, by="url") %>% 
  distinct()

top100 <- merge(final_df2, features_df, by="songid") %>% 
  distinct()

write_csv(top100, "top100data.csv")
