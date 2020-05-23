# Author: Myrtille Genot
# Code : Webscrape : Billboard, Genius & Spotify 
# Date: 04/14/2020


#load data sets

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



# Section 1 : BILLBOARD WEBSCRAPE 




# 1. From Billboard Website :  https://www.billboard.com/charts/year-end/2018/hot-100-songs

#Function to read html 
#just date input, but could make more specific if I wanted to.
get_top100 <- function(date) {
  #url
  input <- paste0("https://www.billboard.com/charts/year-end/",date,"/hot-100-songs") 
  chart_page <- xml2::read_html(input)
  #get rank of song
  rank <- chart_page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'ye-chart-item__rank')]") %>% 
    rvest::html_text() 
  #get artist
  artist <- chart_page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'ye-chart-item__artist')]") %>% 
    rvest::html_text()
  #get song 
  song <- chart_page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//div[contains(@class, 'ye-chart-item__title')]") %>% 
    rvest::html_text()
  #get year date
  date <- chart_page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//span[contains(@class, 'dropdown__year-select-date')]") %>% 
    rvest::html_text()
  
  chart_df <- data.frame(date, rank, artist, song)
}


# 2. From alternative Website:  http://billboardtop100of.com

#Function to read html 
get_top100_1 <- function(date) {
  #initialise
  input <- paste0("http://billboardtop100of.com/",date,"-2/") 
  chart_page <- xml2::read_html(input) 
  #scrape table
  rawdata <- rvest::html_table(chart_page, header= FALSE)[[1]]
  #make table
  colnames(rawdata) <- c("rank","artist","song")
  songsperyear <- data.frame(rawdata, date)
}

# 3. Run for loops to extract data

#from Billboard Website : years 1958 to 2019


#empty list to hold data
hot_100 <- list()
years1 <- c(2013:2019)

#actual for loop
for (i in 1:length(years1)) {
  hot_100[[i]] <- get_top100(years1[[i]])
}

#data frame
billboard1<- purrr::map(hot_100, as.data.table) %>% 
  rbindlist(fill=TRUE)


# from alternative Billboard Website : years 1958 through to 2019.

top100 <- list()
years <- c(1958:2012)

#actual for loop
for (i in 1:length(years)) {
  top100[[i]] <- get_top100_1(years[[i]])
}

billboard2<- purrr::map(top100, as.data.table) %>% 
  rbindlist(fill=TRUE)

billboard <- rbind(billboard1,billboard2)
billboard$artist <- gsub("\n", "",billboard$artist)
billboard$song <- gsub("\n","",billboard$song)

# Scrape 2016 from alternative website - only 99 obvs from Billboard. 
lyrics2016 <- get_top100_1(2016)
lyrics2016$song <-  gsub("LYRICS", "", lyrics2016$song)

# Full Data Set with 2016 

#remove previous 2016 observations
billboard_full1 <- billboard %>% 
  filter(!date==2016)
# Combine 2016 to other dataset
billboard_full <- rbind(lyrics2016, billboard_full1)
#remove scraping \n
billboard_full$song <- gsub("\n", "", billboard_full$song )
billboard_full$artist <- gsub("\n", "", billboard_full$artist )
# remove % because it messes up my scrape
billboard_full$song <- gsub("%", "", billboard_full$song )

#check dataset is now complete 
datacompleteness <- billboard_full%>% 
  group_by(date) %>% 
  count()

#see what type of data I have
sapply(billboard_full, class)


# Save data set 
write_csv(billboard_full, "billboard_full.csv")






# Section 2 : GENIUS WEBSCRAPE 

# Scrape Lyrics for songs 
# Retrieve song URL

billboard_songartist <- billboard_full %>% 
  select(artist,song) %>% 
  distinct()

song_top <- billboard_songartist$song #5983
artist_top <- billboard_songartist$artist #5983


# Genius API info

username <-  "fill in"
password <- "fill in"
token <-"fill in"


# Webscrape : get url info for the artist and song, and some more info

get_url <- function(song) {
  base <- "https://api.genius.com/"
  endpoint <- "search"
  call1 <- paste(base, endpoint,"?","q","=",song, sep="")
  
  get_search <- httr::GET(call1, config = httr::add_headers(Authorization = paste("Bearer", token, sep = ' ' )))
  
  get_search_text <- httr::content(get_search, "text")
  get_search_json <- fromJSON(get_search_text, flatten=TRUE)
}

# Format song types properly
songs1 <- as.data.frame(song_top) %>% 
  rename(song = 1) 

songs1$song <- gsub(" ","%20", songs1$song)


song <- songs1$song
holder_url <- list()

for ( i in 1:length(song)) {
  holder_url[[i]] <- get_url(song[[i]])
}

#Do it with remainder
songs2 <- song[4277:5983]
holder_url2 <- list()

for ( i in 1:length(songs2)) {
  holder_url2[[i]] <- get_url(songs2[[i]])
}

#remainder AGAIN
songs3 <- songs2[420:1707]
holder_url3 <- list()

for ( i in 1:length(songs3)) {
  holder_url3[[i]] <- get_url(songs3[[i]])
}

#Put them all up together
total <- c(holder_url, holder_url2, holder_url3)

#Process to make into a dataframe and save.
holderurl<- purrr::map(total, as.data.table) %>% 
  rbindlist(fill=TRUE)
unlisted <- rbindlist(holderurl$response, fill = T, idcol = "meta") %>% 
  rename( artist = 29)
df <- apply(unlisted,2,as.character)


# GENIUS SONGS : Save dataset 
write.csv(df, "genius_fullsong.csv")


# Section 3 : SPOTIFY WEBSCRAPE 




# Use billboard data to pull out songs 
library("spotifyr")
# Connect to Spotify API 
Sys.setenv(SPOTIFY_CLIENT_ID = 'fill in')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'fill in')
access_token <- get_spotify_access_token()

# Make list to have the songs from Spotify 

ids <- list()

for (i in 1:length(song_top)) {
  ids[[i]] <- search_spotify(song_top[[i]], type = "track", limit=50, auth = get_spotify_access_token())
}

song_spotify <- purrr::map(ids, as.data.table) %>% 
  rbindlist(fill = TRUE) 

listartist <- purrr::map(song_spotify$artists, as.data.table)%>% 
  rbindlist(fill = TRUE)  

#get artist based on ID 
library("tibble")
hiddenartist <- list()

for (i in 1:nrow(song_spotify)) {
  hiddenartist[[i]] <- as.data.frame(song_spotify$artists[[i]]) %>% 
    add_column(song =song_spotify$name[[i]] ) %>% 
    add_column(songid = song_spotify$id[[i]]) %>% 
    add_column(year = song_spotify$album.release_date[[i]])
}
# data frame with all artists pulled from song_info. 
seeartist <- purrr::map(hiddenartist, as.data.table)%>% 
  rbindlist(fill = TRUE)  %>% 
  rename(artist = 3) 


write_csv(seeartist, "spotify_fullsongs.csv")