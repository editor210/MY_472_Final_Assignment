library(httr)
library(rvest)
library(dplyr)
library(spotifyr)

url <- "https://chartmasters.org/spotify-most-popular-artists/"
html_content <- read_html(url)
tab <- html_table(html_content, fill = TRUE)
popular_artists <- tab[[1]]

colnames(popular_artists) <- c("rank",
                               "artist_img",
                               "artist_cm",
                               "popularity",
                               "weekly +/-",
                               "daily Streams")


popular_artists <- popular_artists %>% select(-artist_img, -"weekly +/-", - popularity)

#Getting the Spotify id for every artist
readRenviron("api.env")
client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
access_token <- get_spotify_access_token(client_id, client_secret)

spotify_id <- data.frame()
for (artist in popular_artists$artist_cm){
  q <- artist
  print(q)
  
  result <- search_spotify(
    q,
    type = "artist",
    market = NULL,
    limit = 1,
    offset = 0,
    include_external = NULL,
    authorization = access_token,
    include_meta_info = FALSE
  )
  
  new_row <- data.frame(result)
  new_row <- cbind(artist_cm = q, new_row)
  spotify_id <- rbind(spotify_id, new_row)
}

#Selecting the relevant information
spotify_id <- spotify_id %>% select(artist_cm, artist_spotify = name, artist_id = id, genres, popularity)

#Checking for different names
indices <- which(spotify_id$artist_spotify != spotify_id$artist_cm)

if (length(indices) > 0) {
  print(paste("Different name on indices:", indices))
  print(paste("1 -", spotify_id$artist_spotify[indices]))
  print(paste("2 -", spotify_id$artist_cm[indices]))
}
#The differences don't seem relevant

#Merging the two dataset
spotify_id_subset <- spotify_id %>% select(artist_cm, artist_spotify, artist_id, artist_popularity = popularity)

popular_artists <- merge(popular_artists, spotify_id_subset, by = "artist_cm")

#Rearranging the columns
popular_artists <- popular_artists2 %>% select(rank,
                                                artist_cm,
                                                artist_spotify,
                                                everything()) %>% arrange(rank)

saveRDS(popular_artists, "popular_artists_cm.rds")



popular_artists <- readRDS("popular_artists_cm.rds")

#Getting the top tracks of each artist in the us
#Setting the access_token
access_token <- get_spotify_access_token(client_id, client_secret)

pop_top_tracks_us <- data.frame()
for (i in 1:nrow(popular_artists)){
  id <- popular_artists$artist_id[i]
  
  result <- get_artist_top_tracks(
    id,
    market = "US",
    authorization = access_token,
    include_meta_info = FALSE
  )
  print(popular_artists$artist_spotify[i])
  
  #We assume that the main artist of the track is the one that we are looking
  result$artists <- id
  
  #Including the name of the artist for better readability
  result$artist_spotify <- popular_artists$artist_spotify[i]
  
  new_rows <- result
  pop_top_tracks_us <- rbind(pop_top_tracks_us, new_rows)
}

#selecting the relevant columns
pop_top_tracks_us <- pop_top_tracks_us %>% select(artist_spotify,
                                                  artist_id = artists,
                                                  track_id = id,
                                                  track_name = name,
                                                  track_popularity = popularity,
                                                  album.release_date,
                                                  album.release_date_precision,
                                                  )

#Getting audio features for all the top tracks US
pop_audio_features_us <- data.frame()
track_list <- c(NULL)
artist_id_list <- c(NULL)
artist_list <- c(NULL)

for (i in 1:nrow(pop_top_tracks_us)){
  
  track <- pop_top_tracks_us$track_id[i]
  track_list <- c(track_list, track)
  
  artist_id_list <- c(artist_id_list, pop_top_tracks_us$artist_id[i])
  
  artist_list <- c(artist_list, pop_top_tracks_us$artist_spotify[i])
  
  if (length(track_list) == 100 | i == nrow(pop_top_tracks_us)){
    result <- get_track_audio_features(track_list,
                                       authorization = access_token)
    
    new_rows <- cbind(result, artist_id = artist_id_list)
    new_rows <- cbind(new_rows, artist_spotify = artist_list)
    #new_rows <- cbind(new_rows, track_id = track_list)
    
    pop_audio_features_us <- rbind(pop_audio_features_us, new_rows)
    #restarting the track list
    track_list <- c(NULL)
    artist_id_list <- c(NULL)
    artist_list <- c(NULL)
  }
}

colnames(pop_audio_features_us)[colnames(pop_audio_features_us) == "id"] <- "track_id"
pop_audio_features_us_subset <- pop_audio_features_us %>% select (-artist_spotify)

#merging the top tracks and the audio features
pop_audio_features_us <- merge(pop_top_tracks_us,
                               pop_audio_features_us_subset,
                               by.x = c("track_id", "artist_id"),
                               by.y = c("track_id", "artist_id"))


saveRDS(pop_audio_features_us, "popular_artists_audio_features.rds")
popular_artists_audio_features <- readRDS("popular_artists_audio_features.rds")
