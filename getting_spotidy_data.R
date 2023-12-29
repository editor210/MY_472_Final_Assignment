library(spotifyr)
library("jsonlite")
library(dplyr)

readRenviron("api.env")
client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
access_token <- get_spotify_access_token(client_id, client_secret)

rs_ranking <- read.csv("rs_ranking.csv")

artist_id_pop <- data.frame()
for (artist in rs_ranking$artist){
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
  new_row <- cbind(artist_rs = q, new_row)
  artist_id_pop <- rbind(artist_id_pop, new_row)
}

artist_id_pop

saveRDS(artist_id_pop, file = "artist_id_pop_spotify.rds")
artist_id_pop <- readRDS("artist_id_pop_spotify.rds")

#Checking for different names
for (i in 1:nrow(artist_id_pop)){
  if (artist_id_pop$name[i] != artist_id_pop$artist_rs[i]){
  print(paste("different name on index", i))
  print(paste("1 -",artist_id_pop$name[i]))
  print(paste("2 -",artist_id_pop$artist_rs[i]))
  }
}

#Differences are minor or non relevant (eg. &, and; upper and lower case differences)
#Only notable differences are:
# - Santana and Carlos Santana -> Sticking to the id provided by spotify that is the verified artist
# - Hank Williams brought the Hank Williams Jr singer, so it must be replaced by Hank Williams with the artist ID of '1FClsNYBUoNFtGgzeG74dW'
# - Check [1] "different name on line 43"
# [1] "1 - George Clinton & Parliament Funkadelic"
# [1] "2 - Parliament and Funkadelic"

#correcting the wrong artists
#Getting the ids of the artists that are wrong
#Parliament and Funkadelic
par_fun_wrong_id <- artist_id_pop %>% filter(artist_rs == "Parliament and Funkadelic") %>% pull(id)
#Hank Williams
hank_wil_wrong_id <- artist_id_pop %>% filter(artist_rs == "Hank Williams") %>% pull(id)

result <- search_spotify(
  "Parliament Funkadelic",
  type = "artist",
  market = NULL,
  limit = 20,
  offset = 0,
  include_external = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

#Selecting the lines of the band Parliament and the band Funkadelic
result_par <- subset(result, id == "5SMVzTJyKFJ7TUb46DglcH")
new_row_p <- cbind(artist_rs = "Parliament and Funkadelic", result_par)
result_fun <- subset(result, id == "450o9jw6AtiQlQkHCdH6Ru")
new_row_f <- cbind(artist_rs = "Parliament and Funkadelic", result_fun)
#Removing the line with the wrong result
artist_id_pop <- subset(artist_id_pop, id != par_fun_wrong_id)
#Adding the new lines
new_row <- rbind(new_row_p, new_row_f)
artist_id_pop <- rbind(artist_id_pop, new_row)

#getting Hank Williams info
result <- search_spotify(
  "Hank Williams",
  type = "artist",
  market = NULL,
  limit = 20,
  offset = 0,
  include_external = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

#Getting the row with the correct result
result_hank_wil <- subset(result, id == "1FClsNYBUoNFtGgzeG74dW")
new_row <- cbind(artist_rs = "Hank Williams", result_hank_wil)
#Removing the row that was wrong
artist_id_pop <- subset(artist_id_pop, id != hank_wil_wrong_id)
artist_id_pop <- rbind(artist_id_pop, new_row)

#Saving the dataset as a RDS file
saveRDS(artist_id_pop, file = "artist_id_pop.rds")
artist_id_pop <- readRDS("artist_id_pop.rds")



#merging the two dfs with the rankings and the spotify data
artist_info_ranking <- merge(rs_ranking, artist_id_pop,
                             by.x = "artist", by.y = "artist_rs",
                             all.x = TRUE)

#Changing the names of columns
artist_info_ranking <- artist_info_ranking %>%  rename(artist_rs = artist,
                                                       artist_spotify = name)

# Reorder columns
artist_info_ranking <- artist_info_ranking %>%
  select(ranking, artist_rs, artist_spotify, id, everything())

artist_info_ranking <- artist_info_ranking[order(artist_info_ranking$ranking), ]

#Correcting the name on the artist_rs column for Parliament and Funkadelic
artist_info_ranking <- artist_info_ranking %>%
  mutate(artist_rs = ifelse(artist_rs == "Parliament and Funkadelic",
                            paste0("Parliament and Funkadelic (", artist_spotify, ")"),
                            artist_rs))


saveRDS(artist_info_ranking, file = "artist_info_ranking.rds")
artist_info_ranking <- readRDS("artist_info_ranking.rds")





#Getting the top tracks of each artist in the us
#Setting the access_token
access_token <- get_spotify_access_token(client_id, client_secret)
top_tracks_us <- data.frame()
for (i in 1:nrow(artist_info_ranking)){
  id <- artist_info_ranking$id[i]
  result <- get_artist_top_tracks(
    id,
    market = "US",
    authorization = access_token,
    include_meta_info = FALSE
  )
  print(artist_info_ranking$artist_rs[i])

  #We assume that the main artist of the track is the one that we are looking
  result$artists <- id
  
  #Including the name of the artist for better readability
  result$artist_spotify <- artist_info_ranking$artist_spotify[i]

  new_rows <- result
  top_tracks_us <- rbind(top_tracks_us, new_rows)
}

top_tracks_gb <- data.frame()
for (i in 1:nrow(artist_info_ranking)){
  id <- artist_info_ranking$id[i]
  result <- get_artist_top_tracks(
    id,
    market = "GB",
    authorization = access_token,
    include_meta_info = FALSE
  )
  print(artist_info_ranking$artist_rs[i])
  
  #We assume that the main artist of the track is the one that we are looking
  result$artists <- id
  
  #Including the name of the artist for better readability
  result$artist_spotify <- artist_info_ranking$artist_spotify[i]
  
  new_rows <- result
  top_tracks_gb <- rbind(top_tracks_gb, new_rows)
}


#changing colnames
for (name in c("id", "name", "popularity", "preview_url")){
  colnames(top_tracks_gb)[colnames(top_tracks_gb) == name] <- paste0("track_",name)
  colnames(top_tracks_us)[colnames(top_tracks_us) == name] <- paste0("track_",name)
}

colnames(top_tracks_gb)[colnames(top_tracks_gb) == "artists"] <- "artist_id"
colnames(top_tracks_us)[colnames(top_tracks_us) == "artists"] <- "artist_id"

top_tracks_us <- top_tracks_us %>%
  select(artist_spotify, artist_id, track_name, track_popularity, track_id, everything())

top_tracks_gb <- top_tracks_gb %>%
  select(artist_spotify, artist_id, track_name, track_popularity, track_id, everything())


saveRDS(top_tracks_gb, file = "top_tracks_gb.rds")
saveRDS(top_tracks_us, file = "top_tracks_us.rds")

top_tracks_us <- readRDS("top_tracks_us.rds")
top_tracks_gb <- readRDS("top_tracks_gb.rds")

#######################################################
#It is not necessary to separate the audio features in two datasets
# The track id can be used to merge everything
#########################################################

access_token <- get_spotify_access_token(client_id, client_secret)
#Getting audio features for all the top tracks GB
audio_features_gb <- data.frame()
track_list <- c(NULL)
artist_id <- c(NULL)
artist_spotify <- c(NULL)
  for (i in 1:nrow(top_tracks_gb)){
    track <- top_tracks_gb$track_id[i]
    track_list <- c(track_list, track)
    artist_id <- c(artist_id, top_tracks_gb$artist_id[i])
    artist_spotify <- c(artist_spotify, top_tracks_gb$artist_spotify[i])
    
    if (length(track_list) == 100 | i == nrow(top_tracks_gb)){
      result <- get_track_audio_features(track_list,
                                         authorization = access_token)
      
      new_rows <- cbind(result, artist_id)
      new_rows <- cbind(new_rows, artist_spotify)
      new_rows <- cbind(track_list)
      
      audio_features_gb <- rbind(audio_features_gb, new_rows)
      #restarting the track list
      track_list <- c(NULL)
      artist_id <- c(NULL)
      artist_spotify <- c(NULL)
    }
  }

colnames(audio_features_gb)[colnames(audio_features_gb) == "id"] <- "track_id"
saveRDS(audio_features_gb, file = "audio_features_gb.rds")



readRenviron("api.env")
client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")

access_token <- get_spotify_access_token(client_id, client_secret)

#Getting audio features for all the top tracks US
audio_features_us <- data.frame()
track_list <- c(NULL)
artist_id <- c(NULL)
artist_spotify <- c(NULL)
for (i in 1:nrow(top_tracks_us)){
  track <- top_tracks_us$track_id[i]
  track_list <- c(track_list, track)
  artist_id <- c(artist_id, top_tracks_us$artist_id[i])
  artist_spotify <- c(artist_spotify, top_tracks_us$artist_spotify[i])
  
  if (length(track_list) == 100 | i == nrow(top_tracks_us)){
    result <- get_track_audio_features(track_list,
                                       authorization = access_token)
    
    new_rows <- cbind(result, artist_id)
    new_rows <- cbind(new_rows, artist_spotify)
    #new_rows <- cbind(new_rows, track_id = track_list)
    
    audio_features_us <- rbind(audio_features_us, new_rows)
    #restarting the track list
    track_list <- c(NULL)
    artist_id <- c(NULL)
    artist_spotify <- c(NULL)
  }
}

colnames(audio_features_us)[colnames(audio_features_us) == "id"] <- "track_id"


saveRDS(audio_features_us, file = "audio_features_us.rds")

audio_features_us <- readRDS("audio_features_us.rds")
audio_features_gb <- readRDS("audio_features_gb.rds")


#Next steps
# 1 - After I finish tidying the artist_id_pop data I add the id to the previous rs_ranking
# Create a relational SQL databse to get the information using the spotify id as a key
# 2 - Get top tracks for each artist
# 3 - Get audio features for every track
# 4 - Find similarities and graphs to show how the artists popularity and genres and audiofeatures relate
# 5 - Get the top 50 global artists in Spotify
# 6 - See how the top 50 audio features is related to the artists in the ranking
# 7 - Argue that the popularity is a controversial thing and can change from country to country
# 8 - The ranking is 100% anglophone (plot a map of the artists origin)
# 9 - Get the end date of each band or artist from the other website to see if the end date is related to the popularity
# 10 - Discuss the results



id_list <- c("")
artist_info <- data.frame()
access_token <- get_spotify_access_token()
for (i in 1:nrow(artist_id_pop)){
  id_list <- c(id_list, artist_id_pop$id[i])
  
  if (length(id_list) == 50 | i == nrow(artist_id_pop)){
    artist_new_info <- get_artists(
      id_list,
      authorization = access_token,
      include_meta_info = FALSE
    )
    artist_info <- rbind(artist_new_info, new_row)
  }
}

get_artists(
  ids,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)




















#Accessing the API without the spotifyR package
library(httr)

# Set the URL and headers
url <- "https://api.spotify.com/v1/artists/2x9SpqnPi8rlE9pjHBwmSC/top-tracks?market=US"
res <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))

class(res)

json_content <- content(res, "parsed")
data <- fromJSON(json_content)
