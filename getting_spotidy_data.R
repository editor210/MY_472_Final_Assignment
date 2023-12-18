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

saveRDS(artist_id_pop, file = "artist_id_pop.rds")
artist_id_pop <- readRDS("artist_id_pop.rds")

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
#Getting the names of the artists that are wrong
#Parliament and Funkadelic
par_fun <- artist_id_pop$artist[43]
#Hank Williams
hank_wil <- artist_id_pop$artist[27]

result <- search_spotify(
  par_fun,
  type = "artist",
  market = NULL,
  limit = 20,
  offset = 0,
  include_external = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)

#Selecting the lines of the band Parliament and the band Funkadelic
result_par_fun <- subset(result, name %in% c("Parliament", "Funkadelic"))
#Removing the line with the wrong result
artist_id_pop <- subset(artist_id_pop, artist_rs != par_fun)
#Adding the new lines
new_row <- cbind(artist_rs = par_fun, result_par_fun)
artist_id_pop <- rbind(artist_id_pop, new_row)

#getting Hank Williams info
result <- search_spotify(
  hank_wil,
  type = "artist",
  market = NULL,
  limit = 20,
  offset = 0,
  include_external = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)


result_hank_wil <- subset(result, id == "1FClsNYBUoNFtGgzeG74dW")

new_row <- data.frame(artist = hank_wil,
                       spotify_name = result_hank_wil$name,
                       id = result_hank_wil$id,
                       pop = result_hank_wil$popularity,
                       followers = result_hank_wil$followers.total)

artist_id_pop <- subset(artist_id_pop, artist != hank_wil)
artist_id_pop <- rbind(artist_id_pop, new_row)

#After retrieving the ids of the artists of the ranking, getting each artist audio features


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



#Next steps
# 1 - After I finish tidying the artist_id_pop data I add the id to the previous rs_ranking
# Create a relational SQL databse to get the information using the spotify id as a key
# 2 - Get top tracks for each artist
# 3 - Get audio features for every artist
# 4 - Find similarities and graphs to show how the artists popularity and genres and audiofeatures relate
# 5 - Get the top 50 global artists in Spotify
# 6 - See how the top 50 audio features is related to the artists in the ranking
# 7 - Argue that the popularity is a controversial thing and can change from country to country
# 8 - The ranking is 100% anglophone (plot a map of the artists origin)
# 9 - Get the end date of each band or artist from the other website to see if the end date is related to the popularity
# 10 - Discuss the results






















#Accessing the API without the spotifyR package
library(httr)

# Set the URL and headers
url <- "https://api.spotify.com/v1/artists/3WrFJ7ztbogyGnTHbHJFl2"
res <- GET(url, add_headers(Authorization = paste("Bearer", access_token)))

class(res)

json_content <- content(res, "text")
data <- fromJSON(json_content)
