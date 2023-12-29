artist_info_ranking <- readRDS("artist_info_ranking.rds")
top_tracks_us <- readRDS("top_tracks_us.rds")
top_tracks_gb <- readRDS("top_tracks_gb.rds")
audio_features_us <- readRDS("audio_features_us.rds")
audio_features_gb <- readRDS("audio_features_gb.rds")

#Defining the relative popularity
artist_info_ranking$relative_pop <- ifelse(artist_info_ranking$popularity <= mean(artist_info_ranking$popularity), "Not Popular", "Popular")
artist_info_ranking$is_popular <- ifelse(artist_info_ranking$popularity > 50, "Popular", "Not Popular")

head(audio_features_us)
head(artist_info_ranking)
colnames(audio_features_us)
colnames(artist_info_ranking)
colnames(top_tracks_us)

#Getting the tracks popularity along with the audio features
subset_top_track <- top_tracks_us %>% select(artist_id, track_id, track_name, track_popularity)
merged_df <- merge(audio_features_us, subset_top_track, by = c("track_id", "artist_id"))

subset_artist_pop <- artist_info_ranking %>% select(artist_id = id, artist_popularity = popularity, relative_pop, is_popular)
merged_df <- merge(merged_df, subset_artist_pop, by = "artist_id")
