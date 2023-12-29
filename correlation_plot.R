
artist_info_ranking <- readRDS("artist_info_ranking.rds")
top_tracks_us <- readRDS("top_tracks_us.rds")
top_tracks_gb <- readRDS("top_tracks_gb.rds")
audio_features_us <- readRDS("audio_features_us.rds")
audio_features_gb <- readRDS("audio_features_us.rds")

#Defining a popular artist as anyone with more than 50 in popularity
artist_info_ranking$is_popular <- ifelse(artist_info_ranking$popularity > 50, "Popular", "Not Popular")

#Defining the relative popularity
artist_info_ranking$relative_pop <- ifelse(artist_info_ranking$popularity <= mean(artist_info_ranking$popularity), "Not Popular", "Popular")

#subsetting the columns that are going to be needed
subset_artist <- artist_info_ranking %>% select(id, artist_spotify, popularity, relative_pop, genres)
subset_top_tracks <- top_tracks_us %>% select(artist_id, track_popularity, track_id, track_name, album.release_date)

merged_data <- merge(subset_artist, subset_top_tracks, by.x = "id", by.y = "artist_id")
#Arranging the data in order of Artist Popularity for plotting
merged_data <- merged_data %>%
  group_by(artist_spotify) %>%
  arrange(popularity, mean_track_popularity = mean(track_popularity))

#Creating the mean(track_popularity) data for plotting
mean_data <- merged_data %>%
  group_by(artist_spotify) %>%
  summarise(mean_track_popularity = mean(track_popularity))

# Merge mean data with the original data
merged_data <- merge(merged_data, mean_data, by = "artist_spotify")
#Creating the text for the interactive plot
merged_data$text <- paste0("Artist: ",artist_spotify,"\n Track: ", track_name ) 

#Factoring the artist_spotify for plotting in order of artist popularity
popularity_order <- artist_info_ranking %>% select(artist_spotify, popularity) %>% arrange(popularity)
merged_data$artist_spotify <- factor(merged_data$artist_spotify, levels = popularity_order$artist_spotify)
class(merged_data$artist_spotify)

# Create the plot
p2 <- ggplot(merged_data, aes(x = artist_spotify)) +
  geom_point(aes(y = track_popularity, color = "Track Popularity"), size = 1.5, alpha = 0.7) +
  geom_point(aes(y = popularity, color = "Artist Popularity"), size = 1.5, alpha = 0.7) +
  geom_point(aes(y = mean_track_popularity, color = "Mean Track Popularity by Artist"), size = 1.5, alpha = 0.7) +
  theme_minimal() +
  labs(y = "Popularity", x = "Artist") +
  ylim(0,100) +
  scale_color_manual(
    values = c("#E63946","#FBAF4F","#BFD3C1"),
    name = NULL
  )+
  theme(axis.text.x = element_text(angle = 90, size = 6, hjust = 1, vjust = 0.5))
  
p2
ggplotly(p2)






p4 <- ggplot(merged_data, aes(y = artist_spotify, x = track_popularity)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Top Tracks Popularity by Artist",
       y = "Artist",
       x = "Track Popularity") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16))

# Display the plot
print(p4)
















