library(ggplot2)
library(ggpubr) #for stat_cor
library(corrplot) #for correlation table
library(plotly) #making the ggplot interactive
library(viridis) #scale color viridis
library(hrbrthemes)
library(dplyr)

artist_info_ranking <- readRDS("artist_info_ranking.rds")
top_tracks_us <- readRDS("top_tracks_us.rds")
top_tracks_gb <- readRDS("top_tracks_gb.rds")
audio_features_us <- readRDS("audio_features_us.rds")
audio_features_gb <- readRDS("audio_features_us.rds")

#Defining a popular artist as anyone with more than 50 in popularity
artist_info_ranking$is_popular <- ifelse(artist_info_ranking$popularity > 50, "Popular", "Not Popular")

artist_info_ranking$inverted_rank <- (artist_info_ranking$ranking - 101) * -1

artist_info_ranking <- artist_info_ranking %>%
  mutate(ranking_group = cut(ranking, breaks = c(seq(0,100,10)), labels = c("1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")))

artist_info_ranking <- artist_info_ranking %>%
  mutate(ranking_group = cut(ranking, breaks = c(seq(0,100,25)), labels = c("1-25", "26-50", "51-75", "76-100")))

arranging_data <- artist_info_ranking %>%
  mutate(followers_k = round(followers.total/1000),2) %>%
  mutate(ranking_group = cut(ranking, breaks = c(seq(0,100,25)), labels = c("1-25", "26-50", "51-75", "76-100"))) %>%
  # Reorder artists by popularity
  arrange(desc(followers_k)) %>%
  mutate(artist_spotify = factor(artist_spotify, artist_spotify)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Artist: ", artist_spotify, "\nPopularity on Spotify: ", popularity, "\n1,000 Followers: ", followers_k, "\nRanking Rolling Stones: ", ranking, sep=""))


#Interactive plot with ranking breaks as colors
p_with_ranking <-arranging_data %>%
  # Classic ggplot
  ggplot( aes(x=ranking, y=popularity, size = followers_k, color = ranking_group, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Thousand Followers", guide = "legend") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none") +
  ylim(0,100) +
  xlim(0,100)
p_with_ranking
pp_with_ranking <- ggplotly(pp_with_ranking, tooltip="text")
pp

#Non interactive plot with ranking and size for followers
p_non_interactive_wsf <- arranging_data %>%
  ggplot(aes(x = ranking, y = popularity, size = followers_k, fill = ranking_group)) +
  geom_point(alpha = 0.5, shape = 21, color = "black") +
  scale_size(range = c(.1, 24), name = "Followers count (thousand)", guide = "legend") +
  scale_fill_viridis(discrete = TRUE, guide = guide_legend(override.aes = list(color = NA))) +
  #geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red") +
  theme_ipsum() +
  labs(fill = "Rolling Stones Ranking") +
  ylim(0,100) +
  xlim(0,100)

p_non_interactive_wsf
pp2_wsf <- ggplotly(p_non_interactive_wsf, tooltip = "text")
pp2_wsf  



#Plot showing only popularity with Linear Regression
assignment_plot <- arranging_data %>%
  ggplot() +
  geom_point(aes(x = ranking, y = popularity, color = is_popular, text = text),size = 5, alpha = 0.5) +
  geom_line(stat = "smooth", method = lm, aes(x = ranking, y = popularity), color = "red", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Spotify Popularity Vs Rolling Stones Ranking",
        color = NULL,
       x = "Rolling Stones Ranking",
       y = "Spotify Popularity") +
  ylim(0,100) +
  xlim(0,100) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.1) +  # Add vertical line at x = 0
  geom_hline(yintercept = 0, color = "black", alpha = 0.1) +  # Add horizontal line at y = 0
  theme(legend.position = "bottom",
        panel.grid = element_line(color = alpha("gray", 0.2), linetype = "dashed"),# Adjust legend position
        axis.text = element_text(size = 10),  # Adjust axis text size
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16)) # Adjust axis title size




assignment_plot
pp_assignment_plot <- ggplotly(assignment_plot, tooltip = "text")
pp_assignment_plot


library(ggplot2)
library(plotly)

# Fit the linear regression model
lm_model <- lm(popularity ~ ranking, data = arranging_data)

# Extract the coefficients
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]

# Create a data frame for the regression line
regression_data <- data.frame(ranking = c(0, 100))
regression_data$popularity <- predict(lm_model, newdata = regression_data)

# Create the ggplot object
assignment_plot <- arranging_data %>%
  ggplot() +
  geom_point(aes(x = ranking, y = popularity, color = is_popular, text = text), size = 5, alpha = 0.5) +
  geom_line(data = regression_data, aes(x = ranking, y = popularity), color = "red", linetype = "solid", alpha = 0.3) +
  theme_minimal() +
  labs(title = "Spotify Popularity Vs Rolling Stones Ranking",
       color = NULL,
       x = "Rolling Stones Ranking",
       y = "Spotify Popularity") +
  ylim(0, 100) +
  xlim(0, 100) +
  geom_vline(xintercept = 0, color = "black", alpha = 0.1) +  # Add vertical line at x = 0
  geom_hline(yintercept = 0, color = "black", alpha = 0.1) +  # Add horizontal line at y = 0
  theme(legend.position = "bottom",
        panel.grid = element_line(color = alpha("gray", 0.2), linetype = "dashed"),  # Adjust panel grid
        axis.text = element_text(size = 10),  # Adjust axis text size
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16))

# Convert ggplot to plotly
pp <- ggplotly(assignment_plot, tooltip = "text")

pp



# Convert ggplot to plotly
pp <- ggplotly(assignment_plot, tooltip = "text")

pp










average_popularity_by_ranking <- artist_info_ranking %>%
  group_by(ranking_group) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE),
            avg_followers = mean(followers.total, na.rm = TRUE))

ggplot(average_popularity_by_ranking, aes(x = ranking_group, y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Popularity by Ranking Group",
       x = "Ranking Group (Bins of 25)",
       y = "Average Popularity") +
  theme_minimal()

ggplot(average_popularity_by_ranking, aes(x = ranking_group, y = avg_popularity, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +  # Add points for emphasis
  labs(title = "Average Popularity by Ranking Group",
       x = "Ranking Group (Bins of 25)",
       y = "Average Popularity") +
  theme_minimal()

ggplot(average_popularity_by_ranking, aes(x = ranking_group, y = avg_popularity, group = 1)) +
  geom_line(color = "blue", size = 1, linejoin = "bevel") +
  geom_point(color = "blue", size = 3) +  # Add points for emphasis
  #geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +  # Add a loess smoothed line
  labs(title = "Average Popularity by Ranking Group",
       x = "Ranking Group (Bins of 25)",
       y = "Average Popularity") +
  theme_minimal()

ggplot(average_popularity_by_ranking, aes(x = ranking_group, y = avg_popularity)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +  # Add points for emphasis
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +  # Add a loess smoothed line
  labs(title = "Average Popularity by Ranking Group",
       x = "Ranking Group (Bins of 25)",
       y = "Average Popularity") +
  theme_minimal()

ggplot(average_popularity_by_ranking, aes(x = ranking_group, y = avg_popularity, size = avg_followers)) +
  geom_point(aes(color = avg_followers), alpha = 0.7) +
  scale_size_continuous(range = c(3, 15)) +  # Adjust the size range of bubbles
  #scale_color_viridis_c() +  # Adjust the color scale
  labs(title = "Bubble Plot of Average Popularity by Ranking Group",
       x = "Ranking Group",
       y = "Average Popularity",
       size = "Average Followers") +
  theme_minimal()


artist_info_ranking$normalized_popularity <- scale(artist_info_ranking$popularity,
                                  center = min(artist_info_ranking$popularity),
                                  scale = max(artist_info_ranking$popularity) - min(artist_info_ranking$popularity))

ggplot(artist_info_ranking, aes(x = inverted_rank, y = popularity)) +
  geom_bar() +
  labs(title = "Inverted Ranking vs. Normalized Popularity",
       x = "Inverted Ranking",
       y = "Normalized Popularity") +
  theme_minimal()

ggplot(artist_info_ranking, aes(x = popularity, y = inverted_ranking)) +
  geom_barh(stat = "identity") +
  labs(title = "Ranking vs. Popularity",
       x = "Popularity",
       y = "Ranking") +
  theme_minimal()



ggplot(artist_info_ranking, aes(x = popularity , y = inverted_rank, label = artist_rs)) +
  geom_point() +
  #geom_text() +  # Add text labels without overlapping
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add 45-degree line
  labs(title = "Ranking vs Popularity",
       x = "Popularity Spotify",
       y = "Rolling Stones Rank") +
  theme_minimal()+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +
  geom_smooth(method = "lm", se = FALSE, color = "red")  # Add linear regression line


ggplot(artist_info_ranking, aes(x = inverted_rank, y = log(followers.total), label = artist_rs)) +
  geom_point() +
  #geom_text() +  # Add text labels without overlapping
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add 45-degree line
  labs(title = "Ranking vs Popularity",
       x = "Rolling Stones Rank",
       y = "Spotify Followers") +
  theme_minimal()+
  xlim(0, 100) +  # Set x-axis limits
  #ylim(0, 100) +
  geom_smooth(method = "lm", se = FALSE, color = "red")


plot1 <- ggplot(artist_info_ranking, aes(x = inverted_rank, y = popularity, group = genres)) +
  geom_point(aes(color = genres)) +
  labs(title = "Inverted Ranking vs. Popularity",
       x = "Inverted Ranking",
       y = "Popularity") +
  theme_minimal()
plot1

plot2 <- ggplot(artist_info_ranking, aes(x = "", y = popularity)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Popularity Distribution",
       x = "Genres",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot2 <- ggplot(artist_info_ranking, aes(x = "", y = popularity)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Popularity Distribution",
       x = "Genres",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +  # Remove grid lines
  geom_text(aes(label = popularity), vjust = -0.5)  # Add labels closer to the graph

print(plot2)

plot2 <- ggplot(artist_info_ranking, aes(x = "", y = popularity)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.7) +
  geom_text(aes(y = min(popularity), label = paste("Min:", min(popularity))),
            color = "black", size = 3, hjust = 1.2, nudge_x = 0) +
  geom_text(aes(y = max(popularity), label = paste("Max:", max(popularity))),
            color = "black", size = 3, hjust = 1.2) +
  geom_text(aes(y = median(popularity), label = paste("Median:", median(popularity))),
            color = "black", size = 3, hjust = 1.2) +
  geom_text(aes(y = quantile(popularity, 0.25), label = paste("Q1:", quantile(popularity, 0.25))),
            color = "black", size = 3, hjust = 1.2) +
  geom_text(aes(y = quantile(popularity, 0.75), label = paste("Q3:", quantile(popularity, 0.75))),
            color = "black", size = 3, hjust = 1.2) +
  labs(title = "Popularity Distribution",
       x = "Genres",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  ylim(0,100)

print(plot2)

plot2 <- ggplot(artist_info_ranking, aes(x = "", y = popularity)) +
  geom_line(stat = "summary", fun.y = "median", color = "blue", size = 1) +
  geom_point(stat = "summary", fun.y = "median", color = "blue", size = 3) +
  geom_line(stat = "summary", fun.y = "quantile", fun.args = list(probs = 0.25), color = "orange", size = 1, linetype = "dashed") +
  geom_point(stat = "summary", fun.y = "quantile", fun.args = list(probs = 0.25), color = "orange", size = 3) +
  geom_line(stat = "summary", fun.y = "quantile", fun.args = list(probs = 0.75), color = "orange", size = 1, linetype = "dashed") +
  geom_point(stat = "summary", fun.y = "quantile", fun.args = list(probs = 0.75), color = "orange", size = 3) +
  labs(title = "Popularity Distribution",
       x = "Genres",
       y = "Popularity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot2)


print(plot2)



print(plot2)


















library(tidyverse)


# Flatten the list of genres
flattened_genres <- artist_info_ranking %>%
  mutate(genres = map(genres, ~unlist(.x))) %>%
  unnest(genres)
new_df <- data.frame(
  genres = flattened_genres$genres,
  popularity = rep(artist_info_ranking$popularity, sapply(artist_info_ranking$genres, length)),
  artist_name = rep(artist_info_ranking$artist_rs, sapply(artist_info_ranking$genres, length))
)


new_df %>%  group_by(genres) %>% summarise(amount = count(), avg_pop = mean(popularity))

summary <- new_df %>%  
  group_by(genres) %>% 
  summarise(amount = n(), avg_pop = mean(popularity)) %>% filter(amount > 1)

new_df %>% 
  group_by(genres) %>% 
  count() %>%
  ggplot(aes(x = c(), y = n)) +
  geom_point() +
  labs(title = "Count of Genres",
       x = "Genres",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

ggplot(summary, aes(x = amount , y = reorder(genres, amount), fill = avg_pop)) +
  geom_col() +
  scale_fill_viridis_c() +  # Use a color scale
  labs(title = "Average Popularity by Genre",
       x = "Amount",
       y = "Genres") +
  theme_minimal()

###########################################################################

head(artist_info_ranking)



