library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)

# Load IMDb dataset
imdb_data <- read_csv("IMDB_processed_data.csv")

# Filter for movies released in 2021
movie_df <- imdb_data %>%
  filter(Release == 2021) %>%
  select(Title, Release, Runtime, Rated, Ratings, Rank)

# Clean columns
movie_df <- movie_df %>%
  mutate(
    Runtime = parse_number(Runtime),   # "120 min" → 120
    Ratings = as.numeric(Ratings)
  )

head(movie_df)

#Graphical correlation (Scatterplot) between ratings and runtimes of 2021 movies
ggplot(movie_df, aes(x = Runtime, y = Ratings)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "IMDb Ratings vs. Runtime (2021 Movies)",
       x = "Runtime (minutes)",
       y = "IMDb Rating")

#Bar chart: Average rating by age rating
movie_df %>%
  group_by(Rated) %>%
  summarise(avg_rating = mean(Ratings, na.rm = TRUE)) %>%
  ggplot(aes(x = Rated, y = avg_rating, fill = Rated)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average IMDb Rating by Age Rating (2021 Movies)",
       x = "Certification",
       y = "Average Rating")




