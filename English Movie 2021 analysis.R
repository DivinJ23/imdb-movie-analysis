#Load all packages
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)

# Load IMDb dataset
imdb_data <- read_csv("IMDB_processed_data.csv")

# Filter for English-language feature films from 2021
movie_df <- imdb_data %>%
  filter(year == 2021, language == "English", type == "movie") %>%
  select(title, year, genre, duration, rating, votes, metascore)

head(movie_df)

# Clean runtime columns
movie_df <- movie_df %>%
  mutate(
    duration = parse_number(duration),   # "120 min" → 120
    genre = str_trim(genre),
    rating = as.numeric(rating),
    votes = parse_number(votes),
    metascore = as.numeric(metascore)
  )

#Creating a new metric of IMDb rating per 1000 votes
movie_df <- movie_df %>%
  mutate(rating_per_1000votes = round(rating / (votes / 1000), 2))

#Graphical correlation between ratings and votes of 2021 English movies
ggplot(movie_df, aes(x = votes, y = rating)) +
  geom_point(alpha = 0.7, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "IMDb Ratings vs. Votes (2021 English Movies)",
       x = "Votes",
       y = "Rating")

#Illustrates average rating by all genres

movie_df_genres <- movie_df %>%
  separate_rows(genre, sep = ",") %>%
  mutate(genre = str_trim(genre))

movie_df_genres %>%
  group_by(genre) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, avg_rating), y = avg_rating)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Average IMDb Rating by Genre (Exploded)",
       x = "Genre",
       y = "Average Rating")



