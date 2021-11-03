library(ggplot2)
library(tidyr)
library(dplyr)

library(grid)
library(gridExtra)
library(png)

library(readr)
library(purrr)
library(GGally)
library(scales)

ratings_full = read.csv("ratings_full.csv")

books = read.csv("books.csv")
books$X = NULL

book_genres = read.csv("book_genres.csv")

book_by_genres = book_genres %>% 
  mutate(genres_full = map(genres_full, ~ strsplit(.x, ", ") %>% unlist())) %>% 
  unnest(genres_full) 

book_by_genres_s = book_by_genres %>% 
  separate_rows(genres_full, sep = ",")

book_by_genres_s %>% 
  group_by(genres_full) %>% 
  summarise(rating = mean(average_rating)) %>% 
  ggplot(aes(reorder(genres_full, rating), rating)) + 
  geom_col(fill = "darkgreen") +
  theme_minimal() + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Publication Year vs Number of Reviewed Books") +
  labs(title='Ratings by Genre',
       x='Genre',
       y='Rating') 


book_by_genres_s %>% 
  group_by(genres_full) %>% 
  summarise(rating = sum(ratings_count)) %>% 
  ggplot(aes(reorder(genres_full, rating), rating)) + 
  geom_col(fill = "darkblue") +
  theme_minimal() + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Publication Year vs Number of Reviewed Books") +
  labs(title='Ratings Count by Genre',
       x='Genre',
       y='Rating Count') 


ratings_full %>%
  rename(
    Rating = rating 
  ) %>% 
  group_by(Rating) %>%
  summarize(Count = n()) %>%
  ggplot(aes(Rating, Count)) + 
  geom_col(fill = "darkgreen") +
  theme_minimal() + 
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Distribution of Ratings in Interactions Data Set") 


books %>%
  filter(!is.na(publication_year)) %>% 
  filter(publication_year > 1950) %>% 
  filter(publication_year < 2022) %>% 
  group_by(publication_year) %>%
  summarize(Count = n()) %>%
  ggplot(aes(publication_year, Count)) + 
  geom_col(fill = "darkgreen") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
  ggtitle("Publication Year vs Number of Reviewed Books")


books = books %>% 
  mutate(
    Rating = round(average_rating)
  ) %>% 
  filter(Rating > 0) %>% 
  mutate(
    Rating = as.factor(Rating)
  )


top_lang = books %>%
  filter(language_code != "") %>% 
  group_by(language_code) %>%
  summarize(Count = n()) %>%
  top_n(10) %>% 
  merge(books, on="language_code")

top_lang %>%
  group_by(language_code, Rating) %>%
  summarize(Count = n()) %>%
  mutate(Proportion = Count/sum(Count)) %>%
  # top_n(10) %>% 
  ggplot() + 
  geom_col(aes(x = language_code, y = Proportion, fill = Rating), position = "dodge") +
  labs(title='Top 10 Languages vs Ratings vs Proportion',
       x='Language',
       y='Proportion of Rounded Ratings') 


books %>%
  filter(!is.na(num_pages)) %>% 
  filter(num_pages <= 1400) %>% 
  mutate(pages_range = cut(num_pages, 
                           breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400), 
                           labels = c('0-200','200-400','400-600','600-800','800-1000','1000-1200','1200-1400'),
                           include.lowest = TRUE)) %>%
  group_by(pages_range) %>%
  summarize(Count = mean(average_rating)) %>%
  ggplot() + 
  geom_col(aes(x = pages_range, y = Count), fill = "darkgreen") +
  scale_fill_brewer(palette='Set1') +
  coord_flip() +
  labs(title='Rating by number of pages',
       x='Number of Pages',
       y='Rating')
  

books %>%
  filter(!is.na(num_pages)) %>% 
  filter(num_pages <= 1400) %>% 
  mutate(pages_range = cut(num_pages, 
                           breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400), 
                           labels = c('0-200','200-400','400-600','600-800','800-1000','1000-1200','1200-1400'),
                           include.lowest = TRUE)) %>%
  group_by(pages_range) %>%
  summarize(Count = sum(ratings_count)) %>%
  ggplot() + 
  geom_col(aes(x = pages_range, y = Count), fill = "darkblue") +
  scale_fill_brewer(palette='Set1') +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title='Rating Count by number of pages',
       x='Number of Pages',
       y='Rating Count')

na_pages = (books %>%
  filter(num_pages <= 1200) %>% 
  filter(!is.na(num_pages)))$num_pages %>%
  cut(breaks = c(0, 200, 400, 600, 800, 1000, 1200), include.lowest = TRUE) 

(books %>%
  filter(!is.na(num_pages)))$num_pages[is.na(na_pages)]

books %>% ggplot(aes(x=average_rating)) +
  geom_histogram(binwidth=.25, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(average_rating, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  scale_y_continuous(labels = scales::comma)

books %>% 
  ggplot(aes(x = ratings_count, y = average_rating)) +
  geom_point(alpha=.1, lwd=.1, col="brown") +
  ylab("Average Rating") +
  xlab("Number of Ratings") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^2,10^7)) 

book_top = books %>% 
  filter(!is.na(ratings_count)) %>% 
  top_n(ratings_count, 10)

