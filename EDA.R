library(ggplot2)
library(tidyr)
library(dplyr)

library(grid)
library(gridExtra)
library(png)

ratings = read.csv("ratings.csv")
books = read.csv("books.csv")

dim(books)
dim(ratings)


# ratings = ratings[ratings$rating!= 0, ]

ratings %>%
  group_by(rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)
