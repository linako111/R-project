url = "http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip"
download.file(url, destfile = "data.zip")
dir.create("data")
unzip("data.zip",exdir = "data")   

files = paste0("data/",list.files("data"))

ratings = read.csv(files[1], sep = ";")
books = read.csv(files[2], sep = ";")
users = read.csv(files[3], sep = ";")

rm(files, url)

library(dplyr)
glimpse(books)

set.seed(1234)
categories = c("Action and Adventure","Classic","Detective and Mystery","Fantasy")
books$category = sample( categories, nrow(books), replace=TRUE, prob=c(0.25, 0.3, 0.25, 0.20))
books$category = as.factor(books$category)

rm(categories)

books$ISBN = paste0("Isbn.",books$ISBN)
users$User.ID = paste0("User.",users$User.ID)
ratings$ISBN = paste0("Isbn.",ratings$ISBN)
ratings$User.ID = paste0("User.",ratings$User.ID)


library(ggplot2)

ratings = ratings[ratings$Book.Rating!= 0, ]

ratings %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)

ratings_sum = ratings %>%
  group_by(User.ID) %>%
  count() 

summary(ratings_sum$n)

user_index = ratings_sum$User.ID[ratings_sum$n>4]

users = users[users$User.ID %in% user_index, ]
ratings = ratings[ratings$User.ID %in% user_index, ]
books = books[books$ISBN %in% ratings$ISBN,]

rm(ratings_sum, user_index)

user_item = ratings %>%
  top_n(10000) %>%
  pivot_wider(names_from = ISBN,values_from = Book.Rating) %>%
  as.data.frame()

row.names(user_item) = user_item$User.ID

user_item$User.ID = NULL

user_item = as.matrix(user_item)

cos_similarity = function(A,B){
  num = sum(A *B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T)) * sqrt(sum(B^2, na.rm = T)) 
  result = num/den
  
  return(result)
}

item_recommendation = function(book_id, rating_matrix = user_item, n_recommendations = 5){
  
  book_index = which(colnames(rating_matrix) == book_id)
  
  similarity = apply(rating_matrix, 2, FUN = function(y) 
    cos_similarity(rating_matrix[,book_index], y))
  
  recommendations = tibble(ISBN = names(similarity), 
                           similarity = similarity) %>%
    filter(ISBN != book_id) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) 
  
  return(recommendations)
  
}

dim(books)
dim(users)
dim(ratings)

recom_cf_item = item_recommendation("Isbn.0446677450")
recom_cf_item

recom_cf_item[order(recom_cf_item$similarity),]


recom_cf_item = recom_cf_item %>%
  left_join(books, by = c("ISBN" = "ISBN")) 

library(ggplot2)
library(tidyr)
library(dplyr)

library(grid)
library(gridExtra)
library(jpeg)

visualizar_recomendacion = function(recomendation,
                                    recommended_book, image, n_books = 5){
  
  if(n_books > nrow(recomendation)) {n_books = nrow(recomendation)}
  
  plot = list()
  
  # dir.create("content_recommended_images")
  for(i in 1:n_books){
    # Create dir & Download the images
    img = pull(recomendation[i,which(colnames(recomendation) == image)])
    name = paste0("content_recommended_images/",i,".jpg")
    suppressMessages(
      download.file(as.character(img), destfile = name ,mode = "wb") 
    )
    
    # Assign Objetc
    plot[[i]] = rasterGrob(readJPEG(name))
  }
  
  do.call(marrangeGrob, args = list(plot, ncol = n_books, nrow = 1, top=""))
  
}

visualizar_recomendacion(recom_cf_item[!is.na(recom_cf_item$Book.Title),],
                         "ISBN",
                         "Image.URL.M"
)
