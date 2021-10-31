# url = "http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip"
# download.file(url, destfile = "data.zip")
# dir.create("data")
# unzip("data.zip",exdir = "data")   
# 
# files = paste0("data/",list.files("data"))


library(ggplot2)
library(tidyr)
library(dplyr)

library(grid)
library(gridExtra)
library(png)

ratings = read.csv("ratings.csv")
books = read.csv("books.csv")

ratings$X = NULL
ratings$book_id_csv = NULL
ratings$is_read = NULL
ratings$is_reviewed = NULL

# users = read.csv(files[3], sep = ";")

# rm(files, url)

library(dplyr)
library(tidyr)
glimpse(books)

# set.seed(1234)
# categories = c("Action and Adventure","Classic","Detective and Mystery","Fantasy")
# books$category = sample( categories, nrow(books), replace=TRUE, prob=c(0.25, 0.3, 0.25, 0.20))
# books$category = as.factor(books$category)

# rm(categories)
names(books)
books$book_id = paste0("bid.",books$book_id)
# users$User.ID = paste0("User.",users$User.ID)
ratings$book_id = paste0("bid.",ratings$book_id)
ratings$user_id = paste0("user.",ratings$user_id)


library(ggplot2)

ratings = ratings[ratings$rating!= 0, ]

ratings %>%
  group_by(rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)

ratings_sum = ratings %>%
  group_by(user_id) %>%
  count() 

summary(ratings_sum$n)

user_index = ratings_sum$user_id[ratings_sum$n>4]

# users = users[users$User.ID %in% user_index, ]
ratings = ratings[ratings$user_id %in% user_index, ]
books = books[books$book_id %in% ratings$book_id,]

rm(ratings_sum, user_index)

user_item = ratings %>%
  top_n(10000) %>%
  pivot_wider(names_from = book_id,values_from = rating) %>%
  as.data.frame()

# user_item$X = NULL
# user_item$book_id_csv = NULL
# user_item$is_read = NULL
# user_item$is_reviewed = NULL

row.names(user_item) = user_item$user_id

user_item$user_id = NULL

user_item = as.matrix(user_item)

cos_similarity = function(A,B){
  num = sum(A *B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T)) * sqrt(sum(B^2, na.rm = T)) 
  result = num/den
  
  return(result)
}

item_recommendation = function(bid, rating_matrix = user_item, n_recommendations = 6){
  
  book_index = which(colnames(rating_matrix) == bid)
  
  similarity = apply(rating_matrix, 2, FUN = function(y) 
    cos_similarity(rating_matrix[,book_index], y))
  
  recommendations = tibble(book_id = names(similarity), 
                           similarity = similarity) %>%
    # filter(book_id != bid) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) 
  
  # print(dim(recommendations))
  return(recommendations)
  
}

dim(books)
# dim(users)
dim(ratings)





# recom_cf_item[order(recom_cf_item$similarity),]

i = 0
for (bid in colnames(user_item)) {
  recom_cf_item = item_recommendation(bid)
  if(recom_cf_item$similarity[5] > 0) {
    # print(i)
    # print(bid)
    # print(recom_cf_item$similarity)
    recom_cf_item = recom_cf_item %>%
      left_join(books, by = c("book_id" = "book_id")) 
    
    count = check_visuals(recom_cf_item[!is.na(recom_cf_item$title),],
                          "image_url")
    print(c(bid, i, count))
    if(count >= 5) {
      print("FOUND!!!")
    }
  }
  i = i+1
  if (i > 500) break
}
books[books$book_id == "bid.9992508",]
fiveimages = c("bid.99746", "bid.9977846", "bid.9997650", "bid.9992508")
my_book = "bid.99746"
recom_cf_item = item_recommendation(my_book)
recom_cf_item
recom_cf_item = recom_cf_item %>%
  left_join(books, by = c("book_id" = "book_id")) 

visualizar_recomendacion(recom_cf_item[!is.na(recom_cf_item$title),],
                         "image_url"
)

download_size <- function(url) as.numeric(httr::HEAD(url)$headers$`content-length`)

#for presentation purposes need to find cases where all images exist
check_visuals = function(recomendation, image, n_books = 6){
  
  if(n_books > nrow(recomendation)) {n_books = nrow(recomendation)}
  
  plot = list()
  count = 0
  # dir.create("content_recommended_images")
  for(i in 1:n_books){
    # Create dir & Download the images
    img = pull(recomendation[i,which(colnames(recomendation) == image)])
    # print("SIZE: ") 
    # print(download_size(img))
    if(download_size(img) > 1000) {
      count = count + 1
    }
  }
  
  return(count)
}


visualizar_recomendacion = function(recomendation, image, n_books = 5){
  
  if(n_books > nrow(recomendation)) {n_books = nrow(recomendation)}
  
  plot = list()
  
  # Create dir & Download the images
  # dir.create("content_recommended_images")
  
  for(i in 1:(n_books+2)) {
    print(c("I", i))
    if (i == 2) {
      ftype = "png"
      name = "content_recommended_images/arrow.png"
    } else {
      if(i == 1) {
        idx = 1
      } else {
        idx = i - 1
      }
      img = pull(recomendation[idx,which(colnames(recomendation) == image)])
      ftype = substr(img, nchar(img) - 2, nchar(img)) # can be png or jpg
      # print(ftype)
      name = paste0("content_recommended_images/",idx, ftype)
      suppressMessages(
        download.file(as.character(img), destfile = name ,mode = "wb")
      )
    }
    
    # Assign Object
    print(c(i, name))
    plot[[i]] = rasterGrob(if (ftype == "png") readPNG(name) else readJPEG(name))
  }
  
  do.call(marrangeGrob, args = list(plot, ncol = n_books+2, nrow = 1, top=""))
  
}



visualizar_recomendacion(recom_cf_item[!is.na(recom_cf_item$title),],
                         "image_url"
)



