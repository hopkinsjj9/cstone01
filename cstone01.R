## https://github.com/hopkinsjj9/cstone01
## Setup

library(dslabs)
library(tidyverse)
library(caret)
library(devtools)
library(dplyr)
library(data.table)
library(gridExtra)
library(recosystem)
library(rrecsys)
library(slimrec)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
 
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                          title = as.character(title),
                                          genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# validation set will be 10% of MovieLens data
 
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# connect movieId to movie title
movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

rm(dl, ratings, movies, temp, test_index, removed, movielens)

## Review 

edx %>% summarize(
  n_users=n_distinct(userId),# unique users from edx
  n_movies=n_distinct(movieId),# unique movies from edx
  min_rating=min(rating),  # the lowest rating 
  max_rating=max(rating) # the highest rating
)

# matrix for top 5 movies
top5 <- edx %>% 
  count(movieId) %>% 
  top_n(5, n) %>% 
  .$movieId

top5.tbl <- edx %>% 
  filter(movieId %in% top5) %>% 
  filter(userId %in% c(13:20)) %>% 
  dplyr::select(userId, title, rating) %>% 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, ":.*")) %>%
  spread(title, rating)

top5.tbl[,1:4] %>% knitr::kable()

top5.tbl[,c(1,5:6)] %>% knitr::kable()

# matrix for a random sample of 100 movies and 100 users with yellow 
# indicating a user/movie combination for which we have a rating.

users <- sample(unique(edx$userId), 100)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  dplyr::select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  dplyr::select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

# plot count rating by movie
RM.pLot <- edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill='#164E80', size=1) + 
  scale_x_log10() + 
  labs(title="Ratings by Movie",x="Ratings", y = "Movie Counts")+
  theme( plot.title = element_text(hjust = 0.5))

# plot count rating by user
RU.pLot <- edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill='#E69F00', size=1) + 
  scale_x_log10() + 
  labs(title="Ratings by User",x="Ratings", y = "User Counts")+
  theme( plot.title = element_text(hjust = 0.5))

grid.arrange(RM.pLot, RU.pLot, ncol=2)

# to save space
rm(top5.tbl, top5, users, RM.pLot, RU.pLot)

## Simple Average 

# RMSE compute root mean square error (RMSE)

RMSE <- function(true_ratings, predicted_ratings){
sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu <- mean(edx$rating) # compute mean rating
mu

model_1_rmse <- RMSE(validation$rating, mu)
model_1_rmse

rsys_rmse_results <- tibble(method = "Just the average", 
RMSE = model_1_rmse)

## Movie Effect

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

ggplot(movie_avgs, aes(x=b_i)) + 
  geom_histogram(aes(y = ..density..), colour="black", fill="lightblue", size=1)+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Movie Averages",x="Ratings", y = "Count")+
  theme( plot.title = element_text(hjust = 0.5))

# create a results table with this and prior approaches
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse

rsys_rmse_results <- bind_rows(rsys_rmse_results,
    tibble(method="Movie Effect Model", RMSE = model_2_rmse))

## User Effect

user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

ggplot(user_avgs, aes(x=b_u)) + 
  geom_histogram(aes(y = ..density..), colour="black", fill="lightblue", size=1)+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="User Averages",x="Ratings", y = "Count")+
  theme( plot.title = element_text(hjust = 0.5))

predicted_ratings <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, edx$rating)
model_3_rmse

rsys_rmse_results <- bind_rows(rsys_rmse_results,
  tibble(method="Movie + User Effects Model",RMSE = model_3_rmse))

## Regularization

# top 10 best movies based on b_i
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  dplyr::select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# top 10 worse movies based on b_i
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  dplyr::select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

# add number of rating of the "best" obscure movies
edx %>% 
  count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  dplyr::select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

edx %>% 
  count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  dplyr::select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

# use cross-validation to pick a lambda:

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>% 
    group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n() + l))

  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))

  predicted_ratings <- 
    edx %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
  
  return(RMSE(predicted_ratings, edx$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

model_4_rmse <- min(rmses)
model_4_rmse

rsys_rmse_results <- bind_rows(rsys_rmse_results,
tibble(method="Regularized Movie + User Effect Model",  
RMSE = model_4_rmse))

## Recosystem

mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() + lambda))

# compute residuals on edx set

edx <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# compute residuals on validation set
validation <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# create data saved on disk in 3 columns with no headers
edx_data <- data_memory(user_index = edx$userId, 
  item_index = edx$movieId, 
  rating = edx$res, 
  index1 = T)

validation_data <- data_memory(user_index = validation$userId, 
  item_index = validation$movieId,
  index1 = T)

# create a model object
recommender <- Reco()

# This is a randomized algorithm
set.seed(1) 

# call the `$tune()` method to select best tuning parameters
res = recommender$tune(
  edx_data,
  opts = list(dim = c(10, 20, 30),
  costp_l1 = 0, costq_l1 = 0,
  lrate = c(0.05, 0.1, 0.2), nthread = 3)
)

# show best tuning parameters
print(res$min)

# Train the model by calling the `$train()` method
# some parameters coming from the result of `$tune()`
# This is a randomized algorithm
set.seed(1) 
suppressWarnings(recommender$train(edx_data, 
  opts = c(dim = res$min$dim, costp_l1 = res$min$costp_l1,
  costp_l2 = res$min$costp_l2, 
  costq_l1 = res$min$costq_l1,
  costq_l2 = res$min$costq_l2, 
  lrate = res$min$lrate,
  verbose = FALSE)))

# use the `$predict()` method to compute predicted values
# return predicted values in memory
predicted_ratings <- recommender$predict(validation_data, out_memory()) +
  mu + validation$b_i + validation$b_u 

# ceiling rating at 5
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

# floor rating at 0.50  
ind <- which(predicted_ratings < 0.5)
predicted_ratings[ind] <- 0.5

model_5_rmse <- RMSE(validation$rating, predicted_ratings)
model_5_rmse

rsys_rmse_results <- bind_rows(rsys_rmse_results,
   tibble(method="Recosystem - Regularized Movie and User",  
      RMSE = model_5_rmse))

## Compare Models

rsys_rmse_results %>% knitr::kable()
