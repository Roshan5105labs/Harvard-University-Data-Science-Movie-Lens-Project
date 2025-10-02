#author: S.Roshan Pranao

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

#### Data Ingestion

# Downloading necessary libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Loading the necessary libraries

library(tidyverse)
library(caret)
library(ggplot2)
library(lubridate)
library(recosystem)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data

set.seed(1) 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set

final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set

removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################################
#### Data Cleaning 

# Analysing the structure of the datasets - edx and final_holdout_test

str(edx)
str(final_holdout_test)

# Converting time stamp to date and time

edx <- edx %>% mutate(date=as_datetime(timestamp))
final_holdout_test <- final_holdout_test %>% mutate(date=as_datetime(timestamp))

# check for any missing data in the edx and final_holdout_test data sets

sum(is.na(edx))
sum(is.na(final_holdout_test))


# Partitioning edx into train_data and test_data

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_data <- edx[-test_index,]
temporary <- edx[test_index,]

# Make sure userId and movieId in test_data are also in train_data

test_data <- temporary %>% 
  semi_join(train_data, by = "movieId") %>%
  semi_join(train_data, by = "userId")

# Add rows removed from test set back into train set

removed <- anti_join(temporary, test_data)
train_data <- rbind(train_data, removed)

rm(test_index, temporary, removed)



################################################################################
#### Exploratory data analysis


# edx part

head(edx)
summary(edx)
dim(edx)

# Unique users,movies,genres count

unique <- c(n_movies = n_distinct(edx$movieId),
    n_genres = n_distinct(edx$genres),
    n_users = n_distinct(edx$userId))

# final_holdout_test part

head(final_holdout_test)
summary(final_holdout_test)


# ratings count

r_count <- edx %>% group_by(rating) %>% 
  summarise(ratings_count=n()) %>%
  arrange(desc(ratings_count))

# Display the result

print(r_count)


# rating distribution 

edx %>%
  ggplot(aes(x = rating)) + 
  geom_histogram(binwidth = 0.5, fill = "#69b3a2", color = "black", alpha = 0.7) + 
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count") + 
  theme_minimal(base_size = 15) + 
  theme( plot.title = element_text(hjust = 0.5, face = "bold", color = "#333333"), axis.title = element_text(face = "bold", color = "#333333"), axis.text = element_text(color = "#333333") ) + 
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) + 
  scale_y_continuous(labels = scales::comma)


# Genre count

g_count <- edx %>% separate_rows(genres,sep="\\|") %>% 
  group_by(genres) %>% 
  summarise(rating_count=n()) %>% 
  arrange(desc(rating_count))
print(g_count)


# Create the bar plot for genre distribution

ggplot(g_count, aes(x = reorder(genres, -rating_count), y = rating_count, fill = genres)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Movie Genres", x = "Genres", y = "Count of Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Movie Distribution

edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies", 
          subtitle = "The distribution is almost symetric.") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_minimal()


# User Frequency

edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  head()


# User Distribution Graph

edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Users", 
          subtitle="The distribution is right skewed.") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()


# Rating Distribution per year

edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "white") + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal()


# Movie with maximum number of ratings

edx %>% group_by(movieId,title) %>% 
  summarise(ratings_count=n()) %>%
  arrange(desc(ratings_count))


## Machine Learning models training and evaluation 

# Defining the loss function calculation - RMSE,MAE,MSE

# Define Mean Absolute Error (MAE)

MAE <- function(true_ratings, predicted_ratings){
  mean(abs(true_ratings - predicted_ratings))
}


# Define Mean Squared Error (MSE)

MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings - predicted_ratings)^2)
}


# Define Root Mean Squared Error (RMSE)

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


### Predicting on a random basis

set.seed(1990)

# Create the probability of each rating

p <- function(x, y) mean(y == x)
rating <- seq(0.5 , 5 , 0.5)

# Using Monte Carlo simulation estimating the rating individually

B <- 10000
M <- replicate(B, {
  s <- sample(train_data$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))


# Predict random ratings

y_hat <- sample(rating, size = nrow(test_data), 
                       replace = TRUE, prob = prob)

 
# Create a tibble for  the error results

result <- tibble(Method = "Project Goal", RMSE = 0.86490, MSE = NA, MAE = NA)

result <- bind_rows(result, 
                    tibble(Method = "Random prediction", 
                           RMSE = RMSE(test_data$rating, y_hat),
                           MSE  = MSE(test_data$rating, y_hat),
                           MAE  = MAE(test_data$rating, y_hat)))

print(result)

##### Random Prediction is not yielding the RMSE less than 0.86490

### Linear Model

### Using the average/mean of the rating

mu <- mean(train_data$rating)

result <- bind_rows(result, 
                    tibble(Method = "Average/Mean Rating", 
                           RMSE = RMSE(test_data$rating, mu),
                           MSE  = MSE(test_data$rating, mu),
                           MAE  = MAE(test_data$rating, mu)))
print(result)

##### Mean Rating Model yields lesser than Random Prediction model of 1.06 but still lesser than the quoted RMSE

### Movie Effect

b_i <- train_data %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - mu))
head(b_i)

##### Movie Effect is distributed in a left skewed manner

# Visualization of Movie Effect Distribution

b_i %>% ggplot(aes(x = bi)) + 
  geom_histogram(bins = 10, col = I("black")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie effect") +
  ylab("Frequency") +
  scale_y_continuous(labels = scales :: comma) + 
  theme_minimal()

# Predict the rating 

y_hat_bi <- mu + test_data %>% 
  left_join(b_i, by = "movieId") %>% 
  pull(bi)

# Calculate the Loss Functions

result <- bind_rows(result, 
                    tibble(Method = "Movie Effect Model", 
                           RMSE = RMSE(test_data$rating, y_hat_bi),
                           MSE  = MSE(test_data$rating, y_hat_bi),
                           MAE  = MAE(test_data$rating, y_hat_bi)))
print(result)

##### Movie Effect Model performs better then the Random and mean rating model but still lies lesser than the quoted RMSE

### User Effect(b_u)

b_u <- train_data %>% 
  left_join(b_i, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

# Prediction

y_hat_bi_bu <- test_data %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(prediction = mu + bi + bu) %>%
  pull(prediction)

# Update the result tibble
result <- bind_rows(result, 
                    tibble(Method = "User Effect Model", 
                           RMSE = RMSE(test_data$rating, y_hat_bi_bu),
                           MSE  = MSE(test_data$rating, y_hat_bi_bu),
                           MAE  = MAE(test_data$rating, y_hat_bi_bu)))
print(result)

# visualization of user effect distribution
train_data %>% 
  group_by(userId) %>%
  summarize(bu = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(bu)) +
  geom_histogram(bins = 20, color='blue') +
  ggtitle("User Effect Distribution") +
  xlab("User Bias") +
  ylab("Count") 

##### This model is performing better than the rest of the models and lies close to the required RMSE

### Regularization

lambdas <- seq(0, 5 , 0.25)

rmse <- sapply(lambdas,function(l){
  # Mean
  mu <- mean(train_data$rating)
  
  # Movie effect (bi)
  bi <- train_data %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  # User effect (bu)  
  bu <- train_data %>% 
    left_join(bi, by="movieId") %>%
    filter(!is.na(bi)) %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l))
  
  # Prediction: mu + bi + bu  
  predicted_ratings <- test_data %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    filter(!is.na(bi), !is.na(bu)) %>%
    mutate(prediction = mu + bi + bu) %>%
    .$prediction
  
  return(RMSE(predicted_ratings, test_data$rating))
})

# Plot the Lambdas vs RMSE

tibble(Lambda = lambdas, RMSE = rmse) %>%
  ggplot(aes(x = Lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Regularization") +
  theme_minimal()

# Consider the lambda that returns the lowest RMSE value.

lambda <- lambdas[which.min(rmse)]

# calculate the predicted rating using the fittest parameters 
# achieved through regularization.  

mu <- mean(train_data$rating)

# Movie effect (bi)

bi <- train_data %>% 
  group_by(movieId) %>%
  summarize(bi = sum(rating - mu)/(n()+lambda))

# User effect (bu)

bu <- train_data %>% 
  left_join(bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bi - mu)/(n()+lambda))

# Prediction

predicted_reg <- test_data %>% 
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  mutate(prediction = mu + bi + bu) %>%
  pull(prediction)

# Update the result table

result <- bind_rows(result, 
                    tibble(Method = "Regularized movie and user effect model", 
                           RMSE = RMSE(test_data$rating, predicted_reg),
                           MSE  = MSE(test_data$rating, predicted_reg),
                           MAE  = MAE(test_data$rating,predicted_reg)))
print(result)

#### This model gives the least RMSE value when compared to other models

### Matrix Factorization

set.seed(1990) 

# Convert the train and test data into recosystem input format

train_set <-  with(train_data, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_set  <-  with(test_data,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# Create the model object

r <-  recosystem::Reco()

# Select the best tuning parameters

opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

# Train the algorithm  

r$train(train_set, opts = c(opts$min, nthread = 4, niter = 20))

#Estimating the predicted values

predict_reco <-  r$predict(test_set, out_memory())
head(predict_reco, 10)

result <- bind_rows(result, 
                    tibble(Method = "Matrix Factorization - recosystem", 
                           RMSE = RMSE(test_data$rating, predict_reco),
                           MSE  = MSE(test_data$rating, predict_reco),
                           MAE  = MAE(test_data$rating, predict_reco)))
print(result)

##### From the result tibble we can infer that Regularized model and Matrix Factorization models get RMSE Values less than the quoted RMSE values

### Implementing the Regularized movie and user effect model on final_holdout_test 

# Mean of rating from edx data set

mu_edx <- mean(edx$rating)

# Movie effect (bi)

b_i_edx <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

# User effect (bu)

b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

# Prediction 

predict_edx <- final_holdout_test  %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(prediction = mu_edx + b_i + b_u) %>%
  pull(prediction)

#  results tibble for final_holdout_test

result_final <- tibble(Method = "Project Goal", RMSE = 0.86490, MSE = NA, MAE = NA)
result_final <- bind_rows(result_final, 
                    tibble(Method = "Regularized movie and user effect model", 
                           RMSE = RMSE(final_holdout_test $rating, predict_edx),
                           MSE  = MSE(final_holdout_test $rating, predict_edx),
                           MAE  = MAE(final_holdout_test $rating, predict_edx)))

# Show the RMSE improvement

print(result_final )

### Implementing the Matrix Factorization model on final_holdout_test

set.seed(1990)

# Transforming the 'edx' and 'validation' sets to recosystem datasets

edx.reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
final_holdout_test.reco  <-  with(final_holdout_test, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Creating the reco model object

r <-  recosystem::Reco()

# Parameter Tuning

opts <-  r$tune(edx.reco, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Model Training

r$train(edx.reco, opts = c(opts$min, nthread = 4, niter = 20))

#Prediction

predict_reco_final <-  r$predict(final_holdout_test.reco, out_memory())

# Update the result table

result_final <- bind_rows(result_final, 
                    tibble(Method = "Final Matrix Factorization - Validation", 
                           RMSE = RMSE(final_holdout_test$rating, predict_reco_final),
                           MSE  = MSE(final_holdout_test$rating, predict_reco_final),
                           MAE  = MAE(final_holdout_test$rating, predict_reco_final)))
print(result_final)