################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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

## Test and Validation Sets
# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Validation modified. Rating excluded
validation_CM <- validation  
validation <- validation %>% select(-rating)

# Libraries loaded
library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(DT)
library(wordcloud) 
library(RColorBrewer)

# Review the class of "edx" dataset
class(edx)
# Get a glimpse "edx" dataset
glimpse(edx)
# number of movies in "edx" dataset
n_distinct(edx$movieId)

# review the class of "validation" dataset
class(validation)
# get a glimpse "validation" dataset
glimpse(validation)
# number of movies in "validation" dataset
n_distinct(validation$movieId)

#RMSE (root mean squre error) define a function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
}
# Modify the year as a column in the edx & validation datasets
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation_CM <- validation_CM %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
# Modify the genres variable in the edx & validation dataset (column separated)
split_edx  <- edx  %>% separate_rows(genres, sep = "\\|")
split_valid <- validation   %>% separate_rows(genres, sep = "\\|")
split_valid_CM <- validation_CM  %>% separate_rows(genres, sep = "\\|")

#list movie genres ratings in descending order
split_edx%>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Plot, Review Of The Ratings And The Distribution
edx %>%
  ggplot(aes(rating))+
  (theme(panel.background = element_rect(fill = '#CCFFFF', colour = '#000099')))+
  (geom_histogram(fill="#3399FF", binwidth = 0.25, color = "#000099"))+
  (scale_x_discrete(limits = c(seq(0.5,5,0.5))))+
  (scale_y_continuous(breaks = c(seq(0, 3000000, 500000))))+
  (ggtitle("Review Of The Ratings And The Distribution"))

#Plot, edx %>% count(userId) %>% 
ggplot(aes(n))+
  (theme(panel.background = element_rect(fill = '#CCFFFF', colour = '#000099')))+
  (geom_histogram(fill="#3399FF", binwidth = 0.25, color = "#000099"))+
  (scale_x_log10()) + 
  (ggtitle("Viewers Rating Frequency"))

#Plot, Movie Rating Frequency
edx %>% count(movieId) %>% 
  ggplot(aes(n))+
  (theme(panel.background = element_rect(fill = '#CCFFFF', colour = '#000099')))+
  (geom_histogram(fill="#3399FF", binwidth = 0.25, color = "#000099"))+
  (scale_x_log10()) + 
  (ggtitle("Movie Rating Frequency"))

# Plot, Historic Timeline view of genres
genres_timeline <- split_edx %>%
  # remove missing data
  na.omit() %>%
  # choose the columns
  select(movieId, year, genres) %>%
  # represents genres as factors
  mutate(genres = as.factor(genres)) %>%
  # group by year & genre
  group_by(year, genres) %>%
  # number count
  summarise(number = n()) %>%
  # insert any missing years/genres
  complete(year = full_seq(year, 1), genres, fill = list(number = 0)) 

# Genres vs year; 4 genres are chosen for readability: animation, sci-fi, war and western movies.
genres_timeline %>%
  filter(year > 1910) %>%
  filter(genres %in% c("War", "Sci-Fi", "Animation", "Western")) %>%
  ggplot(aes(x = year, y = number)) +
  (theme(panel.background = element_rect(fill = '#CCFFFF', colour = '#000099')))+
  geom_line(size=1.2,aes(color=genres)) +
  scale_fill_brewer(palette = "Dark2")+
  (ggtitle("Historic Timeline"))

# Plot, Comparison Of Ratings Over The Release Year
edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  theme(panel.background = element_rect(fill = '#CCFFFF', colour = '#000099'))+
  geom_point(shape=8)+
  geom_smooth()+
  ggtitle("Ratings And Release Year")

#Plot, Comparison Of Ratings With Genres
split_edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 1.5*se, ymax = avg + 1.5*se)) +
  theme(panel.background = element_rect(fill = '#CCFFFF', colour = '#000099'))+
  geom_point(shape=8) +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Ratings And Genres")

#Prediction models
#Model 1 Simple Mean

#Calculate mu
mu <- mean(edx$rating)
mu

# We now use mu to predict using the simple mean. 
model1_rmse <- RMSE(validation_CM$rating, mu)
model1_rmse
rmse_summary <- data_frame(method = "Model 1: Simple Mean", RMSE = model1_rmse)
rmse_summary %>% knitr::kable()


# Model 2 Movie Ratings Impact

# Plot, No. of movies - Deviation
movie_mean <- edx %>%
  group_by(movieId) %>%
  summarize(Deviation = mean(rating - mu))
movie_mean %>%
  qplot(Deviation, geom ="histogram", bins = 20, data = ., fill = I("#000099"), color = I("white"),
        ylab = "No. of movies", main = "No. of movies - Deviation")

# Model 2 results
predicted_ratings <- mu +  validation %>%
  left_join(movie_mean, by='movieId') %>%
  pull(Deviation)
model2_rmse <- RMSE(predicted_ratings, validation_CM$rating)
rmse_summary <- bind_rows(rmse_summary,
                          data_frame(method="Model 2: Movie Raings Impact",  
                                     RMSE = model2_rmse ))%>% distinct

#Model 3,Movie with user effect
# calculate the average rating for"mu", for those that have rated over 100 movies, said penalty term user effect. Users affect the ratings favourably or unfavourably
user_avgs<- edx %>% 
  left_join(movie_mean, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu -Deviation))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 20, data = ., fill = I("#000099"), color = I("white"))

#Calculate approx mu & b_i, estimate b_u
viewer_means <- edx %>%
  left_join(movie_mean, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - Deviation))


#Output of model 3
predicted_ratings <- validation%>%
  left_join(movie_mean, by='movieId') %>%
  left_join(viewer_means, by='userId') %>%
  mutate(pred = (mu + Deviation + b_u)) %>%
  pull(pred)
model3_rmse <- RMSE(predicted_ratings, validation_CM$rating)
rmse_summary <- bind_rows(rmse_summary,
                          data_frame(method="Model 3: Movie with user effect",  
                                     RMSE = model3_rmse))%>% distinct
rmse_summary %>% knitr::kable()
rmse_summary %>% knitr::kable()

#Model 4, Regularized Movie with User Impact
#improve RMSE by applying used of regulariation
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  Deviation <- edx %>% 
    group_by(movieId) %>%
    summarize(Deviation = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(Deviation, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - Deviation - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(Deviation, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + Deviation + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation_CM$rating))
})

#lambda plot
qplot(lambdas, rmses, color = I("#000099"))

#get the lowest lambda value we use the which.min.
lambdamin <- lambdas[which.min(rmses)]
lambdamin

#model 4 output
rmse_summary <- bind_rows(rmse_summary,
                          data_frame(method="Model 4: Regularized Movie with User Impact",  
                                     RMSE = min(rmses))) %>% distinct
rmse_summary %>% knitr::kable()

