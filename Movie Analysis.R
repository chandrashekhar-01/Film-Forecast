library(ggplot2)
library(dplyr)    
library(rpart)    
library(rpart.plot)  

IMDB <- read.csv("C:/Users/Chandra Shekhar/Downloads/movie_metadata.csv", header = TRUE, sep = ",")
IMDB <- IMDB[!duplicated(IMDB), ]
IMDB <- IMDB[!is.na(IMDB$gross) & !is.na(IMDB$budget), ]
IMDB$facenumber_in_poster[is.na(IMDB$facenumber_in_poster)] <- round(mean(IMDB$facenumber_in_poster, na.rm = TRUE))


cols_to_check <- c("director_facebook_likes", "actor_3_facebook_likes", 
                   "actor_1_facebook_likes", "cast_total_facebook_likes", 
                   "actor_2_facebook_likes", "aspect_ratio")
for (col in cols_to_check) {
  if (col %in% colnames(IMDB)) {
    IMDB[[col]][IMDB[[col]] == 0] <- NA
  }
}

cols_to_impute <- c("num_critic_for_reviews", "cast_total_facebook_likes", "movie_facebook_likes")
for (col in cols_to_impute) {
  if (col %in% colnames(IMDB)) {
    IMDB[[col]][is.na(IMDB[[col]])] <- round(mean(IMDB[[col]], na.rm = TRUE))
  }
}

IMDB <- subset(IMDB, select = c(
  gross, imdb_score, num_critic_for_reviews, num_voted_users, 
  cast_total_facebook_likes, facenumber_in_poster, num_user_for_reviews, 
  budget, movie_facebook_likes
))

set.seed(45)
train.index <- sample(row.names(IMDB), dim(IMDB)[1] * 0.6)
valid.index <- sample(setdiff(row.names(IMDB), train.index), dim(IMDB)[1] * 0.2)
test.index <- setdiff(row.names(IMDB), union(train.index, valid.index))

train <- IMDB[train.index, ]
valid <- IMDB[valid.index, ]
test <- IMDB[test.index, ]

# Train Decision Tree
class.tree <- rpart(imdb_score ~ . -gross, data = train, method = "anova")

# Cross-validation and pruning to avoid overfitting
set.seed(51)
cv.ct <- rpart(imdb_score ~ . -gross, data = train, method = "anova", cp = 0.00001, minsplit = 5, xval = 5)
pruned.ct <- prune(cv.ct, cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])

# Generate predictions for train, validation, and test sets
tree.pred.train <- predict(pruned.ct, train)
tree.pred.valid <- predict(pruned.ct, valid)
tree.pred.test <- predict(pruned.ct, test)

# Calculate RMSE for each set
rmse.train <- sqrt(mean((tree.pred.train - train$imdb_score)^2))
rmse.valid <- sqrt(mean((tree.pred.valid - valid$imdb_score)^2))
rmse.test <- sqrt(mean((tree.pred.test - test$imdb_score)^2))

# Print RMSE for training, validation, and test sets
cat("Training RMSE: ", rmse.train, "\n")
cat("Validation RMSE: ", rmse.valid, "\n")
cat("Test RMSE: ", rmse.test, "\n")

# Get input from the user for new movie prediction
movie_name <- readline(prompt = "Enter the name of the movie: ")

# Create a data frame for the new user input
new_data <- data.frame(
  gross = 1500000000, 
  num_critic_for_reviews = 1000,  
  num_voted_users = 1000000,  
  cast_total_facebook_likes = 2000000,
  facenumber_in_poster = 5,  
  num_user_for_reviews = 5000,
  budget = 300000000,  
  movie_facebook_likes = 500000  
)


# Predict on the new data
new_data.pred <- predict(pruned.ct, new_data)

# Classify the prediction into categories: Blockbuster, Super Hit, Average, Flop, Disaster
if (new_data.pred >= 7.5) {
  category <- "Blockbuster"
} else if (new_data.pred >= 7) {
  category <- "Super Hit"
} else if (new_data.pred >= 5) {
  category <- "Average"
} else if (new_data.pred >= 3) {
  category <- "Flop"
} else {
  category <- "Disaster"
}

cat("\nMovie: ", movie_name, "\n")
cat("Prediction for IMDb Score: ", new_data.pred, "\n")

if (category == "Blockbuster") {
  cat("\033[1;32m", movie_name, "is expected to be a mega hit! This is going to be a Blockbuster!\033[0m\n")
} else if (category == "Super Hit") {
  cat("\033[1;32m", movie_name, "will be a big success! It’s a Super Hit!\033[0m\n")
} else if (category == "Average") {
  cat("\033[1;33m", movie_name, "is expected to perform reasonably well. This will be an Average movie.\033[0m\n")
} else if (category == "Flop") {
  cat("\033[1;31m", movie_name, "might struggle at the box office. It's going to be a Flop.\033[0m\n")
} else {
  cat("\033[1;35m", movie_name, "seems to be heading for disaster. This is a Disaster!\033[0m\n")
}
