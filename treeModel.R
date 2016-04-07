library(rpart)
library(rpart.plot)

setwd("F:/ml-1m")
options(stringsAsFactors = FALSE)
movies = readLines("movies.dat")
movies_df = do.call(rbind.data.frame, strsplit(movies, "::"))
names(movies_df) <- c("MovieID", "Title", "Genres")
allGenres <- unique(unlist(strsplit(movies_df$Genres, "[|]")))


ratings = readLines("ratings.dat")
ratings_df = do.call(rbind.data.frame, strsplit(ratings, "::"))
names(ratings_df) <- c("UserID", "MovieID", "Rating", "Timestamp")
ratings_df$Rating <- as.numeric(ratings_df$Rating)
ratings_df$Timestamp <- as.POSIXct(as.numeric(ratings_df$Timestamp), origin = "1970-01-01")
ratings_df$weekday <- format(ratings_df$Timestamp, "%a")
ratings_df$weekday <- ifelse(ratings_df$weekday %in% c("週六", "週日"), "weekend", "workday")
ratings_df$hours <- as.numeric(format(ratings_df$Timestamp, "%H"))
ratings_df$hours <- ifelse(ratings_df$hours > 6 & ratings_df$hours < 18 , "day", "night")
ratings_df$Target <- ifelse(ratings_df$Rating %in% c(4, 5), 1, 0)


users = readLines("users.dat")
users_df = do.call(rbind.data.frame, strsplit(users, "::"))
names(users_df) <- c("UserID", "Gender", "Age", "Occupation", "Zip-code")



getTrainData = function(genres){
  #genres = "Children's"
  MovieID = movies_df[grep(genres, movies_df$Genres), "MovieID"]
  df = ratings_df[ratings_df$MovieID %in% MovieID, c("UserID", "weekday", "Target")]
  df = merge(df, users_df, by = "UserID")
  df = df[, setdiff(names(df), c("UserID", "Zip-code", "Occupation"))]
}

getTreeModel = function(genres){
  model <- rpart(formula = Target ~ ., data = getTrainData(genres), 
                 control = rpart.control(cp = -1, maxdepth = 2))
  rpart.plot(model)
  model
  
}

allGenres
getTreeModel("Horror")




