######
library(SparseM)
library(Matrix)
library(dplyr)
######

get_user_ratings <- function(value_list) {
  dat <- data.table(MovieID = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(MovieID)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), rating = as.numeric(rating))]
  dat <- dat[rating > 0]

  
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$MovieID, 
                               j = rep(1,nrow(dat)), 
                               x = dat$rating, 
                               dims = c(nrow(ratingmat), 1))
}


get_genre_ratings <- function(value_list, selected_genre) {

  dat <- data.table(MovieID = value_list[1], rating = 5, genre = value_list[3] )
  colnames(dat) <- c("MovieID", "rating", "genre")
  dat[, ':=' (MovieID = as.numeric(MovieID), rating = as.numeric(rating))]

  dat <- dat[genre == selected_genre]
  print(dat)
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$MovieID, 
                               j = rep(1,nrow(dat)), 
                               x = dat$rating, 
                               dims = c(nrow(ratingmat), 1)
  )

}

get_bayesian_genre_ratings <- function(value_list, selected_genre) {
  
  dat = as.data.frame(top_10_movies_genre %>% filter(Genres == selected_genre))
  return(dat)
  
}


# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
Genre = as.character(movies$Genres)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

######
ratings = readLines(paste0(myurl, 'ratings.dat?raw=true'))
ratings = strsplit(ratings, split = "::", fixed = TRUE, useBytes = TRUE)
ratings = matrix(unlist(ratings), ncol = 4, byrow = TRUE)
ratings = data.frame(ratings, stringsAsFactors = FALSE)
colnames(ratings) = c('user_id', 'MovieID', 'rating', 'timestamp')
ratings$MovieID = as.integer(ratings$MovieID)
ratings$rating = as.integer(ratings$rating)
ratings$user_id = as.integer(ratings$user_id)
#print(ratings)
typeof(ratings)
######

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


###### Pre-processing for Bayesian Input
## Sytem 1 code###
# Define a method to categorize a review as positive or negative
pos_neg_decision = 3

rating_sentiment = ratings %>%
  mutate(pos_sentiment = rating > pos_neg_decision)

movie_avg_rating = rating_sentiment %>%
  group_by(MovieID) %>%
  summarise(positive_rating = sum(pos_sentiment), total = n())

alpha_movie = movie_avg_rating$positive_rating + 1
beta_movie = movie_avg_rating$total - movie_avg_rating$positive_rating + 1
bayesian_mean_rating = alpha_movie/(alpha_movie + beta_movie)
movie_avg_rating$bayesian_rating = bayesian_mean_rating
print("Bayesian Rating calculation done")
dim(movie_avg_rating)

movie_genre_with_rating = left_join(movie_avg_rating, movies, by = "MovieID")

head(movie_genre_with_rating)

# Get top 10 for each Genre
top_10_movies_genre = movie_genre_with_rating %>% group_by(Genres) %>% slice_max(order_by = bayesian_rating, n = 10)

######
# reshape to movies x user matrix
ratingmat <- sparseMatrix(ratings$MovieID, ratings$user_id, x=ratings$rating) # movie x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dim(ratingmat)

dimnames(ratingmat) <- list(MovieID = as.character(1:3952), user_id = as.character(sort(unique(ratings$user_id))))

######

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data

      value_list <- reactiveValuesToList(input)
      #user_ratings <- get_genre_ratings(movies, input$genre)
      genre_recs <- get_bayesian_genre_ratings(movies, input$genre)
      ######
      user_predicted_ids = genre_recs$MovieID
      
      recom_results1 <- data.table(Rank = 1:10, 
                                   MovieID = movies$MovieID[movies$MovieID %in% user_predicted_ids], 
                                   Title = movies$Title[movies$MovieID %in% user_predicted_ids], 
                                   Predicted_rating =  genre_recs$bayesian_rating,
                                   Genre = movies$Genre[movies$MovieID %in% user_predicted_ids])
      ######
    }) # still busy
    
  }) # clicked on button
  
  df2 <- eventReactive(input$btn2, {  
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)

      user_ratings <- get_user_ratings(value_list)
      
      ######
      # add user's ratings as first column to rating matrix
      rmat <- cbind(user_ratings, ratingmat)

      
      # get the indices of which cells in the matrix should be predicted
      # predict all books the current user has not yet rated
      items_to_predict <- which(rmat[, 1] == 0)

      prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
      
      rmat <- t(rmat)
      rmat = new('realRatingMatrix', data = rmat)
      model <- Recommender(rmat, method = "UBCF", param=list(normalize = "center", method="Cosine", nn=5))
      
      res <- predict(model, rmat[1], type='ratings')
      res_list <- as(res, 'list' )
      print(res_list)
      user_results <- sort(res_list[[1]], decreasing = TRUE)[1:10]

      user_predicted_ids <- as.numeric(names(user_results))      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)

      
    }) # still busy
    
  })
  
  
  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result_genre <- df1()
    recom_result_final <- recom_result_genre %>%
      distinct(MovieID, .keep_all = TRUE)
    num_movies1 <- nrow(recom_result_final)

    if(nrow(recom_result_final) > 5){

      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[movies$MovieID == recom_result_final$MovieID[j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[movies$MovieID == recom_result_final$MovieID[j]])
            )
        )        
      }))) # columns
      #})
    } # rows
    else{
      showModal(modalDialog(
        title = "There are less than 5 movies of your favourite genre available in the database"))
      list(fluidRow(lapply(1:num_movies1, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[movies$MovieID == recom_result_final$MovieID[j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[movies$MovieID == recom_result_final$MovieID[j]])
            )
        )        
      }))) # columns
      #})
    } #rows
    
  })
  
  output$results2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result_rating <- df2()

    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result_rating$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result_rating$MovieID[(i - 1) * num_movies + j]])
            )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function