## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

library(lobstr)
mem_used()

source('helpers.R')

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


shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = span("Movie Recommender", 
                                      style = "color: white; font-size: 17px; font-weight: bold; font-family: American Typewriter, serif")),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Genre Recommender", tabName = "RecommendationbyGenre", icon = icon("list-alt")),
        menuItem("Rating Recommender", tabName = "RecommendationbyUserRating", icon = icon("table")),
        menuItem("Readme", tabName = "Readme", icon = icon("readme"))
      )),
    
    dashboardBody(includeCSS("movies.css"),
                  tabItems(
                    tabItem(tabName = "RecommendationbyGenre",
                            fluidRow(
                              box(width = 12, title = "Step 1: Select your favourite movie genre", status = "info", solidHeader = TRUE, collapsible = FALSE,
                                  selectInput("genre",
                                              "Genre:",
                                              c(unique(movies$Genre))),
                                  uiOutput("genre_input")
                              )
                            ),
                            
                            fluidRow(
                              useShinyjs(),
                              box(
                                width = 12, status = "info", solidHeader = TRUE,
                                title = "Step 2: Discover Top 5 movies of your favourite movie genre",
                                br(),
                                withBusyIndicatorUI(
                                  actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")

                                ),
                                br(),
                                tableOutput("results1")
                              )
                            )
                    ),  
                    
                    tabItem(tabName = "RecommendationbyUserRating",          
                            fluidRow(
                              box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                  div(class = "rateitems",
                                      uiOutput("ratings")
                                  )
                              )
                            ),
                            
                            fluidRow(
                              useShinyjs(),
                              box(
                                width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                                title = "Step 2: Discover movies you might like based on your rating",
                                br(),
                                withBusyIndicatorUI(
                                  actionButton("btn2", "Click here to see recommendations", class = "btn-warning")
                                ),
                                br(),
                                tableOutput("results2")
                                
                              )
                            )
                    ),
                    tabItem(tabName = "Readme",          
                            fluidRow(box(width = 20, status = "info", includeMarkdown("Readme.Rmd"))
                                  ))
                              )
                            )

                )  
    )
