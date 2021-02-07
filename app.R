#
#
### Shiny App,  'An N-gram Prediction model for spoken English
#   This is a Shiny web application created in RStudio.  

# To execute:  select the 'Run App' button above.
# input:       a word or phrase entered to a text box
# output:      a predicted next word 

library(shiny)

suppressPackageStartupMessages({
    library(tidytext)
    library(tidyverse)
    library(stringr)
    library(knitr)
})


#' Source ngram matching function
#source("ngram.R")

#' Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("N-gram Prediction Model for spoken English"),
    p("This app takes as input, a phrase and predicts the next word as output."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Instructions:"), 
            h5("Enter a word or words in the text box."),
            h5("The predicted next word renders below it in purple."),
            br(),
            h4("Try these:"),
            h5("One more ____"),
            h5("Truth be ____"),
            h5("This is not my ____"),
            h5("Cinco de ____"),
            h5("I have no ____"),
            h5("What are you ____"),
            h5("Is it not a beautiful ____"),
            h5("Data scientists enjoy Johns Hopkins ____"),
            br(),
            
            a("Source Code", href = "https://github.com/mark-blackmore/JHU-Data-Science-Capstone/tree/master/ngram_match")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("predict",
                         textInput("user_input", h3("Enter a phrase here:"), 
                                   value = "Your words"),
                         h3("This is the predicted next word:"),
                         h4(em(span(textOutput("ngram_output"), style="color:purple")))),
                
                tabPanel("frequent trigrams",
                         br(),
                         img(src = "quadgrams.png", height = 200, width = 400)),
                         
                
                tabPanel("frequent bigrams",
                         br(),       
                         img(src = "trigrams.png", height = 200, width = 400)),
                
                tabPanel("frequent unigrams",
                         br(),
                         img(src = "bigrams.png", height = 200, width = 400))
            )   
        )
    )
)


# Define server logic required to draw a histogram
#' Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ngram_output <- renderText({
        #tri_words <- readRDS("/Users/susanlmartin/shinyapp/predmd/app_data/tri_words_fast.rds")
        #bi_words <- readRDS("/Users/susanlmartin/shinyapp/predmd/app_data/bi_words_fast.rds")
        #quad_words <- readRDS("/Users/susanlmartin/shinyapp/predmd/app_data/quad_words_fast.rds")
        ngrams(input$user_input)
    })
    
    #' Load Training Data, created by `04A_Task_Script.R`
    #bi_words <- readRDS("./app_data/bi_words_fast.rds")
    #tri_words  <- readRDS("./app_data/tri_words_fast.rds")
    #quad_words <- readRDS("./app_data/quad_words_fast.rds")
    
    #' Create User Input and Data Cleaning Function; Calls the matching functions
    ngrams <- function(input){
        # Create a dataframe
        input <- data_frame(text = input)
        # Clean the Inpput
        replace_reg <- "[^[:alpha:][:space:]]*"
        input <- input %>%
            mutate(text = str_replace_all(text, replace_reg, ""))
        # Find word count, separate words, lower case
        input_count <- str_count(input, boundary("word"))
        input_words <- unlist(str_split(input, boundary("word")))
        input_words <- tolower(input_words)
        # Call the matching functions
        out <- ifelse(input_count == 0, "Please input a phrase",
                      ifelse(input_count == 3, quadgram(input_words),
                             ifelse(input_count == 2, trigram(input_words), bigram(input_words))))
        
        # Output
        return(out)
    }
    
    #' Create Ngram Matching Functions
    bigram <- function(input_words){
        num <- length(input_words)
        bi_words <- readRDS("/Users/susanlmartin/shinyapp/predmd/app_data/bi_words_fast.rds")
        filter(bi_words, 
               word1==input_words[num]) %>% 
            top_n(1, n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word", 2)) %>%
            as.character() -> out
        ifelse(out =="character(0)", "?", return(out))
    }
    
    trigram <- function(input_words){
        num <- length(input_words)
        tri_words <- readRDS("/Users/susanlmartin/shinyapp/predmd/app_data/tri_words_fast.rds")
        filter(tri_words, 
               word1==input_words[num-1], 
               word2==input_words[num])  %>% 
            top_n(1, n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word", 3)) %>%
            as.character() -> out
        ifelse(out=="character(0)", bigram(input_words), return(out))
    }
    
    quadgram <- function(input_words){
        num <- length(input_words)
        quad_words <- readRDS("/Users/susanlmartin/shinyapp/predmd/app_data/quad_words_fast.rds")
        filter(quad_words, 
               word1==input_words[num-2], 
               word2==input_words[num-1], 
               word3==input_words[num])  %>% 
            top_n(1, n) %>%
            filter(row_number() == 1L) %>%
            select(num_range("word", 4)) %>%
            as.character() -> out
        ifelse(out=="character(0)", trigram(input_words), return(out))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
