# 0. Setup ---------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(readr)

# PARAMETERS ------------------------------------------

### The "package" = essential functions
source("swiftkey1.R")

shinyServer(function(input, output) {

    # 1. Load model ------------------------------
    ngram_model <- read_rds("./en_US.twitter_model.rds.gz")
    
    # 2. Get input phrase ------------------------------

    input_phrase <- eventReactive( input$update, {
        input$input_phrase
    })
    
    output_words <- eventReactive(input$update, {
        
        prediction_list(ngram_model, last_n_words(input_phrase(),3))$word[1:3]
        
    })
    
            
    # 3. Predict next word ------------------------------
    
    output$next_word_1 <- renderText({
        output_words()[1]
    })
    output$next_word_2 <- renderText({
        output_words()[2]
    })
    output$next_word_3 <- renderText({
        output_words()[3]
    })
        
   
})
