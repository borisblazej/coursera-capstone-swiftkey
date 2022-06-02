# 0. Setup ---------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)

# PARAMETERS ------------------------------------------

### The "package" = essential functions
source("swiftkey1.R")

shinyServer(function(input, output) {

    # 1. Load model ------------------------------
    ngram_model <- read_rds("../data/en_US.twitter_model.rds.gz")

    # 2. Get input phrase ------------------------------

    input_phrase <- eventReactive( input$update, {
            input_phrase <- input$input_phrase
    })
            
    # 3. Predict next word ------------------------------
            
    output$next_word <- renderText({
        next_word(ngram_model, input_phrase)
    })
        
   
})
