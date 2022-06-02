#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)


# Define UI for application
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Next word prediction based on US twitter data"),
        
        # Sidebar with a text box for writing a phrase
        sidebarLayout(
            sidebarPanel(
                h3("Start writing ..."),
                textInput("input_phrase", "Input phrase", 
                          value = "", width = NULL, 
                          placeholder = NULL),
                actionButton("update", label = "Calculate")
            ),
            
            # Give most probable next word
            mainPanel(h3("Next Word Options:"),
                    verbatimTextOutput("next_word"))
        )
    )
)
        