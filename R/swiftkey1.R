# Implementation of an n-gram Katz model, version 1

# Libraries
library(tidyverse)
library(stringr)
library(svMisc)

# Public Functions ----------------------------------------

load_text <- function(textfile, nrows = 100000) {
    
    con <- file(textfile)
    text.raw <- readLines(con)
    print.default(">>> File read ... <<<")
    close(con)
    text.sample <- sample(x = text.raw, 
                          size = nrows)
    print.default(">>> Text sampled ... <<<")
    text.clean <- clean_text(text.sample)
    print.default(">>> Text cleaned OK <<<")
    
    text.clean
}

train_model <- function(text) {
    
    ngrams <- list()
    for(n in 1:4) {
        ngram[[n]] <- build_ngram(text, n)
        
        
        
        
    }
    
    
    return(model)
}

next_word <- function(phrase) {
    
}

# Internal Functions ----------------------------------------

## for load_text()
clean_text <- function(text) {
    
    text1 <- str_to_lower(text)
    print.default(">>> Only lower-case characters ... <<<")
    
    ### remove "non-word" characters
    ### we allow  special characters ' like in it's and - like in e-learning
    text1 <- str_replace_all(text1, "[^\\w'\\-]", " ")
    text1 <- str_replace_all(text1, "[\\s]+", " ")
    text1 <- str_replace_all(text1, "[^A-Za-z'\\-]", " ")
    
    ### special characters survive only if ...
    ### ... not at begin/end
    text1 <- str_replace_all(text1, "^['-]", " ")
    text1 <- str_replace_all(text1, "['-]$", " ")
    ### ...not alone or beside each other
    text1 <- str_replace_all(text1, "'[\\s-]", " ")
    text1 <- str_replace_all(text1, "[\\s-]'", " ")
    text1 <- str_replace_all(text1, "-[\\s']", " ")
    text1 <- str_replace_all(text1, "[\\s']-'", " ")
    
    text1 <- str_trim(text1)
    print.default(">>> Non-word characters removed ... <<<")
    text1
    
}

## for train_model()
build_ngram <- function(text, n, keep_top = 1000) {
    
    if(n == 1) {
        
        words <- str_split(text, "\\s") %>% 
            unlist()
        words <- str_trim(words)
        
        ngram_freq <- as.data.frame(words) %>% 
            group_by(words) %>% 
            summarize(freq = n()) %>% 
            arrange(-freq) %>% 
            filter(words != "") %>%
            head(keep_top)
        
        tot_n <- sum(ngram_freq$freq)
        
        ngram <- ngram_freq %>% 
            mutate(prob = freq / tot_n) %>% 
            select(-freq)
    
        }
    
    ngram
    
}
