# Implementation of an n-gram Katz model, version 1

# Libraries
library(tidyverse)
library(stringr)
library(svMisc)

# Public Functions ----------------------------------------

load_text <- function(textfile, nrows = 100000) {
    
    con <- file(textfile)
    text.raw <- readLines(con)
    close(con)
    text.sample <- sample(x = text.raw, 
                          size = nrows)
    text.clean <- clean_text(text.sample)

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
    text1
    
}

## for train_model()
build_ngram <- function(text, n, keep_top = 1000) {
    
    ### Development: #####
    # text <- train_text
    # n <- 2
    ######################
    
    if(n == 1) {
        
        ### step 1: split text rows into words
        words <- str_split(text, "\\s") %>% 
            unlist()
        words <- str_trim(words)
        
        ### step 2: count by words
        word_freq <- as.data.frame(words) %>% 
            group_by(words) %>% 
            summarize(freq = n()) %>% 
            arrange(-freq) %>% 
            filter(words != "") %>%
            head(keep_top)
        
        ### step 3: calculate probabilities from frequencies
        tot_n <- sum(word_freq$freq)
        
        ngram <- word_freq %>% 
            mutate(prob = freq / tot_n) %>% 
            select(-freq)
    
    }
    
    if (n > 1) {
        
        ngram <- list()
        
        ### step 1: split text into ngrams
        
        word_separated_lines <- str_split(text, "\\s")
        
        n_lines <- length(word_separated_lines)
        
        for(i in 2:n) { # bigram, trigram, ...
            
            for (line in 1:n_lines) {
                
                ### Development ###
                # i <- 2
                # line <- 3
                ###################
                
                ngram[[line]] <- list()
                
                n_words <- length(word_separated_lines[[line]])
                ### in a line with n_words n_ngrams are possible
                n_ngrams <- n_words + 1 - i
                
                for (p in (1:n_ngrams)) {
                    # start pos. of possible ngrams
                    
                    ### Development ###
                    # p <- 2
                    ###################
                    if (n_ngrams > 1) { 
                        ngram[[line]][[p]] <- character()
                        
                        for (j in 1:i) {
                            ngram[[line]][[p]] <-
                                c(ngram[[line]][[p]],
                                  word_separated_lines[[line]][p + j - 1])
                            
                        }
                    }
                }
            }
        }

    }
    
    ngram
    
}
