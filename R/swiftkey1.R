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
                          size = min(nrows, length(text.raw)
                                     ))
    text.clean <- clean_text(text.sample)

    text.clean
}

train_model <- function(text, max_n = 4, keep_top = 1000) {
    
    ngrams <- list()
    for(n in 1:max_n) {
        ngrams[[n]] <- build_ngram(text, n, keep_top)
        
    }
    
    ngrams
}

next_word <- function(model, phrase, max_n = 3) {
    
    # model <- ngram_model
    # phrase <- "through the"
    # max_n <- 3
    
    clean_phrase <- clean_text(phrase)
    final_words <- last_n_words(clean_phrase, max_n)
    
    pred_list <- prediction_list(model, final_words)
    
    as.character(pred_list[1,1])
    
    
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
    text1 <- str_replace_all(text1, "^['-]+", "")
    text1 <- str_replace_all(text1, "['-]+$", "")
    ### ...not alone or beside each other
    text1 <- str_replace_all(text1, "'+[\\s-]", "")
    text1 <- str_replace_all(text1, "[\\s-]'+", "")
    text1 <- str_replace_all(text1, "-+[\\s']", "")
    text1 <- str_replace_all(text1, "[\\s']-+", "")
    
    text1 <- str_trim(text1)
    text1
    
}

## for train_model()
build_ngram <- function(text, n, keep_top) {
    
    ### Development: #####
    # text <- train_text
    # n <- 2
    ######################
    
    text <- str_replace_all(text, "\\s+", " ")

    
    if(n == 1) {
        
        ### step 1: split text rows into words
        words <- str_split(text, "\\s") %>% 
            unlist()
        X1 <- str_trim(words)
        
        ### step 2: count by words
        ngram_freq <- as.data.frame(X1) %>% 
            group_by(X1) %>% 
            summarize(freq = n()) %>% 
            arrange(-freq) %>% 
            filter(X1 != "") %>%
            head(keep_top)
        
        ### step 3: calculate probabilities from frequencies
        # tot_n <- sum(ngram_freq$freq)
        # 
        # ngram <- ngram_freq %>% 
        #     mutate(prob = freq / tot_n) %>% 
        #     select(-freq)
    
    }
    
    if (n > 1) {
        
        ngram_raw <- list()
        
        ### step 1: split text into ngrams
        
        word_separated_lines <- str_split(text, "\\s")

        n_lines <- length(word_separated_lines)
        
        for(i in 2:n) { # bigram, trigram, ...
            
            for (line in 1:n_lines) {
                
                ### Development ###
                # i <- 2
                # line <- 3
                ###################
                
                ngram_raw[[line]] <- list()
                
                n_words <- length(word_separated_lines[[line]])
                ### in a line with n_words n_ngrams are possible
                n_ngrams <- n_words + 1 - i
                
                for (p in 1:n_ngrams) { # start pos. of possible ngrams
                    
                    ### Development ###
                    # p <- 2
                    ###################
                    if (n_ngrams >= 1) { 
                        ngram_raw[[line]][[p]] <- character()
                        
                        for (j in 1:i) { # words in current ngram
                            ngram_raw[[line]][[p]][[j]] <-
                                word_separated_lines[[line]][p + j - 1]
                                
                                
                                # paste(ngram_raw[[line]][[p]],
                                #       word_separated_lines[[line]][p + j - 1],
                                #       sep = " ")

                        }
                    }
                }
            }
        }
        
        ngram_list <- unlist(ngram_raw, recursive = FALSE)
        
        ngram_df <- data.frame(t(sapply(ngram_list,c)))
        
        ### step 2: count by ngram
        ngram_freq <- ngram_df %>%
            group_by_all() %>% 
            summarize(freq = n(), .groups = "keep") %>% 
            ungroup() %>% 
            arrange(-freq) %>%
            head(keep_top)
        
        ### step 3: calculate probabilities from frequencies
        # tot_n <- sum(ngram_freq$freq)
        # 
        # ngram <- ngram_freq %>%
        #     mutate(prob = freq / tot_n) %>%
        #     select(-freq)
    }
    
    ngram_freq
    
}

last_n_words <- function(phrase, n) {

## last_n_words()
### input: a clean phrase of Englisch text
### output: vector of max. n words occuring at the end of the phrase
    
    words <- str_split(phrase, "\\s") %>% 
        unlist()
    words <- str_trim(words)
    
    # return max. n words or number provided in phrase
    l <- min(length(words),n)
    
    tail(words, l)
    
}

prediction_list <- function(model, words) {
    
    ### Development: ############
    # model <- ngram_model
    # words <- c("i", "thank")
    # ########################
    
    n_words <- length(words)
    
    ngram_len <- length(model)

    
    max_freq <- sapply(model, function(x) {
        max(x$freq)
    })
    
    top_freqs <- data.frame()
    # top_freqs <- list()
    
    
    for (n in 2:ngram_len) { # bigram, trigram ...
        
        ### Development: ############
        # n <- 4
        ########################
        
        ### if less than maximum possible number of words given in input phrase
        ### we must consider a shift for filtering
        ### maximum possible is (n -1) where the shift  = 0
        filter_shift <- n -1 -n_words
        
        freqs_raw <- model[[n]]
        
        first_used_word <- max(n_words+2-n,1)
        
        ### Loop through words of input phrase
        ### Attention: depending on ngram_len not all words can be used!
        for (word_no in first_used_word:n_words) { 
        
            ### Development: ############
            # word_no <- 2
            ########################
            
            ### stepwise reduction of raw frequencies by filtering by words
            ### from i nput phrase
            ### Attention: the position >> ngram_pos + ngram_len - 1 -n_words <<
            ### considers input phrases with less than maximum number of words,
            ### e.g. a 2-word phrase for a 4-gram model
            freqs_raw <- freqs_raw %>% 
                filter_at((word_no + filter_shift),
                          all_vars(. == words[word_no]))
            }

        freqs <- freqs_raw  %>%
             select_at(c(n, n + 1)) %>% 
            mutate("n" = n)
        
        names(freqs) <- c("word", "score", "n")
        
        top_freqs <- rbind(top_freqs, freqs) 
            
        
    }
    
    ### add most frequent single words for no match in ngrams:
    single_freqs <- model[[1]] %>%
        head(5) %>% 
        mutate(word = X1,
               score = 6 - row_number(),
               n = 1) %>% 
        select(word, score, n)
    
    top_freqs <- rbind(top_freqs, single_freqs)             
        
    top_freqs <- top_freqs %>%
        group_by(word) %>%
        summarise(final_score = sum(score ^ n)) %>% 
        arrange(-final_score)

    top_freqs
    
}
