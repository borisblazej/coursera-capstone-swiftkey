# helper functions for the n-gram model

## clean_text()
### input: any phrase of englisch text
### output: lower-case phrase without non-word characters

clean_text <- function(raw_text) {
    
    # all characters lowercase
    text_1 <- str_to_lower(raw_text)
    
    # remove "non-word" characters
    ### we allow  special characters ' like in it's and - like in e-learning
    
    text_2 <- str_replace_all(text_1, "[^\\w'\\-]", " ")
    text_2 <- str_replace_all(text_2, "[\\s]+", " ")
    text_2 <- str_replace_all(text_2, "[^A-Za-z'\\-]", " ")
    ### special characters survive only if ...
    ### ... not at begin/end
    text_2 <- str_replace_all(text_2, "^['-]", " ")
    text_2 <- str_replace_all(text_2, "['-]$", " ")
    ### ...not alone or beside each other
    text_2 <- str_replace_all(text_2, "'[\\s-]", " ")
    text_2 <- str_replace_all(text_2, "[\\s-]'", " ")
    text_2 <- str_replace_all(text_2, "-[\\s']", " ")
    text_2 <- str_replace_all(text_2, "[\\s']-'", " ")
    
    
    clean_text <- str_trim(text_2)
    
    clean_text
}

## last_3_words()
### input: a clean phrase of Englisch text
### output: vector of max. 3 words occuring at the end of the phrase

last_n_words <- function(phrase) {

    words <- str_split(phrase, "\\s") %>% 
        unlist()
    words <- str_trim(words)
    
    # return max. 3 words or number provided in phrase
    l <- min(length(words),3)
    
    tail(words, l)
    
}


