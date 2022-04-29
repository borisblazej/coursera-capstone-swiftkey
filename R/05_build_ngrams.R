# Libraries ---------------------------------
library(stringr)
library(tidyverse)

# Data (Output of 02_Clean.R ) -----------
clean_text <- read_rds("./data/en_US.twitter_clean.rds.gz")


# Parameters / Regex Patterns ---------------------------------------

first_word_pattern <- "^[\\w'\\-]+\\s"
first_2_words_pattern <- "^[\\w'\\-]+\\s[\\w'\\-]+\\s"
first_3_words_pattern <- "^[\\w'\\-]+\\s[\\w'\\-]+\\s[\\w'\\-]+\\s"
gram2_pattern <- "[\\w'\\-]+\\s[\\w'\\-]+"
gram3_pattern <- "[\\w'\\-]+\\s[\\w'\\-]+\\s[\\w'\\-]+"
gram4_pattern <- "[\\w'\\-]+\\s[\\w'\\-]+\\s[\\w'\\-]+\\s[\\w'\\-]+"


# 2-grams -----------------------------------
gram2.1 <- str_match_all(clean_text, gram2_pattern) %>% 
    unlist()
### above only finds word pairs (1,2), (3,4), ... but not (2,3), hence:
gram2.2 <- str_replace(clean_text, first_word_pattern, "") %>% 
    str_match_all(gram2_pattern) %>% 
    unlist()
gram2 <- c(gram2.1, gram2.2)
gram2 <- str_trim(gram2)
rm(gram2.1, gram2.2)

# 3-grams --------------------------------------
gram3.1 <- str_match_all(clean_text, gram3_pattern) %>% 
    unlist()
gram3.2 <- str_replace(clean_text, first_word_pattern, "") %>% 
    str_match_all(gram3_pattern) %>% 
    unlist()
gram3.3 <- str_replace(clean_text, first_2_words_pattern, "") %>% 
    str_match_all(gram3_pattern) %>% 
    unlist()
gram3 <- c(gram3.1, gram3.2, gram3.3)
gram3 <- str_trim(gram3)

rm(gram3.1, gram3.2, gram3.3)
gc()

# 4-grams --------------------------------------

gram4.1 <- str_match_all(clean_text, gram4_pattern) %>% 
    unlist()
gram4.2 <- str_replace(clean_text, first_word_pattern, "") %>% 
    str_match_all(gram4_pattern) %>% 
    unlist()
gram4.3 <- str_replace(clean_text, first_2_words_pattern, "") %>% 
    str_match_all(gram4_pattern) %>% 
    unlist()
gram4.4 <- str_replace(clean_text, first_3_words_pattern, "") %>% 
    str_match_all(gram4_pattern) %>% 
    unlist()
gram4 <- c(gram4.1, gram4.2, gram4.3, gram4.4)
gram4 <- str_trim(gram4)

rm(gram4.1, gram4.2, gram4.3, gram4.4)
gc()

# write to persistent data files

write_rds(words, "./data/en_US.twitter_word.rds.gz", compress = "gz")
write_rds(gram2, "./data/en_US.twitter_gram2.rds.gz", compress = "gz")
write_rds(gram3, "./data/en_US.twitter_gram3.rds.gz", compress = "gz")
write_rds(gram4, "./data/en_US.twitter_gram4.rds.gz", compress = "gz")



