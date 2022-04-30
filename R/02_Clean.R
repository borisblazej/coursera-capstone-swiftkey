### Task: load raw_text data as developed in 01_Explore.Rmd from a given corpus and do some transparent cleaning steps
### Output: clean_text

# 0. Libraries
library(stringr)
library(readr)

# 1. Load raw data
### In case of error run 01_Explore.Rmd

raw_data_path <- "./data/en_US.twitter.txt"
con <- file(raw_data_path)
raw_text <- readLines(con)
### for runtime optimization use only 10% sample
raw_text <- sample(raw_text, floor(0.3 * length(raw_text)))

# 2. All characters lowercase

text_1 <- str_to_lower(raw_text)

# 3. Remove "non-word" characters
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

# 99. Write RDS to use in further steps

write_rds(clean_text, 
          "./data/en_US.twitter_clean.rds.gz", compress = "gz")






