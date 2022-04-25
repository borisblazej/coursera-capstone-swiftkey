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

head(raw_text, 10)

# 2. Remove "non-word" characters

text_1 <- str_replace_all(raw_text, "[^\\w]", " ")
text_1 <- str_replace_all(text_1, "[\\s]+", " ")
text_1 <- str_replace_all(text_1, "[^A-Za-z0-9]", " ")
text_1 <- str_trim(text_1)

head(text_1, 10)

# 3. All characters lowercase

clean_text <- str_to_lower(text_1)

sample(clean_text, 100)



# 99. Write RDS to use in furtehr steps

write_rds(clean_text, "./data/en_US.twitter_clean.rds.gz", compress = "gz")





