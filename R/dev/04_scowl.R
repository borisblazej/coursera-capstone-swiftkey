# Task: load SCOWL words into vector
# Output: scowl

# 1. List of relevant SCOWL file names

all_scowl_files <- list.files(path = "./scowl/final")

scowl_files <- all_scowl_files[-which(str_detect(all_scowl_files, "name"))]

scowl <- sapply(scowl_files, function(f) {
    con <- file(paste0("./scowl/final/",f))
    content <- readLines(con)
    close(con)
    return(content)
}) %>% 
    unlist(use.names = FALSE)

scowl_clean <- str_to_lower(scowl)

write_rds(scowl_clean, "./data/scowl_clean.rds.gz", compress = "gz")

