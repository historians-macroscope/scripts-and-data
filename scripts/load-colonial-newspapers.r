library(readr)
cnd  <- read_csv("https://raw.githubusercontent.com/shawngraham/exercise/gh-pages/CND.csv")
papers_df <- tibble(id = cnd$`Article ID`, 
                    text = cnd$Text, year = cnd$Year)

tidy_papers <- papers_df %>%
  unnest_tokens(word, text)
