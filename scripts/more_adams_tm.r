ja  <- read_csv("https://raw.githubusercontent.com/historians-macroscope/topic-modeling/master/data/johnadams.csv")

ja_df <- tibble(id = ja$id, text = ja$text, date = ja$date)

tidy_ja <- ja_df %>%
  unnest_tokens(word, text)

dtm2 = dtm
rowSum <- apply(dtm2 , 1, sum)
dtm2 <- dtm2[rowSum> 0, ]

K = 20 #number of topics
newmodel <- LDA(dtm2, K, method="Gibbs", 
                control=list(iter = 500, verbose = 25, alpha = 0.2))



