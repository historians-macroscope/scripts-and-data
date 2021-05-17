install.packages("readr") 
# libraries
library(tidyverse)
library(tidytext)
library(readr)
library(stringr)
library(magrittr)
library(tm) 

# get the data
witches  <- read_csv("all-reports.csv")

#do some cleaning up to change the column names and to remove punctuation and numerals from the text. Notice there’s a bit of regex in the str_remove command

witches <- tibble (case = witches$id,
                   text = (str_remove_all(witches$text,
                                          "[0-9]|[[:punct:]]")), 
                   year = witches$year, 
                   month = witches$month)

# add a new colum with the row number in, just to make it easier to keep track of things

witches$id <- row.names(witches)

# turn into tidy format

tidy_witches <- witches %>%
  unnest_tokens(word, text)


# load up the default list of stop_words that comes
# with the tidyverse

data(stop_words)

# remove (anti-join) the stopwords

tidy_witches <- tidy_witches %>%
  anti_join(stop_words)

# this line might take a few moments to run by the way
witch_words <- tidy_witches %>%
  count(id, word, sort = TRUE)

# take a look at what you've just done
# by examining the first few lines of `witches_words`

head(witch_words)
View(witch_words) 

custom_stop_words <- bind_rows(tibble(word = c("aforesaid", 
                                               "court","archives","lord","salem") ,
                                      lexicon = c("custom")), 
                               stop_words) 

# delete custom stopwords from our data

tidy_witches <- tidy_witches %>%
  anti_join(custom_stop_words)

# turn that into a matrix
dtm <- witch_words %>%
  cast_dtm(id, word, n)

w_lda <- LDA(dtm, k = 10) #where k is the number of topics to model
w_lda

w_topics <- tidy(w_lda, matrix = "beta")

w_top_terms <- w_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

w_top_terms %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

witch_words <- tidy_witches %>%
  count(year, word, sort = TRUE)

dtm <- witch_words %>%
  cast_dtm(year, word, n)

w_lda <- LDA(dtm, k = 10)

# turn the topics into tidy format, and then plot them

w_documents <- tidy(w_lda, matrix = "gamma")
w_documents # view the result; notice the column ‘document’ is our original ‘year’ metadata

ggplot(w_documents, aes(x=document, y=gamma, fill=topic)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


a <- Corpus(DirSource("diaries/"),
            readerControl=list(language="lat")) #lat = latin characters

#clean up the text by cleaning up and stemming
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, tolower)
a <- tm_map(a, removeWords, stopwords("english"))
a <- tm_map(a, stemDocument, language = "english")

#create the document term matrix
dtm <-DocumentTermMatrix(a)
dtm <- removeSparseTerms(dtm, 0.75)

# remove any empty rows
# Find the sum of words in each document
# and then remove all docs without words
rowTotals <- apply(dtm , 1, sum) 
dtm.new <- dtm[rowTotals> 0, ] 

library(topicmodels)
d_lda <- LDA(dtm.new, k = 20, control = list(seed = 1234))
