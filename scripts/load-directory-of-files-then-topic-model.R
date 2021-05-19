library(tm)

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
