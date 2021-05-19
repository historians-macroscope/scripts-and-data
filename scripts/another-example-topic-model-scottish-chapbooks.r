# Topic Modeling Scottish Chapbooks
# slightly modified version of
# https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html
# by Andreas Niekler, Gregor Wiedemann

library(tidyverse)
library(tidytext)

### getting the data into shape

#we have already downloaded just the textfiles
#from https://data.nls.uk/data/digitised-collections/chapbooks-printed-in-scotland/
#we put them together into a csv
#with the extracted  publication dates from the associated mets files.
# the result is the file 'data/chapbooks-text.csv'
# which is in this repo in the data/chapbooks-text.csv.zip file.
# you'll have to unzip it; it was the only way to get that much data
# onto github!
# change the location in the next line relative
# to your current working directory
# which you can find out with the 
# getwd()
# command

cb  <- read_csv(  "data/chapbooks-text.csv")

#put the data into a tibble (data structure for tidytext)
#but also, strip out all the digits from the text column
cb_df <- tibble(id = cb$line, text = (str_remove_all(cb$text, "[0-9]")), date = cb$date)

#turn cb_df into tidy format
#use View(cb_df) to see the difference
#from the previous table
tidy_cb <- cb_df %>%
  unnest_tokens(word, text)

#the only time filtering happens 
data(stop_words)

tidy_cb <- tidy_cb %>%
  anti_join(stop_words)
###
#tally things up in preparation for making the dtm
diary_words <- tidy_cb %>%
  count(id, word, sort = TRUE)

dtm <- diary_words %>%
  cast_dtm(id, word, n)

# have a look at the number of documents and terms in the matrix
# this should equal the number of documents from dim(cb_df)
# if it doesn't we've lost some rows when we pulled out the stopwords
dim(dtm)

# let's build some topic models!
require(topicmodels)
# number of topics
K <- 15
# set random number generator seed
# for purposes of reproducibility
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

ncol(dtm) # lengthOfVocab

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta) 

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)        

terms(topicModel, 10)

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
topicNames

# select some documents for the purposes of
# sample visualizations
exampleIds <- c(2, 100, 200)

# load libraries for visualization
library("reshape2")
library("ggplot2")
N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

#topic distributions
# see alpha from previous model
attr(topicModel, "alpha") 

# explore how a topic model might change by
# attenuating some of the parameters
topicModel2 <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # reset topicnames

# get topic proportions from example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

#install.packages('lda')
library(lda) #after having install.packages('lda')
#topic ranking
# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")

# What are the most probable topics in the entire collection?
# Approach 1: We sort topics according to their probability within the entire collection:
topicProportions <- colSums(theta) / nrow(dtm)  # mean probablities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order

#Approach 2: We count how often a topic appears as a primary topic within a paragraph This method is also called Rank-1
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nrow(dtm)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)

#topics over time
# append decade information for aggregation
cb$decade <- paste0(substr(cb$date, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = cb$decade), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames

# reshape data frame, for when I get the topics over time thing sorted
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")

# plot topic proportions per deacde as bar plot
require(pals)
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
