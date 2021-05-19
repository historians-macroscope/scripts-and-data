library(rvest)
library(dplyr)
library(magrittr)

diary1 <- "https://www.masshist.org/digitaladams/archive/doc?id=D1"

html_d <- read_html(diary1)
html_d

#let's look at the nodes
html_d %>%
  html_nodes("div")

html_d %>%
  html_nodes("div.entry")

html_d %>%
  html_nodes("div.dateline span") %>%
  html_attr("title")

entry <- html_d %>%
  html_nodes("div.entry") %>%
  html_text()

id <- html_d %>%
  html_nodes("a") %>%
  html_attr("id")

id <- na.omit(id)

id <- id[-c(1)]

scrape <- tibble(id, entry)
View(scrape)

install.packages("stringr")
  
library(stringr)
#removes the diary metadata from the date by taking everything 
#one row at a time in ‘id’ after the first three characters and
#overwriting it back in place (ie, stripping out the first three
#characters in each row

scrape$id <- substring(scrape$id, 4)

#We can do the same thing again by specifying which characters 
#in ‘id’ represent the month, and writing that information
#to a new variable called ‘month’. We then will add that
#variable as a column in our table ‘scrape’

month <- substring(scrape$id, 5, 6)
scrape['month'] <- month

install.packages("tidytext")
library(tidytext)

text_df <- tibble(line = 1:179, text = scrape$entry, date = scrape$id, month = scrape$month)

tidy_diary <- text_df %>%
  unnest_tokens(word, text)

View(tidy_diary) 

data(stop_words)

View(stop_words)

tidy_diary <- tidy_diary %>%
  anti_join(stop_words)

install.packages("ggplot2")

library(ggplot2)

tidy_diary %>%
  count(word, sort = TRUE) %>% # count the words
  filter(n > 20) %>% # filter for counts over twenty
  mutate(word = reorder(word, n)) %>% # arrange from highest to lowest and then pass the result to the ggplot graphics package
  ggplot(aes(word, n)) + # built a plot of word against count
  geom_col() + # make it a bar chart
  xlab(NULL) + # don’t use an x label
  coord_flip() # flip it so that the x and y axes change places

diary_words <- tidy_diary %>%
  count(date, word, sort = TRUE)



install.packages("tm") #text mining package
library(tm)
dtm <- diary_words %>%
  cast_dtm(date, word, n)


library(topicmodels)
d_lda <- LDA(dtm, k = 4)
d_lda

d_topics <- tidy(d_lda, matrix = "beta")

d_top_terms <- d_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

d_top_terms %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

mystopwords <- tibble(word =
                        c("page","view","illegible","image",
                          "img","mr","said","will"))

diary_words <- anti_join(diary_words, mystopwords,
                         by = "word")

dtm <- diary_words %>%
  cast_dtm(date, word, n)


d_documents <- tidy(d_lda, matrix = "gamma")
d_documents

month <- substring(d_documents$document, 5, 6)
d_documents['month'] <- month

ggplot(d_documents, aes(x=month, y=gamma, fill=topic)) +
  geom_bar(stat = "identity") + ylab("proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#example using a 'seed' value to enable reproducibility:

d_lda <- LDA(dtm, k = 4, control = list(seed = 1234))
