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
