# Adams' Diaries

library(rvest)
library(dplyr)
library(magrittr)

base_url <- "https://www.masshist.org"
main_page <- read_html(x = "https://www.masshist.org/digitaladams/archive/browse/diaries_by_date.php")	

urls <- main_page %>% # feed `main.page` to the next step
  html_nodes("a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

links <- main_page %>% 		# feed `main.page` to the next step
  html_nodes("a") %>% 	# get the CSS nodes
  html_text() 		# extract the text

diaries <- data.frame(links = links, urls = paste(base_url,urls, sep=""), stringsAsFactors = FALSE)

View(diaries)

diaries <- diaries %>% slice(9:59)

dir.create("diaries")

#---
# Loop over each row in `diaries`
for(i in seq(nrow(diaries))) { 
  
  # we're going to loop over each row in 'diaries', extracting the entries from the pages and then writing them to file.
  
  text <- read_html(diaries$urls[i]) %>% # load the page
    html_nodes(".entry") %>% # isloate the text
    html_text() # get the text
  
  # Create the file name
  filename <- paste0("diaries/", diaries$links[i], ".txt") 
  
  #this uses the relevant link text as the file name 
  
  sink(file = filename) # open file to write
  cat(text) # write the file
  sink() # close the file
} #end the loop

