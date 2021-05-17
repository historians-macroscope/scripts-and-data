# Salem Witchcraft Papers
# http://salem.lib.virginia.edu/swp-intro.html


library(rvest)
library(dplyr)
library(magrittr)

base_url <- "http://salem.lib.virginia.edu/"
main_page <- read_html(x = "http://salem.lib.virginia.edu/category/swp.html")	

urls <- main_page %>% # feed `main.page` to the next step
  html_nodes("a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

links <- main_page %>% 		# feed `main.page` to the next step
  html_nodes("a") %>% 	# get the CSS nodes
  html_text() 		# extract the text

reports <- data.frame(links = links, urls = paste(base_url,urls, sep=""), stringsAsFactors = FALSE)
View(reports)

reports <- reports %>% slice(10:150)

dir.create("reports")

#---
# Loop over each row in `reports`
#---

for(i in seq(nrow(reports))) { 
  
  # we're going to loop over each row in 'reports', extracting the entries from the pages and then writing them to file.
  
  text <- read_html(reports$urls[i]) %>% # load the page
    html_nodes(".doc") %>% # isloate the text
    html_text() # get the text
  
  # Create the file name
  filename <- paste0("reports/", reports$links[i], ".txt") 
  
  #this uses the relevant link text as the file name 
  
  sink(file = filename) %>% # open file to write
    cat(text) # write the file
  sink() # close the file
} #end the loop

