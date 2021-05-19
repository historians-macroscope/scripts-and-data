#install.packages("rvest")
library(rvest)
library(dplyr)
library(magrittr)

base_url <- "http://salem.lib.virginia.edu/"
main_page <- read_html(x = "http://salem.lib.virginia.edu/category/swp.html")	

urls <- main_page %>% 		# feed `main_page` to the next step
  html_nodes("a") %>% 	# get the CSS nodes for a link
  html_attr("href") 	# extract the URLs from the link
links <- main_page %>% 	   # feed `main_page` to the next step
  html_nodes("a") %>%    # get the CSS nodes
  html_text() 		   # extract the text

reports <- data_frame(links = links, urls = paste(base_url,urls, sep=""), stringsAsFactors = FALSE)

View(reports)
reports <- reports %>% slice(10:60)
dir.create("reports")

reports.df <- data.frame() 
# Loop over each row in `reports`
for(i in seq(nrow(reports))) { 
  
  # we're going to loop over each row in 'reports', extracting the entries from the pages and then writing them to file.
  
  text <- read_html(reports$urls[i]) %>% # load the page
    html_nodes(".doc") %>% 			 # isloateisolate the text
    html_text() 					 # get the text
  
  reports.df<-rbind(reports.df,data.frame(text), stringsAsFactors = FALSE)
  
} #end the loop


# str_extract will grab the first match
# reports.df[,1] is necessary so that R knows to search through the first column (the only column) of data

month <- (str_extract(reports.df[,1], 
                      "January|February|March|April|May|June|July|August|September|October|November|December"))

# this will grab the year; we could get days similarly

year <- str_extract(reports.df[,1], "\\d{4}")

# this one was tricky. there are groups within groups. Basically, it says, find two or one digit numbers separated by a point.

id <- str_extract(reports.df[,1], "(([0-9]{2})|([0-9]{1})).(([0-9]{2})|([0-9]{1}))")

# this appends these new columns to our original column

reports.df$year <- year
reports.df$month <- month
reports.df$id <- id  # this is the original case number, not the row number

#get rid of newlines and commas in the text

reports.df$text<-str_replace_all(reports.df$text, 
                                 "[\r\n]", "")

reports.df$text<- str_replace_all(reports.df$text,
                                  ",", "")

install.packages('writexl')
library(writexl)
write_xlsx(reports.df, "all-reports.xlsx")
