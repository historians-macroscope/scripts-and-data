## data

Some data files.

### Chapbooks from the National Library of Scotland

Chapbooks are from [The National Library of Scotland](https://data.nls.uk/data/digitised-collections/chapbooks-printed-in-scotland/). SG downloaded the full dataset, and used [this script](https://unix.stackexchange.com/questions/317925/search-all-xml-files-recursively-in-directory-for-a-specific-tag-and-grep-the-ta#317947) to parse the xml files for the publication dates. He then used regular expressions to pare away the xml so that he ended up with a list of file names and dates; he joined this data with the chapbook inventory. He then used a quick R script by Ben Marwick to read the text files into R

```R
library(tidyverse)
library(here)

file_names <- dir(here("nls-text-chapbooks/"), full.names = TRUE)

# but the source text has lots of carriage returns in it.
# i removed these in terminal with
# perl -p -i -e 's/\R//g;' *.txt

full_text_tbl <- data.frame(file_name = file_names,
                        full_text = map_chr(file_names,
                                            read_lines))

metadata <- read.csv("nls-metadata/chapbooks-inventory.csv")

df <- data.frame(line = 1:3080, title = metadata$title, text = full_text_tbl$full_text, date = metadata$year, certainty = metadata$certainty)

write.csv(df, "nls-metadata/chapbooks-text.csv")
```
to create a single CSV file with the chapbook OCR'd text, arranged 'title, text, data, certainty' (because some files the metadata wasn't sure when the chapbook was printed.
