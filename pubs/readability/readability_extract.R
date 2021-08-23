#Libraries
library(tidyverse)
library(quanteda.textstats) 
##################################
#Variables
fulltext_path = "C:/Users/tbogo/Desktop/fulltext" # path to full text documents

abstract_path <- "~/Coding/R/sysconf/data/abstract" # path to abstract documents

path_out <- "readability.csv"

readability_metrics <- c("SMOG", "Flesch.Kincaid") # Readability metrics, see: quanteda.textstat_readability for full options


##################################
#Fulltext extract and clean

fulltext <- tibble(document = list.files(fulltext_path, pattern = "*[0-9].txt")) # Creates tibble of document file names

fulltext$text <- paste(fulltext_path, "/", fulltext$document, sep="") %>% #adds file path prefix to file names
  sapply(read_file) # reads in files

fulltext <- left_join(fulltext, fulltext$text %>% # adding readability scores to fulltext
                  textstat_readability(measure = readability_metrics) %>% # Gets readability scores
                  mutate(document = sub(paste(fulltext_path, "/", sep = ""), "", document)) #cleans document path for merging
) %>% # end merge
  #extract features
  mutate(confrence = flatten_chr(map(str_split(document, "[_]"), 1)), # adds conference
         num_words = lengths(strsplit(text, "\\W+"))) %>% #extracts num words
  subset(select=-text) #removes full text to limit the size of the file

fulltext$confrence = as_factor(unlist(fulltext$confrence)) # converts conferences to factor type

##################################
#Abstract extract and clean
abstracts <- tibble(document = list.files(abstract_path, pattern = "*[0-9].txt")) # Creates tibble of document file names

abstracts$text <- paste(abstract_path, "/", abstracts$document, sep="") %>% #adds file path prefix to file names
  sapply(read_file) 

abstracts <- left_join(abstracts, abstracts$text %>%
                         textstat_readability(measure = readability_metrics) %>%
                         mutate(document = sub(paste(abstract_path, "/", sep = ""), "", document)) #cleans document path for merging
) %>%
  mutate(num_words = lengths(strsplit(text, "\\W+"))) %>% #extracts num words
  rename_with(~ paste("abstract.", .x, sep="")) %>% # prepends "abstract." to col names
  rename(document = abstract.document, # renamed for merging with fulltext
         abstract = abstract.text)  

##################################
#Join fulltext with abstract data then write out 

left_join(fulltext, abstracts) %>%
  write_csv(path_out)

