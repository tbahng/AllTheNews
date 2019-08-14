# this file checks a body of text (i.e. a report) for words that shouldn't belong in the text
# All Assignments must be written in the third person (no use of I, you, we, us, etc)
rm(list = ls())
library(tm)
library(magrittr)
#setwd('C:/Users/ke392d/Desktop/Master/LTP_Learning Together Program/LTP - Masters Data Science/ist707')
# Function to parse a text string into n-word combinations
parse_words <- function(x, n = 3) {
  require(tau)
  require(magrittr)
  textcnt(x, n = n, method = 'string') %>% unclass %>% as.data.frame %>% rownames %>% return
}
# read data
myFile <- choose.files()
#myText <- readLines('blah.txt') %>% tolower
myText <- qdapTools::read_docx(myFile)
myWords <- parse_words(myText, n = 1)
badWords <- c('i','you','we','us')
# how many bad words are in my text?
table(badWords %in% myWords)
# which bad words are in my text?
badWords[badWords %in% myWords]
# which lines in my text contain badwords?
myTextList <- lapply(myText, parse_words, n = 1)
sapply(myTextList, function(x) any(badWords %in% x)) %>% which

