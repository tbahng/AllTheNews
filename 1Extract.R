# this file reads the data from 'longform.csv' and performs some exploration and transformation
# results are saved in 'data/1Extract.rda'
# "longform.csv" was originally written by querying a Sqlite .db downloaded from:
# URL: https://components.one/datasets/all-the-news-articles-dataset/
# Sentiment Data extracted from 'data/AFINN-111.txt' (AFINN lexicon)

# references to string cleaning operations
# https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
rm(list = ls())
#################################################################
# load libraries
#################################################################
library(data.table)
library(stringr)
library(magrittr)

#################################################################
# define functions
#################################################################

# function to check vector of strings for:
# non-utf8 encodings and html tags
check_text <- function(x) {
  # checks for html tags, non-utf-8 encodings, and carriage returns/new line characters
  grepl(pattern = '[^ -~] | [<.*?>]', x) | grepl(pattern = '[\r\n]', x)
}

# function to convert non-utf8 text to utf-8 encoding
# function to clean text
clean_text <- function(x) {
  Encoding(x) <- "UTF-8"
  # replace any non UTF-8 with ''
  x <- iconv(x, "UTF-8", "UTF-8",sub='')
  # remove html tags
  x <- gsub("<.*?>", "", x)
  # remove carriage return / new line characters
  x <- gsub("[\r\n]", "", x)
  return(x)
}

# function to apply over vector to convert zero-length string to NA
blankToNA <- function(x) {
  x[x == ''] <- NA
  return(x)
}

#################################################################
# read datasets
#################################################################
# Read longform.csv (original data)
pth <- "data/original_data/longform.csv"
system.time(
  dat <- fread(pth)
)

# Read AFINN lexicon
pth <- 'data/AFINN-111.txt'
affin <- read.table(file = pth, 
                    sep = '\t', 
                    quote = '',
                    stringsAsFactors = FALSE)
#################################################################
# structure of dataset
#################################################################
str(dat)
# 204135 observations of 12 variables; meaning there are 204135 unique news articles
# the variable 'id' is the unique identifier and is of type integer
head(sort(dat$id)); tail(sort(dat$id))
# the variable 'title' is the news article title and is of type character; max character length is 243; min character length is 0
summary(nchar(dat$title))
# the variable 'author' gives the author name respective to the news article and is of type character; max character length is 655; min character length is 0
summary(nchar(dat$author))
# the variable 'date' gives the articles publish date but it is of type character
summary(nchar(dat$date)); summary(nchar(dat$date[dat$date != '']))
head(sort(dat$date)); tail(sort(dat$date))
# the variable 'content' contains the full news article and is of type character; max character length is 294200 and min character length is 0; there are 15443 observations of zero length strings
summary(nchar(dat$content)); sum(dat$content == '')
# the variable 'year' gives the year of the article's publish date and is of type integer; years range from 2000 to 2018; there are 12605 missing values; some years are missing such as 2001 and 2002
sort(unique(dat$year)); sum(is.na(dat$year))
# the variable 'month' gives the month of the article's publish date and is of type integer; similar to 'year' there are 12605 missing values.
sort(unique(dat$month)); sum(is.na(dat$month))
# the variable 'publication' gives the name of the news article source and is of type character; there are 19 unique publications however 1 of them is a zero length string; 7715 observations have no publisher name
unique(dat$publication); sum(dat$publication == '')
# the variable 'category' is of type character and has 11 unique values; one of those values is a zero length string; there are 35422 observations with no category name
unique(dat$category); sum(dat$category == '')
# the variable 'digital' is of type integer and contains values in [0,1,2,3,4] with unknown definition; there are 11020 missing values; this is likely a nominal variable and should be made into factor
sort(unique(dat$digital)); sum(is.na(dat$digital))
# the variable 'section' is of type character with 495 unique values that represent the section of the news the article was published under; there are 129563 values that are zero length strings
length(unique(dat$section)); sum(dat$section == ''); head(sort(unique(dat$section)), 20)
# the variable 'url' is of type character with 98797 unique values that represent the url of the article; there are 105339 values that are zero length strings
length(unique(dat$url)); sum(dat$url == '')

#################################################################
# are there any variables suitable to be used as class labels?
#################################################################
sapply(dat, function(x) length(unique(x)))
# potential class variables are 'publication', 'category', 'digital', and 'section'

#################################################################
# look across variables and count the missing values
#################################################################
sapply(dat, function(x) sum(is.na(x)))
# variables 'year', 'month', and 'digital' contain missing values

#################################################################
# look across variables for duplicated data
#################################################################
sapply(dat, function(x) sum(duplicated(x)))
# all variables with the exception of 'id' contain duplicated values.

#################################################################
# look across variables for incorrect data
#################################################################
# so far, the dataset contains both missing values and zero length strings

# [title] there are appear to be articles without title
summary(nchar(dat$title))

# [title] title variable contains 17482 observations that need to be cleaned for special characters
sum(check_text(dat$title))

# [author] there appear to be articles without an author and some author names are over 243 characters long
summary(nchar(dat$author))

# [author] author variable contains 32401 observations with blanks (i.e. ""). 
sum(dat$author == '')

# [author] author variable contains 247 observations that need to be cleaned for special char
sum(check_text(dat$author))

# [date] there appear to be 12605 articles without a date; zero-length string value which should be made NA for consistency
sum(dat$date == '')

# [date] some date values have a character length of 8 and some have 66
summary(nchar(dat$date[dat$date != '']))

# [content] there are 15443 articles without any content (i.e. blank content); there are 162454 observations of content that need to be cleaned of non-utf-8 characters, carriage return/line breaks, and/or html tags.
sum(dat$content == ''); sum(check_text(dat$content))

# [year] similar to date, there are 12605 articles with missing values for year
sum(is.na(dat$year))

# [month] similar to year, there are 12605 articles with missing values for month
sum(is.na(dat$month))

# [publication] there are 7715 observations that have no publisher name; these are zero length strings that should be made NA for consistency
sum(dat$publication == '')

# [category] this variable contains 35422 zero length strings which should be made NA for consistency
sum(dat$category == '')

# [digital] this variable is of type integer and should probably be changed to nominal (factor); it is uncertain what this variable represents without a data dictionary
class(dat$digital)

# [digital] this variable has 11020 missing values
sum(is.na(dat$digital))

# [section]

#################################################################
# variable transformation
#################################################################
# convert zero-length strings to NA
strVars <- sapply(dat, is.character) %>% names

system.time({
  dat[, (strVars) := lapply(.SD, blankToNA), .SDcols = strVars]
})

# check that blanks (i.e. '') converted to NA.
sapply(dat, function(x) sum(is.na(x)))

# clean character variables 
# replace any non UTF-8 with ''
# remove html tags
# remove carriage return / new line characters

# assess magnitude of unclean text for each character variable
sapply(dat[, strVars, with = FALSE], function(x) sum(check_text(x)))
# based on the assessment the variables 'title', 'author' and 'content' need cleaning
dirtyVars <- c('title','author','content')

system.time({
  dat[, (dirtyVars) := lapply(.SD, clean_text), .SDcols = dirtyVars]
})

# check that character strings are clean
sapply(dat[, dirtyVars, with = FALSE], function(x) sum(check_text(x)))
dat$title[check_text(dat$title)] %>% tail
dat$title[check_text(dat$title)] %>% head
dat$author[check_text(dat$author)] %>% head
dat$author[check_text(dat$author)] %>% tail
# it appears that the remaining "cleaned" text now only includes "valid" characters

# convert character variables to factor
# potential class variables are 'publication', 'category', 'digital', and 'section'
chgVars <- c('publication','category','digital','section')

system.time({
  dat[, (chgVars) := lapply(.SD, as.factor), .SDcols = chgVars]
})

sapply(dat[, chgVars, with = FALSE], levels)

#################################################################
# save cleaned data back to RDA
#################################################################
pth <- 'data/1Extract.rda'

system.time(
  save(
    dat,
    affin,
    file = pth
  )
)