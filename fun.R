###############################
# IST707 Project              #
# Group 1                     #
# Thomas Bahng, Ted Tinker,   #
# Tim Zalk, Michael Znidarsic #
#                             #
# 1Extract.R                  #
###############################

# Functions for other scripts in AllTheNews project


# function to check vector of strings for:
# non-utf8 encodings and html tags
check_text <- function(x) {
  # checks for html tags, non-utf-8 encodings, and carriage returns/new line characters
  grepl(pattern = '[^ -~] | [<.*?>]', x) | grepl(pattern = '[\r\n]', x)
}

## This is causing trouble with tolower() during tm_map process.
# function to convert non-utf8 text to utf-8 encoding
# function to clean text
# clean_text <- function(x) {
#   Encoding(x) <- "UTF-8"
#   # replace any non UTF-8 with ''
#   x <- iconv(x, "UTF-8", "UTF-8",sub='')
#   # remove html tags
#   x <- gsub("<.*?>", "", x)
#   # remove carriage return / new line characters
#   x <- gsub("[\r\n]", "", x)
#   return(x)
# }

# function to convert utf8 text to latin1 encoding
# function to clean text
clean_text <- function(x) {
  Encoding(x) <- "UTF-8"
  # replace any UTF-8 with latin1 else ''
  x <- iconv(x, "UTF-8", "latin1", sub='')
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
