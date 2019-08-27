###############################
# IST707 Project              #
# Group 1                     #
# Thomas Bahng, Ted Tinker,   #
# Tim Zalk, Michael Znidarsic #
#                             #
# 1Extract.R                  #
###############################

# This file performs additional transformation to the dataset saved in 'data/1Extract.rda'
# primary transformations will be to:
## subset the data to relevant observations
## convert data to a document term matrix
## convert data to a transactions dataset for association rule mining
# Results are saved to 'data/2Transform.rda'

rm(list = ls())

#################################################################
# load libraries
#################################################################

library(tm)
library(slam)
library(magrittr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(arules)
library(viridis)
library(tidytext)

#################################################################
# load functions
#################################################################

source("fun.R")

#################################################################
# load data
#################################################################

load('data/1Extract.rda')
# convert data to data.frame
dat <- as.data.frame(dat)

#################################################################
# subset data - exclude variables that will not be used in analysis
#################################################################

# these variables will not be used in any analysis
exclVars <- c('url')
dat <- dat[, !names(dat) %in% exclVars]

#################################################################
# Assess Missing Values
#################################################################

# Total missing values by variable
data.frame(
  varName = factor(names(dat), levels = names(dat)),
  totalNA = unname(sapply(dat, function(x) sum(is.na(x))))
) %>% ggplot(., aes(x = varName, y = totalNA)) +
  geom_col() + coord_flip() +
  theme_bw() + 
  ggtitle("Missing Values by Variable")

# Total missing values by variable grouped by Publication
pubs <- levels(dat$publication)
vars <- names(dat)[names(dat) != 'publication']
m <- matrix(nrow = length(pubs), ncol = length(vars))
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    vec <- is.na(dat[which(dat$publication == pubs[i]), vars[j]])
    m[i,j] <- sum(vec) / length(vec)
  }
}

noPub <- sapply(dat[which(is.na(dat$publication)), vars],
                 function(x) sum(is.na(x)) / length(x)) %>% unname
m <- rbind(m, noPub)
dfNA <- as.data.frame(m)
colnames(dfNA) <- vars
dfNA$publication = c(pubs, 'Unspecified')

dfNA <- melt(dfNA, id = 'publication')
# it appears that observations where the publication is missing contain mostly missing values across most of the variables.
ggplot(dfNA, aes(x = variable, y = publication)) +
  geom_tile(aes(fill = value), color = 'black') +
#  scale_fill_gradientn(colors = c('darkblue','orange')) +
  scale_fill_viridis(option = 'inferno', direction = 1, begin = 0.1, end = 0.9) +
  labs(fill = '% NA') + 
  ggtitle("Percent Missing Values by Variable")
# 11 publications have 100% missing section
# the variable 'section' might be good to remove altogether due to missing values.
#################################################################
# subset data
# subset the data down to observations with complete cases in specified variables
#################################################################
# subset the data down to observations with complete cases in specified variables
impVars <- c('publication','content','date','year','month','category','digital')
df <- dat[complete.cases(dat[,impVars]),] %>% droplevels()

# re-assess percent missing values by variable
pubs <- levels(df$publication)
vars <- names(df)[names(df) != 'publication']
m <- matrix(nrow = length(pubs), ncol = length(vars))
for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    vec <- is.na(df[which(df$publication == pubs[i]), vars[j]])
    m[i,j] <- sum(vec) / length(vec)
  }
}

dfNA <- as.data.frame(m)
colnames(dfNA) <- vars
dfNA$publication = pubs

dfNA <- melt(dfNA, id = 'publication')
ggplot(dfNA, aes(x = variable, y = publication)) +
  geom_tile(aes(fill = value), color = 'black') +
#  scale_fill_gradientn(colors = c('darkblue','orange')) +
  scale_fill_viridis(option = 'inferno', direction = 1, begin = 0.1, end = 0.9) +
  labs(fill = '% NA') + 
  ggtitle("Percent Missing Values by Variable")

# Dataset is now cleaned to contain complete cases in relevant variables
# 'publication','content','date','year','month','category','digital'

# is there enough data by target variable 'publication' for supervised learning?
# some publications contain more samples; all except 'Verge' have over 2000 observations
summary(df$publication)
# is there sufficient data by year?
# it appears that years with sufficient samples begin in 2014 through 2018
summary(df$year)
#################################################################
# transform data & subset data
# combine title and content as they are more useful as one string element than separate
#################################################################
# combine title and content into one variable called 'fullText'
df$fullText <- ifelse(!is.na(df$title), paste(df$title, df$content), df$content)
# remove 'title' and content' from data.frame
exclVars <- c('title','content')
df <- df[, !names(df) %in% exclVars]

# assess the distribution of character counts in 'fullText'
summary(nchar(df$fullText))

#################################################################
# subset data
# remove the variable 'section' given prevalence of missing values
# subset data.frame (df) down to only the most prolific authors across the 18 publications
#################################################################
# remove 'section' from data.frame
exclVars <- c('section')
df <- df[, !names(df) %in% exclVars]
# remove observations where no author is mentioned
impVars <- c('author')
df <- df[complete.cases(df[,impVars]),] %>% droplevels()

#################################################################
# sample the data
# subset data down to only the most prolific authors across the 18 publications
# for each publication keep just the top 80% of authors by number of articles
# this will help to ensure the data only keeps the "main voices" of each publication
#################################################################
# assess the frequency of most prolific authors overall
# it appears that some author names are not real names (e.g. "Breitbart News", "Associated Press", "NPR Staff", etc.)
head(sort(table(df$author), decreasing = TRUE))
# assess the count of unique authors by publication
length(unique(df$author))
tapply(df$author, df$publication, function(x) length(unique(x)))

top4Pubs <- table(df$publication) %>% sort(., decreasing = TRUE) %>% 
  head(4) %>% names

# distributions of number of articles by author faceted by top 4 publications
# vast majority of authors in each publication don't write as many articles
par(mfrow = c(2,2))
sapply(top4Pubs, plot_hist_author) %>% invisible
par(mfrow = c(1,1))

# for each publication, subset the data to only the authors that make up top 80% of observations
rownames(df) <- NULL
pubs <- levels(df$publication)
keepRowsList <- lapply(pubs, function(x) which(df$author %in% get_top_authors(x)))
keepRows <- do.call('c', keepRowsList)
df <- df[keepRows,] %>% droplevels()

#################################################################
# create document term matrix
# Because sentiment analysis will be performed, stopwords will not include affin lexicon
# Stopwords will include explicit mention of publication
# Week 9 Text Mining states that stemming can be useful for topic-oriented classification (i.e. section)
# Because stemming will inherently affect sentiment analysis, two document term matrices will be created
# One will be stemmed and the other not stemmed
# the document term matrix will be filtered for terms with count frequencies greater than a specified threshold
#################################################################

## Created Document Term Matrix WITH STEMMING
# start the clock
ptm <- proc.time()
# create an object inheriting from VectorSource; required for Corpus
words.vec <- VectorSource(df$fullText)
# define a corpora; This will be used as we go further along text mining
system.time(
  words.corpus <- Corpus(words.vec)
)

# transform the corpus
pubWords <- levels(dat$publication) %>%
  strsplit(., split = ' ') %>% unlist %>% tolower() %>% unique
skipWords <- c(pubWords, stopwords('en'),
               stopwords('en')[!stopwords('en') %in% affin$V1])
# remove punctuation
words.corpus <- tm_map(words.corpus, removePunctuation)
# remove numbers
words.corpus <- tm_map(words.corpus, removeNumbers)
# convert text to lower case
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
# remove stop words
words.corpus <- tm_map(words.corpus, removeWords, skipWords)
# remove white space
words.corpus <- tm_map(words.corpus, stripWhitespace)
# apply stemming
words.corpus <- tm_map(words.corpus, stemDocument, language = 'english')

# construct a document term matrix
dtm <- DocumentTermMatrix(words.corpus)
# time elapsed
proc.time() - ptm

# assess the term frequency
termTotals <- col_sums(dtm)

# plot top six terms
head(sort(termTotals, decreasing = T)) %>% barplot(., main = 'Top 6 Terms')

# 75% of the terms have less than frequency count of 4
# given performance constraints in R, reduce size of DTM
# only include terms with termTotal greater than 99.2th percentile (i.e. ~ 2000)
quantile(termTotals)
(topPerc <- quantile(termTotals, probs = seq(.98,1, 0.001))) # percentiles
sapply(
  topPerc,
  function(x) sum(termTotals > x)
) # remaining terms at each percentile

# filter the document term matrix for terms greater than frequency count of 2000
minTermFreq <- 2000
dtm <- dtm[, which(termTotals > minTermFreq)]

#################################################################
## Created Document Term Matrix WITHOUT STEMMING
#################################################################
# start the clock
ptm <- proc.time()
# create an object inheriting from VectorSource; required for Corpus
words.vec <- VectorSource(df$fullText)
# define a corpora; This will be used as we go further along text mining
system.time(
  words.corpus <- Corpus(words.vec)
)

# remove punctuation
words.corpus <- tm_map(words.corpus, removePunctuation)
# remove numbers
words.corpus <- tm_map(words.corpus, removeNumbers)
# convert text to lower case
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
# remove stop words
words.corpus <- tm_map(words.corpus, removeWords, skipWords)
# remove white space
words.corpus <- tm_map(words.corpus, stripWhitespace)

# construct a document term matrix
dtm_sentiment <- DocumentTermMatrix(words.corpus)
# time elapsed
proc.time() - ptm

#################################################################
# Begin Sentiment Analysis
#################################################################

# tidy document term matrix
dtm_td <- tidy(dtm_sentiment)
# create sentiment vector using afinn
dtm_sentiments <- dtm_td %>%
  left_join(get_sentiments("afinn"), by = c(term = "word"))
doc_sentiments <- dtm_sentiments %>%
  group_by(document) %>%
  summarise(score = sum(score, na.rm = TRUE)) %>% as.data.frame
doc_sentiments <- doc_sentiments %>% .[order(as.numeric(.$document)),]

# plot of top words contributing to positive/negative sentiment
# using bing sentiment
dtm_sentiments <- dtm_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
dtm_sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 16000) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment") +
  ggtitle("Top Terms By Sentiment")


############################################
# Create item matrix (i.e. transactions data set)
# for association rule mining
# subset to just one year [2018]
# filter out terms that are not 
############################################
# drop any unused levels from data.frame
df <- droplevels(df)
# subset data to just one year 2018
dfArm <- df[which(df$year == 2018),] %>% droplevels()
# create a mini-document term matrix to process the fullText
# read the full corpus of text
corp <- Corpus(VectorSource(dfArm$fullText))
# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
ndocs <- length(corp)
minTermFreq <- ndocs * 0.01
# ignore overly common words i.e. terms that appear in more than 100% of the documents
maxTermFreq <- ndocs * 1
# create document term matrix; only include word lengths 3-15 characters
dtm2 <- DocumentTermMatrix(corp,
                          control = list(
                            stopwords = skipWords, 
                            removePunctuation = TRUE,
                            removeNumbers = TRUE,
                            tolower = TRUE,
                            stripWhitespace = TRUE,
                            bounds = list(global = c(minTermFreq, maxTermFreq)),
                            wordLengths=c(3, 15)
                          ))

# assess the term frequency
termTotals <- col_sums(dtm2)

# plot top six terms
head(sort(termTotals, decreasing = T)) %>% barplot(., main = 'Top 6 Terms')

# 75% of the terms have less than frequency count of 271
quantile(termTotals)

# for each document get all the terms used
allTerms <- dtm2$dimnames$Terms
# get combined terms by document
m <- as.matrix(dtm2)
termVec <- apply(m, 1, function(x) paste(allTerms[which(x > 0)], collapse = ' '))

# convert dataframe to transactions dataset
trans <- as(strsplit(termVec, " "), "transactions")

# plot item frequency
# relative item frequency plot
itemFrequencyPlot(trans, topN = 20,
                  main = 'Relative Item Frequency Plot')

# absolute item frequency plot
itemFrequencyPlot(trans, type = 'absolute', topN = 20,
                  main = 'Absolute Item Frequency Plot')
############################################
# save data
############################################
save(
  df, # transformed original dataset
  dtm, # document term matrix
  trans, # transactions dataset
  doc_sentiments, # sentiment score by document
  file = "data/2Transform.rda"
)
