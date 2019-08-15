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
  scale_fill_gradientn(colors = c('darkblue','orange')) +
  labs(fill = '% NA') + 
  ggtitle("Percent Missing Values by Variable")

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
  scale_fill_gradientn(colors = c('darkblue','orange')) +
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
# create document term matrix
# Because sentiment analysis will be performed, stopwords will not include affin lexicon
# Stopwords will include explicit mention of publication
# Week 9 Text Mining states that stemming can be useful for topic-oriented classification (i.e. section)
# the document term matrix will be filtered for terms with count frequencies greater than a specified threshold
#################################################################
# start the clock
ptm <- proc.time()
# create an object inheriting from VectorSource; required for Corpus
words.vec <- VectorSource(df$fullText)
# define a corpora; This will be used as we go further along text mining
system.time(
  words.corpus <- Corpus(words.vec)
)

# transform the corpus
skipWords <- c(levels(dat$publication), stopwords('en'),
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

############################################
# Create item matrix (i.e. transactions data set)
# for association rule mining
############################################
# drop any unused levels from data.frame
df <- droplevels(df)
# subset data for only factor variables
i <- sapply(df, is.factor)
dfArm <- df[, i]

# select some top highly meaningful, topical terms from document term matrix
# 'trump', 'nation', 'clinton', 'world', 'compani', 'republican', 'campaign', 'elect',
# 'democrat', 'obama', 'vote', and 'school'
head(sort(termTotals, decreasing = T), 100)
keepCols <- c('trump', 'nation', 'clinton', 'world', 'compani', 'republican', 
              'campaign', 'elect','democrat', 'obama', 'vote', 'school')
tmp <- as.data.frame.matrix(dtm[, intersect(colnames(dtm), keepCols)])
tmp <- tmp %>% 
  mutate_all(., discretize, method = 'frequency')

# bind original factors with discretized term variables
dfArm <- cbind(dfArm, tmp) %>% droplevels()
# convert dataframe to transactions dataset
trans <- as(dfArm, "transactions")

############################################
# save data
############################################
save(
  df, # transformed original dataset
  dtm, # document term matrix
  trans, # transactions dataset
  file = "data/2Transform.rda"
)