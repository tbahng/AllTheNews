
# this file explores sentiment in news articles
# This section will explore sentiment across the news articles in the repository. Assessment will include trends, word clouds, and overall sentiment by publication.
rm(list = ls())
############################################################################
# load libraries
# define functions
############################################################################
library(magrittr)
library(ggplot2)
library(reshape2)
library(knitr)
library(ggfortify)
library(tm)
library(wordcloud)
library(tidyverse)
library(tidytext)
# function to normalize using min-max transformation
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# function to produce word cloud of year's sentiment
# x is a vector of row indices
getWC <- function(x) {
  myDTM <- dtm_sentiment[x,]
  # tidy document term matrix
  dtm_td <- tidy(myDTM)
  # create sentiment vector using afinn
  dtm_sentiments <- dtm_td %>%
    inner_join(get_sentiments("afinn"), by = c(term = "word"))
  
  # word cloud without stop words
  wordcloud(
    words = dtm_sentiments$term,
    freq = dtm_sentiments$count,
    scale=c(3,0.5), 
    max.words=150, 
    random.order=FALSE, 
    rot.per=0.35, 
    colors=brewer.pal(8, 'Dark2')
  )
}


######################################################
# load data
######################################################
load('data/2Transform.rda')



# Trending will look at the years between 2014 and 2018 given the sufficient number of observations in each of these years.
keep <- which(df$year %in% c(2014,2015,2016,2017,2018))
# raw sentiment by year
dat <- data.frame(year = df$year[keep], sentiment = doc_sentiments$value[keep]) %>%
  group_by(year) %>%
  summarise(value = sum(sentiment, na.rm = TRUE))

ggplot(dat, aes(x = year, y = value, fill = year)) +
  geom_col(color = 'black') +
  ggtitle("Raw Sentiment Over Time")


# For each year, sentiment scores were normalized using Min-Max transformation. These values were then grouped by year and aggregated by the mean.
# normalized average sentiment by year using Min Max
datTrend <- data.frame(year = df$year[keep], sentiment = doc_sentiments$value[keep]) %>% droplevels()
normSentiment <- lapply(2014:2018, 
                        function(x) minMax(datTrend$sentiment[datTrend$year == x]))
names(normSentiment) <- 2014:2018
datTrend2 <- normSentiment %>% 
  purrr::map_df(., ~as.data.frame(.x), .id="col_name")
colnames(datTrend2) <- c('year','sentiment')
datTrend2 <- datTrend2 %>%
  group_by(year) %>%
  summarise(value = mean(sentiment, na.rm = TRUE))
ggplot(datTrend2, aes(x = year, y = value, fill = year)) +
  geom_col(color = 'black') +
  ggtitle("Mean Normalized Sentiment Over Time")
# On average, sentiment increased from 2014 to 2015 and began to slightly decrease from 2016 to 2018.


# Sentiment Word Clouds of Articles: 2014 - 2018
years <- 2014:2018
indexList <- lapply(years, function(x) which(df$year == x))
names(indexList) <- years


# Word Cloud of Sentiment Terms: 2014
getWC(indexList[['2014']])


# Word Cloud of Sentiment Terms: 2015
getWC(indexList[['2015']])


# Word Cloud of Sentiment Terms: 2016
getWC(indexList[['2016']])


# Word Cloud of Sentiment Terms: 2017
getWC(indexList[['2017']])


# Word Cloud of Sentiment Terms: 2018
getWC(indexList[['2018']])


# raw sentiment by publication
dat <- data.frame(publication = df$publication, sentiment = doc_sentiments$value) %>%
  group_by(publication) %>%
  summarise(value = sum(sentiment, na.rm = TRUE))

ggplot(dat, aes(x = publication, y = value, fill = publication)) +
  geom_col(color = 'black') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none") +
  ggtitle("Raw Sentiment by Publication")

# Measured by raw sentiment score, it appears overall that NYT has the highest positive sentiment in their articles. Breitbart and Business Insider have the lowest negative sentiment.


# For each publication, sentiment scores were normalized using Min-Max transformation. These values were then grouped by publication and aggregated by the mean.
# normalized average sentiment by publication using Min Max
datPub <- data.frame(publication = df$publication, 
                     sentiment = doc_sentiments$value) %>% droplevels()
normSentiment <- lapply(levels(datPub$publication), 
                        function(x) minMax(datPub$sentiment[datPub$publication == x]))
names(normSentiment) <- levels(datPub$publication)
datPub2 <- normSentiment %>% 
  purrr::map_df(., ~as.data.frame(.x), .id="col_name")
colnames(datPub2) <- c('publication','sentiment')
datPub2 <- datPub2 %>%
  group_by(publication) %>%
  summarise(value = mean(sentiment, na.rm = TRUE))
ggplot(datPub2, aes(x = publication, y = value, fill = publication)) +
  geom_col(color = 'black') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none") +
  ggtitle("Mean Normalized Sentiment by Publication")

# By normalizing the sentiment scores by publication, it appears **on average** that Atlantic, Breitbart, and Verge have the highest positive sentiment. The lowest negative sentiment on average seems to be from articles by Business Insider, Guardian, and NPR.

