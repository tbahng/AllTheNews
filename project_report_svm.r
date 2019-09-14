
rm(list = ls())
############################################################################
# load libraries
# define functions
############################################################################
library(magrittr)
library(caret)
library(ggplot2)
library(reshape2)
library(knitr)
library(corrplot)
library(Cairo)
library(CORElearn)
library(naivebayes)
library(e1071)
library(ggfortify)
library(klaR)
library(tm)
library(wordcloud)


######################################################
# load data
######################################################
load('data/2Transform.rda')
rm(doc_sentiments, dtm_sentiment, trans)


######################################################
# assess the frequency of labels
# choose which labels to sample for modeling
######################################################
table(df$publication) %>% sort(., decreasing = T) %>% head %>%
  kable(., caption = 'Frequency of Classes')
# new york times and breitbart have roughly even number of classes
# from earlier sentiment analysis, the two publications appear to be diametrically opposed in terms of sentiment
# this experiment will try to predict the two classes


######################################################
# subset the dtm to only observations for NYT and Breitbart
######################################################
set.seed(111)
keep.n <- which(df$publication == 'New York Times') %>% sample(., 5000)
keep.b <- which(df$publication == 'Breitbart') %>% sample(., 5000)
keep <- c(keep.n, keep.b)
m <- as.matrix(dtm[keep,])
myLabels <- df$publication[keep] %>% droplevels()


######################################################
# assess for near-zero variance
######################################################
# check for near-zero variance
datNZV <- nearZeroVar(m, saveMetrics = TRUE, allowParallel = TRUE)

# zero-variance predictors
table(datNZV$zeroVar) %>% kable(., col.names = c('zero_var','freq'),
                                caption = 'Zero Variance Assessment of Predictors')

# near-zero variance predictors
table(datNZV$nzv) %>% kable(., col.names = c('nzv','freq'),
                            caption = 'Near-Zero Variance Assessment of Predictors')

# 6 predictors displayed zero variance.
# remove zero variance predictors
m <- m[,which(!datNZV$zeroVar)]


######################################################
# Normalization of features
######################################################
# normalize the matrix by dividing values by their row sums
m <- m / rowSums(m)

######################################################
# scaling
######################################################
m <- scale(m)


######################################################
# chi-squared test for independence on discretized features
######################################################
q <- quantile(m)
cp <- c(round(q[1],2) - 0.01, q[2], q[3], q[4], round(q[5],2) + 0.01)
discFeatures <- apply(m, 2, function(x) cut(x, breaks = cp))
# chi-square test for independence between label and predictors
# results: between lie label and predictors
testChiSq <- apply(discFeatures, 2, function(x) chisq.test(table(myLabels, x))$p.value)
names(testChiSq) <- colnames(discFeatures)

# these features are significant predictors concerning the class label
sigFeatures <- which(testChiSq <= 0.05)



######################################################
# feature ranking using Gain Ratio
######################################################
# get the gain ratio (quality) by variable for Class Label; sort decreasing
quality.gr <- attrEval(label ~ .,
                       data = data.frame(cbind(label = myLabels, m)),
                       estimator = "GainRatio") %>% sort(., decreasing = TRUE)
quality.gr <- quality.gr[quality.gr > 0]

plot(quality.gr, col = 'blue',
     main = "Ordered Gain Ratio")


head(quality.gr) %>% 
  kable(., col.names = 'Gain Ratio', caption = "Top Variables by Gain Ratio")



quantile(quality.gr) %>% 
  kable(., col.names = "Quartiles", caption = "Gain Ratio: Quartiles")


######################################################
# training / test split
######################################################
# 70% of data will be used for training while 30% will be used for test
sampleTrain <- floor(0.7 * nrow(m))
set.seed(123)
train_ind <- sample(seq_len(nrow(m)), size = sampleTrain)

# numeric data for training and test
datTrain <- m[train_ind, ]
datTest <- m[-train_ind, ]

# labels for training and test
labelTrain <- myLabels[train_ind]
labelTest <- myLabels[-train_ind]

barplot(table(labelTrain), col = 'blue', 
        main = 'Class Frequency')



######################################################
# Modeling SVM
######################################################
# tune svm for classifier
# important features based on gain ratio
colKeep <- which(colnames(m) %in% names(quality.gr[1:62]))
datTrainSVM <- datTrain[, colKeep]
datTestSVM <- datTest[, colKeep]
# tuning grid for classifier
myGrid <- data.frame(cost = rep(10^(-3:2), 3), 
                       kernel = c(rep('linear',6), rep('radial', 6), rep('polynomial', 6)),
                       accuracy = NA,
                       prec.1 = NA,
                       recall.1 = NA,
                       prec.2 = NA,
                       recall.2 = NA)

# tuning classifier
for (i in 1:dim(myGrid)[1]) {
  # fit model
  fit <- svm(x = datTrainSVM, y = labelTrain, 
             kernel = myGrid$kernel[i], cost = myGrid$cost[i])
  # predict test set
  preds <- predict(fit, datTestSVM)
  # confusion matrix
  cm <- table(labelTest, preds)
  # accuracy
  myGrid$accuracy[i] <- round(sum(diag(cm)) / sum(cm), 2)
  # precision
  prec <- diag(cm) / colSums(cm)
  myGrid$prec.1[i] <- prec[1]
  myGrid$prec.2[i] <- prec[2]
  # recall
  rec <- diag(cm) / rowSums(cm)
  myGrid$recall.1[i] <- rec[1]
  myGrid$recall.2[i] <- rec[2]
}
# the best classifier used linear kernel with cost of constraints = 10 trained on a subset of 62 features with the highest gain ratio.

# index for best paramaters
i.best <- which.max(rowMeans(myGrid[, 3:7]))

kable(myGrid, caption = "Fig.25 Tuning Grid (Lie)")



# fit final svm lie classifier
fit.svm <- svm(x = datTrainSVM, y = labelTrain, 
               kernel = 'linear', cost = 10)

# prediction of test data
preds <- predict(fit.svm, datTestSVM)
# confusion matrix
cmat.svm <- table(preds, labelTest)
kable(cmat.svm, caption = "Confusion Matrix")
acc.svm <- round(sum(diag(cmat.svm)) / sum(cmat.svm), 2)


myGrid[i.best,] %>% kable(., caption = "Model Performance on Test Set")

