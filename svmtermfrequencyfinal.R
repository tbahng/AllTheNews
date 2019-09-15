library(e1071)
setwd("C:\\Users\\mznid\\Desktop\\WorkingDictionary")
library(tm)
library(tidytext)
library(party)
library(caret)
library(SparseM)
library(ztable)
library(magrittr)
library(foreach)
library(doParallel)

load('IST 707 Project\\2Transform.rda')
object.size(dtmNorm)

memory.limit(16000)

as.sparseMatrix <- function(simple_triplet_matrix_sparse) {
  retval <-  sparseMatrix(i=as.numeric(simple_triplet_matrix_sparse$i),
                          j=as.numeric(simple_triplet_matrix_sparse$j),
                          x=as.numeric(as.character(simple_triplet_matrix_sparse$v)),
                          dims=c(simple_triplet_matrix_sparse$nrow, 
                                 simple_triplet_matrix_sparse$ncol),
                          dimnames = simple_triplet_matrix_sparse$dimnames,
                          giveCsparse = TRUE)
}
length(doc_sentiments$score)
str(df)
publisher <- df$publication
max(as.numeric(doc_sentiments$document))
beforeid <- (df$date <="2016-11-08")
dtm_sentiment
dtmNorm[beforeid,]
df[,1:8]
# m <- as.sparseMatrix(dtmNorm)

str(df)

# CUT OUT COLUMNS

lwdtmNorm <- dtmNorm[,!colnames(dtmNorm) %in% c("reuterscom")]
n <- as.sparseMatrix(lwdtmNorm)




# CLEAN UP

rm("dtm")
rm("df")
# rm("dtmNorm")
rm("dtm_sentiment")
rm("doc_sentiments")
gc()

str(m)
str(n)


# TEST/TRAIN

rounder <- round(length(n@Dimnames$Docs)*0.9)
randomizer <- sample(c(1:length(n@Dimnames$Docs)), replace=FALSE)

test <- n[randomizer[1:rounder],]
train <- n[randomizer[(rounder + 1):length(n@Dimnames$Docs)],]

labeltest <- publisher[randomizer[1:rounder]]
labeltrain <- publisher[randomizer[(rounder + 1):length(publisher)]]

timelabeltest <- beforeid[randomizer[1:rounder]]
timelabeltrain <- beforeid[randomizer[(rounder + 1):length(publisher)]]

str(train)
str(test)

head(n)


# SUBSET BY PUBLICATION

breitbarttrain <- train[labeltrain == "Breitbart",]
breitbarttest <- test[labeltest == "Breitbart",]
breitbarttrainlabel <- as.factor(timelabeltrain[labeltrain == "Breitbart"])
breitbarttestlabel <- as.factor(timelabeltest[labeltest == "Breitbart"])

nyttrain <- train[labeltrain == "New York Times",]
nyttest <- test[labeltest == "New York Times",]
nyttrainlabel <- as.factor(timelabeltrain[labeltrain == "New York Times"])
nyttestlabel <- as.factor(timelabeltest[labeltest == "New York Times"])

reuterstrain <- train[labeltrain == "Reuters",]
reuterstest <- test[labeltest == "Reuters",]
reuterstrainlabel <- as.factor(timelabeltrain[labeltrain == "Reuters"])
reuterstestlabel <- as.factor(timelabeltest[labeltest == "Reuters"])

atlantictrain <- train[labeltrain == "Atlantic",]
atlantictest <- test[labeltest == "Atlantic",]
atlantictrainlabel <- as.factor(timelabeltrain[labeltrain == "Atlantic"])
atlantictestlabel <- as.factor(timelabeltest[labeltest == "Atlantic"])

str(atlantictrain)
# TRUE means BEFORE the election

length(labeltrain)
length(train@Dimnames$Docs)


write.csv(test, "SVMTEST.csv")
#rm("test")

svmmodel <- e1071::svm(x=atlantictrain, y=atlantictrainlabel, cost= 5, kernel = "linear")

testpredictions <- predict(svmmodel, newdata=atlantictest)

svmerror <- data.frame(testpredictions, atlantictestlabel)
svmerror[3] <- svmerror[1] == svmerror[2]
colnames(svmerror) <- c("Predictions", "Actuals", "Correct")

svmtestaccuracy <- sum(svmerror$Correct) / length(svmerror$Correct)
svmconfusion <- table(svmerror$Predictions, svmerror$Actuals)

svmheatmap <- ztable(svmconfusion) %>% makeHeatmap() %>% print(caption="Support Vector Machine Confusion Matrix", family="Helvetica")


summary(svmmodel)



# saveRDS(svmmodel, file = "svmcost10linear4xtrainBEST.rds")


trainpredictions <- predict(svmmodel, newdata=atlantictrain)
svmtrainerror <- data.frame(trainpredictions, atlantictrainlabel)
svmtrainerror[3] <- svmtrainerror[1] == svmtrainerror[2]
colnames(svmtrainerror) <- c("Predictions", "Actuals", "Correct")

svmtrainaccuracy <- sum(svmtrainerror$Correct) / length(svmtrainerror$Correct)
svmtrainconfusion <- table(svmtrainerror$Predictions, svmtrainerror$Actuals)



summary(svmmodel)






















#### Multicore



# registerDoSEQ()

# cl <- makeCluster(2)

numCores <- detectCores()
registerDoParallel(numCores)

foreach (i=1) %dopar% {
  
  multicoresvmmodel1 <- e1071::svm(x=train, y=labeltrain, C= 10, kernel = "linear")
  multicoresvmmodel2 <- e1071::svm(x=train, y=labeltrain, C= 1, kernel = "linear")
  multicoresvmmodel3 <- e1071::svm(x=train, y=labeltrain, C= 0.1, kernel = "linear")
  multicoresvmmodel4 <- e1071::svm(x=train, y=labeltrain, C= 1, kernel = "sigmoid")
  multicoresvmmodel5 <- e1071::svm(x=train, y=labeltrain, C= 1, kernel = "radial")
  
}


saveRDS(multicoresvmmodel1, file = "multicoresvmmodel1.rds")
saveRDS(multicoresvmmodel2, file = "multicoresvmmodel2.rds")
saveRDS(multicoresvmmodel3, file = "multicoresvmmodel3.rds")
saveRDS(multicoresvmmodel4, file = "multicoresvmmodel4.rds")
saveRDS(multicoresvmmodel5, file = "multicoresvmmodel5.rds")
#stopCluster()


