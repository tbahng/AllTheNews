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
library(slam)

load('IST 707 Project\\2Transform.rda')
str(df)
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

df[,1:8]

dtmsums <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)
normalizedsentiment <- doc_sentiments$score / as.vector(dtmsums[,1])

summer <- aggregate(data.frame(as.matrix(dtm)), FUN= mean, by=list(df$publication))

vocabdf <- data.frame(publisher= summer$Group.1, vocab = 
                        c(sqrt(var(as.vector(as.numeric(summer[1,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[2,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[3,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[4,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[5,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[6,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[7,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[8,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[9,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[10,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[11,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[12,-1]))))
                          ,sqrt(var(as.vector(as.numeric(summer[13,-1]))))
                        )
                      
)



aggregatenormsentiment <- c(
  mean(normalizedsentiment[df$publication == "Atlantic"])
  ,mean(normalizedsentiment[df$publication == "Breitbart"])
  ,mean(normalizedsentiment[df$publication == "Business Insider"])
  ,mean(normalizedsentiment[df$publication == "Buzzfeed News"])
  ,mean(normalizedsentiment[df$publication == "CNN"])
  ,mean(normalizedsentiment[df$publication == "Fox News"])
  ,mean(normalizedsentiment[df$publication == "Guardian"])
  ,mean(normalizedsentiment[df$publication == "Los Angeles Times"])
  ,mean(normalizedsentiment[df$publication == "New York Times"])
  ,mean(normalizedsentiment[df$publication == "NPR"])
  ,mean(normalizedsentiment[df$publication == "Reuters"])
  ,mean(normalizedsentiment[df$publication == "Verge"])
  ,mean(normalizedsentiment[df$publication == "Washington Post"])
)


str(summer)

publisher <- df$publication

beforeid <- (df$date <="2016-11-08")

inputs <- data.frame(sentiment= doc_sentiments$score, length = as.vector(dtmsums[,1]), beforeelection = beforeid)
str(inputs)


hist(doc_sentiments$score)
# m <- as.sparseMatrix(dtmNorm)


mean(normalizedsentiment[df$publication == "Breitbart" & inputs$beforeelection == TRUE])
mean(normalizedsentiment[df$publication == "New York Times" & inputs$beforeelection == TRUE])
mean(normalizedsentiment[df$publication == "Reuters" & inputs$beforeelection == TRUE])

mean(normalizedsentiment[df$publication == "Breitbart" & inputs$beforeelection == FALSE])
mean(normalizedsentiment[df$publication == "New York Times" & inputs$beforeelection == FALSE])
mean(normalizedsentiment[df$publication == "Reuters" & inputs$beforeelection == FALSE])


library(ggplot2)
library(reshape2)
graph <- data.frame(Publication = c("Breitbart","New York Times","Reuters","Breitbart","New York Times","Reuters"), NormalizedSentiment =  c(mean(normalizedsentiment[df$publication == "Breitbart" & inputs$beforeelection == TRUE]),mean(normalizedsentiment[df$publication == "New York Times" & inputs$beforeelection == TRUE]),mean(normalizedsentiment[df$publication == "Reuters" & inputs$beforeelection == TRUE]),mean(normalizedsentiment[df$publication == "Breitbart" & inputs$beforeelection == FALSE]),mean(normalizedsentiment[df$publication == "New York Times" & inputs$beforeelection == FALSE]),mean(normalizedsentiment[df$publication == "Reuters" & inputs$beforeelection == FALSE]))   ,   Election = c("Before","Before","Before","After","After","After"))

meltedgraph <- melt(graph, id.vars = c("Publication", "Election"))
meltedgraph$Election <- factor(meltedgraph$Election, levels = c("Before", "After"))
meltedgraph$Publication <- factor(meltedgraph$Publication, levels = c("Breitbart", "Reuters", "New York Times"))


groupedbar <- ggplot(meltedgraph, aes(Election, value)) + geom_bar(aes(fill = Publication), width = 0.5, position = position_dodge(width = 0.6), stat="identity") + theme(legend.position="right", legend.title = element_blank(),axis.title.x=element_text(),axis.title.y=element_text()) + xlab("2016 Election") + ylab("Normalized Sentiment")





warnings()



graph2 <- data.frame(Publication = df$publication, Sentiment = normalizedsentiment)

graph2complete <- aggregate(graph2, FUN=mean, by=list(graph2$Publication))[,c(1,3)]
colnames(graph2complete) <- c("Publication", "Normalized Sentiment")

meltedgraph <- melt(graph2complete, id.vars = "Publication")

bar2 <- ggplot(meltedgraph, aes(x= Publication, y = value)) + geom_bar(aes(fill = Publication), width = 0.7,  stat="identity") + theme(legend.position="right", legend.title = element_blank(),axis.title.x=element_text(),axis.title.y=element_text()) + ylab("Normalized Sentiment")





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



# CONVERT BEFORE/AFTER TO NUMERIC

inputs$beforeelection <- as.numeric(inputs$beforeelection)

# TEST/TRAIN

rounder <- round(length(inputs$length)*0.01)
randomizer <- sample(c(1:length(inputs$length)), replace=FALSE)

test <- inputs[randomizer[1:rounder],]
train <- inputs[randomizer[(rounder + 1):length(inputs$length)],]

labeltest <- publisher[randomizer[1:rounder]]
labeltrain <- publisher[randomizer[(rounder + 1):length(publisher)]]

timelabeltest <- beforeid[randomizer[1:rounder]]
timelabeltrain <- beforeid[randomizer[(rounder + 1):length(beforeid)]]

str(train)
str(test)

head(n)




# SUBSET TO ATLANTIC REUTERS AND BREITBART

train <- train[labeltrain %in% c("Breitbart", "New York Times", "Reuters"),]
test <- test[labeltest %in% c("Breitbart", "New York Times", "Reuters"),]
labeltrain <- as.factor(as.character(labeltrain[labeltrain %in% c("Breitbart", "New York Times", "Reuters")]))
labeltest <- as.factor(as.character(labeltest[labeltest %in% c("Breitbart", "New York Times", "Reuters")]))



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

svmmodel <- e1071::svm(x=train, y=labeltrain, cost= 10, kernel = "sigmoid")

testpredictions <- predict(svmmodel, newdata=test)

svmerror <- data.frame(testpredictions, labeltest)
svmerror[3] <- svmerror[1] == svmerror[2]
colnames(svmerror) <- c("Predictions", "Actuals", "Correct")

svmtestaccuracy <- sum(svmerror$Correct) / length(svmerror$Correct)
svmconfusion <- table(as.factor(as.character(svmerror$Predictions)), as.factor(as.character(svmerror$Actuals)))

svmheatmap <- ztable(svmconfusion) %>% makeHeatmap() %>% print(caption="Support Vector Machine Confusion Matrix", family="Helvetica")


summary(svmmodel)



# saveRDS(svmmodel, file = "RADIALSVMCOST102ndproblem.rds")


trainpredictions <- predict(svmmodel, newdata=train)
svmtrainerror <- data.frame(trainpredictions, labeltrain)
svmtrainerror[3] <- svmtrainerror[1] == svmtrainerror[2]
colnames(svmtrainerror) <- c("Predictions", "Actuals", "Correct")

svmtrainaccuracy <- sum(svmtrainerror$Correct) / length(svmtrainerror$Correct)
svmtrainconfusion <- table(svmtrainerror$Predictions, svmtrainerror$Actuals)



str(df)



#KMEANS


library(factoextra)


aggregatetrain <- aggregate(data.frame(train), FUN=mean, by= list(labeltrain))
rownames(aggregatetrain) <- aggregatetrain[,1]
aggregatetrain <- aggregatetrain[,-1]
aggregatetrain[,3] <- vocabdf[,2]



k <- 2
set.seed(100)
sentimentkmeansmodel <- kmeans(aggregatetrain[,-3], k)

str(train)
fviz_cluster(sentimentkmeansmodel, data=aggregatetrain[,-3], axes = c(1,3), xlab = "Sentiment", ylab = "Article Length")


sentimentkmeansmodel$cluster



str(sentimentkmeansmodel$cluster)
names(sentimentkmeansmodel$cluster)
rownames(train)

table(sentimentkmeansmodel$cluster, factors(labeltrain))

mean(train[sentimentkmeansmodel$cluster == 1,3])
mean(train[sentimentkmeansmodel$cluster == 2,3])
mean(train[sentimentkmeansmodel$cluster == 3,3])


library(rgl)

k <- 2
set.seed(100)
sentimentvocabkmeansmodel <- kmeans(aggregatetrain, k)

plot3d(aggregatetrain, xlab= "Sentiment", ylab = "Article Length", zlab = "Normalized Vocabulary Standard Deviation" , col=sentimentvocabkmeansmodel$cluster, size= 9)
text3d(aggregatetrain$sentiment,aggregatetrain$length,aggregatetrain$beforeelection,  rownames(aggregatetrain))

str(aggregatetrain)



normaggregatetrain <- aggregatetrain
normaggregatetrain[,1] <- aggregatenormsentiment
aggregatetrain$sentiment
k <- 2
set.seed(100)
normsentimentvocabkmeansmodel <- kmeans(normaggregatetrain, k)


plot3d(normaggregatetrain, xlab= "Normalized Sentiment", ylab = "Article Length", zlab = "Normalized Vocabulary Standard Deviation" , col=sentimentvocabkmeansmodel$cluster, size= 9)
text3d(normaggregatetrain$sentiment,normaggregatetrain$length,normaggregatetrain$beforeelection,  rownames(normaggregatetrain))



k <- 2
set.seed(100)
newsentimentkmeansmodel <- kmeans(normaggregatetrain[,-3], k)

str(train)
fviz_cluster(newsentimentkmeansmodel, data=normaggregatetrain[,-3], axes = c(1,3), xlab = "Normalized Sentiment", ylab = "Article Length")
