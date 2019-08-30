library(tidytext)
library(party)
library(caret)
library(randomForest)

load('data/2Transform.rda')

as.sparseMatrix <- function(simple_triplet_matrix_sparse) {
  retval <-  sparseMatrix(i=as.numeric(simple_triplet_matrix_sparse$i),
                          j=as.numeric(simple_triplet_matrix_sparse$j),
                          x=as.numeric(as.character(simple_triplet_matrix_sparse$v)),
                          dims=c(simple_triplet_matrix_sparse$nrow, 
                                 simple_triplet_matrix_sparse$ncol),
                          dimnames = simple_triplet_matrix_sparse$dimnames,
                          giveCsparse = TRUE)
}

publisher <- df$publication

m <- data.frame(as.matrix(as.sparseMatrix(dtm)))
m <- cbind(publisher,m)

m <- m[, -nearZeroVar(m)]
m[,"reuterscom"] <- NULL

m$ArticleLength <- nchar(df$fullText)

### Split into training and test

set.seed(123)
train_ind <- sample(nrow(m), size = .7*nrow(m))
train <- m[train_ind, ]
test <- m[-train_ind, ]






### Make decision tree predicting df$Publication from dtm's data

myTree <- ctree(publisher~.,train)

myPred <- predict(myTree,test)

table_mat <- table(test$publisher, myPred)
table_mat

sum(diag(table_mat))/sum(table_mat)

plot(myTree)

### Not bad for a default! Let's prune it.

goodTree <- ctree(publisher~.,train, controls=ctree_control(mincriterion=.99,minsplit=30,minbucket=10,maxdepth=7))

myPred <- predict(goodTree,test)

table_mat <- table(test$publisher, myPred)
table_mat

sum(diag(table_mat))/sum(table_mat)

plot(goodTree)












### Random Forests

myForest <- randomForest(publisher~.,train)

myPred <- predict(myForest,test)

table_mat <- table(test$publisher, myPred)
table_mat

sum(diag(table_mat))/sum(table_mat) # 67.6

### Great! 

goodForest <- randomForest(publisher~.,train,mtry=500) # Best of list below

myPred <- predict(goodForest,test) 
table_mat <- table(test$publisher, myPred)
table_mat
sum(diag(table_mat))/sum(table_mat)

varImpPlot(goodForest)


# ntree = default, mtry=800: 70.2
# ntree = default, mtry=600: 70.4
# ntree = default, mtry=500: 70.6 # Whoo! Only 10 % of data used for training
# ntree = default, mtry=400: 70.4
# ntree = default, mtry=300: 70.2
# ntree = default, mtry=200: 70.0
# ntree = default, mtry=100: 69.5
# ntree = default, mtry=50 : 68.6

# ntree = 1000, mtry=500: 70.49
# ntree = 250, mtry=500: 70.46


save(
  goodForest,
  file = "data/forest.rda"
)
