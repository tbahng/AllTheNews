library(caret)
library(e1071)

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

### Make Naive Bayes

defaultBayes <- naiveBayes(publisher~.,train)

myPred <- predict(defaultBayes,test) 
table_mat <- table(test$publisher, myPred)
table_mat
sum(diag(table_mat))/sum(table_mat) 

# Even with 70% of the data, the model is only 32.2% accurate.