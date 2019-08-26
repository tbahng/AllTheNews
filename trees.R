library(tidytext)
library(party)
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

### Split into training and test

set.seed(123)
train_ind <- sample(nrow(m), size = .01*nrow(m))
train_ind2 <- sample(nrow(m), size = .01*nrow(m))


train <- m[train_ind, ]
test <- m[train_ind2, ]

### Make decision tree predicting df$Publication from dtm's data

myTree <- ctree(publisher~.,train)

myPred <- predict(myTree,test)

table_mat <- table(test$publisher, myPred)
table_mat
