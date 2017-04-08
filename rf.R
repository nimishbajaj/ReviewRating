library(caTools)

tweetSparse = read.csv("tweetsSparse.csv")
set.seed(123)
split = sample.split(tweetsSparse$Avg, SplitRatio = 0.7)
trainSparseTree = subset(tweetsSparse, split==TRUE)
testSparseTree = subset(tweetsSparse, split==FALSE)

# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Avg ~ ., data=trainSparseTree)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparseTree)

table(testSparseTree$Avg, predictRF)

# Accuracy:
mat = as.matrix(table(testSparseTree$Avg, predictRF))
sum(diag(mat))/sum(mat)

#average rating
sum(colSums(mat)*c(1,2,3,4,5))/sum(mat)
