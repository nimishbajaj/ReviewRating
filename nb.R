#using naive bayes classifier
library(e1071)
library(caTools)

#Split train test
tweets = read.csv("tweetsSparse.csv")
set.seed(123)
split = sample.split(tweets$Avg, SplitRatio = 0.7)
train = subset(tweets, split==TRUE)
test = subset(tweets, split==FALSE)


# Naive Bayes Classifier

set.seed(123)
tweetNB = naiveBayes(Avg ~ ., data=train)

# Make predictions:
predictNB = predict(tweetNB, newdata=test)

table(test$Avg, predictNB)

# Accuracy:
mat = as.matrix(table(test$Avg, predictNB))
sum(diag(mat))/sum(mat)

#average rating
sum(colSums(mat)*c(1,2,3,4,5))/sum(mat)
