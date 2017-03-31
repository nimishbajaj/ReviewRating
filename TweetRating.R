library(tm)
library(SnowballC)
library(caTools)
library(e1071)

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
round1 = tweets$Avg*10 + 30
round2 = round(round1, digits=-1)/10
tweets$Avg = as.factor(round2)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content


frequencies = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(frequencies, 0.995)
#sparse = frequencies
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Avg = tweets$Avg


set.seed(123)
split = sample.split(tweetsSparse$Avg, SplitRatio = 0.9)
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
13*1 + 66*2 + 259*3 + 17*4
990/sum(mat)
mean(round2)
