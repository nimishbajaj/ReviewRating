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
sparse = removeSparseTerms(frequencies, 0.997)
#sparse = frequencies
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Avg = tweets$Avg

write.csv(tweetsSparse, "tweetsSparse.csv")
set.seed(123)
split = sample.split(tweetsSparse$Avg, SplitRatio = 0.7)
train = subset(tweetsSparse, split==TRUE)
test = subset(tweetsSparse, split==FALSE)
write.csv(train, "trainCSV.csv")
write.csv(test, "testCSV.csv")

