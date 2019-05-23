tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)

# Corpus: concept introduced by the tm package
# Corpus ; collection of documents
corpus = VCorpus(VectorSource(tweets$Tweet))

corpus

corpus[[1]]$content

corpus = tm_map(corpus, content_transformer(tolower)) # 소문자로 통일하기

corpus[[1]]$content

corpus = tm_map(corpus, removePunctuation) # 특수기호 없애기

corpus[[1]]$content

# stopword 리스트 생성
stopwords("english")[1:10]

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]$content

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)

frequencies

inspect(frequencies[1000:1005,505:515])


findFreqTerms(frequencies, lowfreq = 20)

findFreqTerms(frequencies, lowfreq = 50)

sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetSparse = as.data.frame(as.matrix(sparse))

colnames(tweetSparse) = make.names(colnames(tweetSparse))

tweetSparse$Negative = tweets$Negative

library(caTools)

set.seed(123)

split = sample.split(tweetSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetSparse, split == TRUE)
testSparse = subset(tweetSparse, split == FALSE)

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

predictCART = predict(tweetCART, newdata= testSparse, type="class")

table(testSparse$Negative, predictCART)

(294+18) / (294+6+37+18)

table(testSparse$Negative)

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data = trainSparse)

predictRF = predict(tweetRF, newdata = testSparse)

table(testSparse$Negative, predictRF)

(293+21) / (293+7+34+21)

