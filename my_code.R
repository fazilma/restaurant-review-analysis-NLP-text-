#rm(list = ls())
#Importing the dataset

dataset_original = read.delim('Restaurant_Reviews.tsv',quote = '',stringsAsFactors = FALSE)

#Cleaning the text
#install.packages('tm')
library(tm)

corpus = VCorpus(VectorSource(dataset_original$Review))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
library(SnowballC)
corpus = tm_map(corpus, removeWords,stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

#Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked
dataset$Liked = factor(dataset$Liked, levels = c(0,1))
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked,SplitRatio =0.8)
training_set = subset(dataset, split==TRUE)
test_set= subset(dataset, split==FALSE)

library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)
#Prediction

y_pred = predict(classifier, newdata = test_set[-692])

#Confusion Matrix
cm = table(test_set[,692],y_pred)