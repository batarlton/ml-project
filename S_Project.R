library(twitteR)
library(tm)
library(qdap)
library(quantmod)
library(rminer)
library(stringr)

source("https://raw.githubusercontent.com/batarlton/ml-project/master/stock_analysis.R")

positives= readLines("https://raw.githubusercontent.com/batarlton/ml-project/master/positive-words.txt")
negatives = readLines("https://raw.githubusercontent.com/batarlton/ml-project/master/negative-words.txt")
Jan <- readLines("https://raw.githubusercontent.com/batarlton/ml-project/master/January2015.txt")

# Gets frequent words in January text
commonWords <- paste0(findFreqTerms(as.DocumentTermMatrix(Jan), lowfreq = 2))


# Use positve and negative word count difference
# to classify tweets
# Reduce tweets to only frequency words
hlist <- list()
for (h in 1:length(Jan)){
  pos <- sum(str_count(Jan[h], positives))
  neg <- sum(str_count(Jan[h], negatives))
  t2<-paste0(c(str_extract(Jan[h], commonWords), sign(pos-neg+0.5)))
  hlist[[h]]<-t2
}
# set format to dataframe, name columns
t3<-as.data.frame(matrix(unlist(hlist),ncol = length(hlist[[1]]), byrow = T),stringAsFactors =TRUE)
colnames(t3)<-c(commonWords, "class")

###############
#Test tweet classification
###############

traint3<-sample(nrow(t3),100)

trainset <- t3[traint3,]
testset <- t3[-traint3,]

#Decision Tree
model <- fit(class~., data = trainset, model = "dt", task="class")
p <- predict(model, testset)
accp <- mmetric(p, testset$class, "ACC")
print(accp)

#Boosting
model <- fit(class~., data = trainset, model = "boosting", task="class")
p <- predict(model, testset)
accp <- mmetric(p, testset$class, "ACC")
print(accp)

#Naive Bayes
model <- fit(class~., data = trainset, model = "naiveBayes", task="class")
p <- predict(model, testset)
accp <- mmetric(p, testset$class, "ACC")
print(accp)

###############
#Using Naive Bayes model to correlate stocks
###############

twmean <- c()
# January
twmean <- c(twmean, mean(as.numeric(p)))

filenames <- c("https://raw.githubusercontent.com/batarlton/ml-project/master/February2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/March2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/April2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/May2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/June2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/July2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/August2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/September2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/October2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/November2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/December2015.txt")

for (l in 1:length(filenames)){
  tmonth <- readLines(filenames[l])
  tmonth <- sample(tmonth, 200)
  hlist <- list()
  for (h in 1:length(tmonth)){
    pos <- sum(str_count(tmonth[h], positives))
    neg <- sum(str_count(tmonth[h], negatives))
    t2<-paste0(c(str_extract(tmonth[h], commonWords), sign(pos-neg+0.5)))
    hlist[[h]]<-t2
  }
  # set format to dataframe, name columns
  t3<-as.data.frame(matrix(unlist(hlist),ncol = length(hlist[[1]]), byrow = T),stringAsFactors =TRUE)
  colnames(t3)<-c(commonWords, "class")
  p <- predict(model, t3)
  twmean <- c(twmean, mean(as.numeric(p)))
}
print(twmean)
plot(x=1:12,y=changeMatrix, type="l")
plot(x=1:12,y=twmean, type="l")

# Main_Project.R - separate code developed and played with

# #References
# # https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
# # https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/
# #credera was used mostly for pulling the data from twitter and parsing it to get rid of unwanted symbols/spaces/weblinks/etc
# 
# 
# #install.packages("syuzhet", dependencies = TRUE)
# #install.packages("plyr", dependencies = TRUE)
# #install.packages("neuralnet", dependencies = TRUE)
# #install.packages("e1071", dependencies = TRUE)
# install.packages("kernlab", dependencies = TRUE)
# 
# library("syuzhet")
# library("plyr")
# library("neuralnet")
# library("e1071")
# library("kernlab")
# 
# # Due to the nature of the API we were forced to change the program so we could pull more tweets and save them. 
# #  There was approx 480 api command limit and a decent amount of tweets but it had a limit too, since we were 
# #  comparing a year at a time, it would have pushed us over the limit. Hence, we pulled about 100 a day for 
# #  every day of 2015 and saved the tweets in files according to months
# filenames <- c("https://raw.githubusercontent.com/batarlton/ml-project/master/January2015.txt","https://raw.githubusercontent.com/batarlton/ml-project/master/February2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/March2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/April2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/May2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/June2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/July2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/August2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/September2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/October2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/November2015.txt", "https://raw.githubusercontent.com/batarlton/ml-project/master/December2015.txt")

# 
# for(i in 1:length(fileNames))
# {
#   filePath <- getwd()
#   currentFile <- sprintf("%s/%s", filePath, fileNames[i])
#   temp <- read.csv(currentFile, header = FALSE, sep = "\n")
#   if(i == 1)
#   {
#     tweetMatrix <- as.matrix(temp)
#   }
#   else
#   {
#     tweetMatrix <- cbind(tweetMatrix, temp)
#   }
# }
# colnames(tweetMatrix) <- gsub(".txt", "", fileNames)
# 
# # get sentiment of the tweets
# for(i in 1:ncol(tweetMatrix))
# {
#   tweetString <- as.character(tweetMatrix[[i]])
#   sentiment <- get_sentiment(tweetString)
#   theSum <- sum(sentiment)
#   total <- length(sentiment)
#   average <- theSum/total
#   sentiment[sentiment != 0]
#   totalPos <- length(positiveIndexes)
#   totalNeg <- length(negativeIndexes)
#   if(i == 1)
#   {
#     tweetSentimentTotal <- theSum
#     tweetSentimentAverage <- average
#   }
#   else
#   {
#     tweetSentimentTotal <- cbind(tweetSentimentTotal, theSum)
#     tweetSentimentAverage <- cbind(tweetSentimentAverage, average)
#   }
# }
# 
# 
# 
# #needs to be run after stock analysis.R
# tweetVSstock <- cbind(t(tweetSentimentAverage), c(t(changeMatrix)))
# rownames(tweetVSstock) <- gsub(".txt", "", fileNames)
# colnames(tweetVSstock) <- c("Sentiment", "Class")
# 
# 
# 
# yearSentimentAvg <- sum(tweetSentimentAverage)/length(tweetSentimentAverage)
# yearStockAvg <- sum(changeMatrix)/length(changeMatrix)
# 
# tweetVSstock<-as.matrix(tweetVSstock)
# 
# for(i in 1:nrow(tweetVSstock))
# {
#   if(tweetVSstock[i,1] < yearSentimentAvg)
#     tweetVSstock[i,1] <- 0
#   else
#     tweetVSstock[i,1] <- 1
#   
#   if(tweetVSstock[i,2] < yearStockAvg)
#     tweetVSstock[i,2] <- 0
#   else
#     tweetVSstock[i,2] <- 1
# }
# 
# nn <- neuralnet(formula = tweetVSstock[,2]~tweetVSstock[,1], data = tweetVSstock, hidden = 5)
# nb <- naiveBayes(formula = tweetVSstock[,2]~tweetVSstock[,1], data = tweetVSstock)
# #not sure this is useful
# km <- kmeans(tweetVSstock, centers= 2, iter.max = 10)
# #randomforest?
# svm <- ksvm(tweetVSstock)


# stock_analysis.R included as commented code to be checked for originality

# #install.packages("quantmod",dependencies = TRUE)
# library(quantmod)
# 
# #Get prices from a certain date to a certain date
# getSymbols("AAPL", from = '2015-01-01', to = '2015-01-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Create matrix for changes
# changeMatrix <- matrix(nrow = 1, ncol = 12)
# 
# #Get the change in price
# changeMatrix[1] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# #Get prices from a certain date to a certain date
# getSymbols("AAPL", from = '2015-02-01', to = '2015-02-28')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[2] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# #Get prices from a certain date to a certain date
# getSymbols("AAPL", from = '2015-03-01', to = '2015-03-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[3] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# #Get prices from a certain date to a certain date
# getSymbols("AAPL", from = '2015-04-01', to = '2015-04-30')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[4] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# #Get prices from a certain date to a certain date
# getSymbols("AAPL", from = '2015-05-01', to = '2015-05-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[5] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-06-01', to = '2015-06-30')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[6] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-07-01', to = '2015-07-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[7] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-08-01', to = '2015-08-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[8] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-09-01', to = '2015-09-30')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[9] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-10-01', to = '2015-10-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[10] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-11-01', to = '2015-11-30')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[11] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# getSymbols("AAPL", from = '2015-12-01', to = '2015-12-31')
# 
# #Put stock info into matrix
# stockMatrix <- as.matrix(AAPL)
# 
# #Get the change in price
# changeMatrix[12] <- stockMatrix[nrow(stockMatrix),4] - stockMatrix[1,1]
# 
# #Create a chart
# #chartSeries(AAPL)