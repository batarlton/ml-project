#References
# https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
# https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/
#credera was used mostly for pulling the data from twitter and parsing it to get rid of unwanted symbols/spaces/weblinks/etc


#install.packages("syuzhet", dependencies = TRUE)
#install.packages("plyr", dependencies = TRUE)
#install.packages("neuralnet", dependencies = TRUE)
#install.packages("e1071", dependencies = TRUE)


library("syuzhet")
library("plyr")
library("neuralnet")
library("e1071")

# Due to the nature of the API we were forced to change the program so we could pull more tweets and save them. 
#  There was approx 480 api command limit and a decent amount of tweets but it had a limit too, since we were 
#  comparing a year at a time, it would have pushed us over the limit. Hence, we pulled about 100 a day for 
#  every day of 2015 and saved the tweets in files according to months
fileNames <- c("January2015.txt", "February2015.txt", "March2015.txt", "April2015.txt", "May2015.txt", "June2015.txt", "July2015.txt", "August2015.txt", "September2015.txt", "October2015.txt", "November2015.txt", "December2015.txt")

for(i in 1:length(fileNames))
{
  filePath <- getwd()
  currentFile <- sprintf("%s/%s", filePath, fileNames[i])
  temp <- read.csv(currentFile, header = FALSE, sep = "\n")
  if(i == 1)
  {
    tweetMatrix <- as.matrix(temp)
  }
  else
  {
    tweetMatrix <- cbind(tweetMatrix, temp)
  }
}
colnames(tweetMatrix) <- gsub(".txt", "", fileNames)

# get sentiment of the tweets
for(i in 1:ncol(tweetMatrix))
{
  tweetString <- as.character(tweetMatrix[[i]])
  sentiment <- get_sentiment(tweetString)
  theSum <- sum(sentiment)
  total <- length(sentiment)
  average <- theSum/total
  sentiment[sentiment != 0]
  totalPos <- length(positiveIndexes)
  totalNeg <- length(negativeIndexes)
  if(i == 1)
  {
    tweetSentimentTotal <- theSum
    tweetSentimentAverage <- average
  }
  else
  {
    tweetSentimentTotal <- cbind(tweetSentimentTotal, theSum)
    tweetSentimentAverage <- cbind(tweetSentimentAverage, average)
  }
}



#needs to be run after stock analysis.R
tweetVSstock <- cbind(t(tweetSentimentAverage), c(t(changeMatrix)))
rownames(tweetVSstock) <- gsub(".txt", "", fileNames)
colnames(tweetVSstock) <- c("Sentiment", "Class")



yearSentimentAvg <- sum(tweetSentimentAverage)/length(tweetSentimentAverage)
yearStockAvg <- sum(changeMatrix)/length(changeMatrix)

tweetVSstock<-as.matrix(tweetVSstock)

for(i in 1:nrow(tweetVSstock))
{
  if(tweetVSstock[i,1] < yearSentimentAvg)
    tweetVSstock[i,1] <- 0
  else
    tweetVSstock[i,1] <- 1
  
  if(tweetVSstock[i,2] < yearStockAvg)
    tweetVSstock[i,2] <- 0
  else
    tweetVSstock[i,2] <- 1
}

nn <- neuralnet(formula = tweetVSstock[,2]~tweetVSstock[,1], data = tweetVSstock, hidden = 5)
nb <- naiveBayes(formula = tweetVSstock[,2]~tweetVSstock[,1], data = tweetVSstock)
#not sure this is useful
km <- kmeans(tweetVSstock, centers= 2, iter.max = 10)


