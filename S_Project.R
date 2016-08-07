<<<<<<< HEAD

#install.packages("twitteR", dependencies = TRUE)
#install.packages("tm", dependencies = TRUE)
#install.packages("qdap")
#install.packages("quantmod", dependencies = TRUE)
#install.packages("rminer", dependencies = TRUE)
#install.packages("stringr", dependencies = TRUE)

=======
>>>>>>> c9a939e60ab27c7f77781ed5781484cb6b02d8be
library(twitteR)
library(tm)
library(qdap)
library(quantmod)
library(rminer)
library(stringr)

source("stock_analysis.R")

# Word lists are modified versions from Hu and Liu
#    Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
#        Proceedings of the ACM SIGKDD International Conference on Knowledge 
#        Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
#        Washington, USA, 
positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")
Jan <- readLines("January2015.txt")

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

filenames <- c("February2015.txt", "March2015.txt", "April2015.txt", "May2015.txt", "June2015.txt", "July2015.txt", "August2015.txt", "September2015.txt", "October2015.txt", "November2015.txt", "December2015.txt")

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