library(twitteR)
library(tm)
library(qdap)
library(quantmod)
library(rminer)
# Word lists are modified versions from Hu and Liu
#    Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
#        Proceedings of the ACM SIGKDD International Conference on Knowledge 
#        Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
#        Washington, USA, 
positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")
Jan <- readLines("January2015.txt")
Febr <- readLines("February2015.txt")
March <- readLines("March2015.txt")

commonWords <- paste0(findFreqTerms(as.DocumentTermMatrix(Jan), lowfreq = 2))

# 

# getSymbols("AAPL", from = '2015-01-01', to = '2015-01-31')
# AAPLChange <- AAPL$AAPL.Close[[nrow(AAPL$AAPL.Close)]]-AAPL$AAPL.Open[[1]]
# 
blist = list()

hlist <- list()
for (h in 1:length(Jan)){
  pos <- sum(str_count(Jan[h], positives))
  neg <- sum(str_count(Jan[h], negatives))
  t2<-paste0(c(str_extract(Jan[h], commonWords), sign(pos-neg+0.5)))
  hlist[[h]]<-t2
}
t3<-as.data.frame(matrix(unlist(hlist),ncol = length(hlist[[1]]), byrow = T),stringAsFactors =TRUE)
colnames(t3)<-c(commonWords, "class")
model <- fit(class~., data = t3[1:500,], model = "naiveBayes", task="class")
p <- predict(model, t3[501:1000,])

blist[[1]] <- c(Reduce("+",hlist), AAPLChange)

getSymbols("AAPL", from = '2016-07-29', to = '2016-08-06')
AAPLChange <- as.data.frame(AAPL$AAPL.Close-AAPL$AAPL.Open)
AAPLChange$date <- as.Date(rownames(AAPLChange))

alist = list()

for (i in 1:nrow(AAPLChange)){
  sdate <-as.character(AAPLChange[i,2]-1)
  udate <-as.character(AAPLChange[i,2]+1)
  tweetsAAPL <- searchTwitter("#AAPL", n=50, since = sdate, until = udate)
  tweetsAAPL <- do.call("rbind", lapply(tweetsAAPL, as.data.frame))[,"text"]
  tweetsAAPL <- str_to_lower(rm_url(tweetsAAPL))
  
  tlist = list()
  
  for (j in 1:length(tweetsAAPL)){
    pos <- sum(str_count(tweetsAAPL[j], positives))
    neg <- sum(str_count(tweetsAAPL[j], negatives))
    tlist[[j]]<-c(pos,neg)
  }
  alist[[i]] <- c(Reduce("+",tlist), AAPLChange[i,1])
}



print(alist)