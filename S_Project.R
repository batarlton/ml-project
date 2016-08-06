library(twitteR)
library(tm)
library(qdap)
library(quantmod)

# Word lists are modified versions from Hu and Liu
#    Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
#        Proceedings of the ACM SIGKDD International Conference on Knowledge 
#        Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
#        Washington, USA, 
positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")

setup_twitter_oauth("b3mFh2y716eNJszWHw8mZpaLi", "ZnIni1gLHQtqFCoPs6P8OBgvQU04fQdDkCRtI3edyioYaNv0T4", "454653060-FK01BRN85A0SF4DPjbb3XCG4N5qzxpxtnOznGQrX", "2oYjMJI648RXaoyBhr2nbDmqbXqiZyDDq6n59UWZOMWgG")
setup_twitter_oauth("zxYRrt8ehfhkzObPHaS0ogvPH", "2NlLd6164qb5Dr7594HqTp8jGthfrRY0A4bU0uJAXcrQe8zbQu", "752704258428968960-ekoMIp6sj0Sg2AQL6zGJObVWCxqzHKy", "8fBFRwtatrp3a4axwOLYDoITI3kFjX7TbzpMXKWLTxtfq")

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