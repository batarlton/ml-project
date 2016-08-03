#References
# https://cran.r-project.org/web/packages/twitteR/twitteR.pdf
# https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/



#install.packages("twitteR", dependencies = TRUE)
#install.packages("ROAuth", dependencies = TRUE)
#install.packages("tm", dependencies = TRUE)
#install.packages("wordcloud", dependencies = TRUE)
#install.packages("syuzhet", dependencies = TRUE)

library("twitteR")
library("ROAuth")
library("tm")
library("wordcloud")
library("syuzhet")

setup_twitter_oauth("zxYRrt8ehfhkzObPHaS0ogvPH", "2NlLd6164qb5Dr7594HqTp8jGthfrRY0A4bU0uJAXcrQe8zbQu", "752704258428968960-ekoMIp6sj0Sg2AQL6zGJObVWCxqzHKy", "8fBFRwtatrp3a4axwOLYDoITI3kFjX7TbzpMXKWLTxtfq")

# search will start at the "until", and end at "since"
tweetsAAPL <- searchTwitter("AAPL", n=526, lang = "en", since='2016-07-02', until='2016-08-02')
tweetsAAPL <- strip_retweets(tweetsAAPL)

#https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-2-create-word-cloud/
#  Above website showed how to obtain tweets and remove unnecessary words/symbols
tweetsAAPL.text <- sapply(tweetsAAPL, function(x) x$getText())
# Replace blank space ("rt")
tweetsAAPL.text <- gsub("rt", "", tweetsAAPL.text)
# Replace @UserName
tweetsAAPL.text <- gsub("@//w+", "", tweetsAAPL.text)
# Remove punctuation
tweetsAAPL.text <- gsub("[[:punct:]]", "", tweetsAAPL.text)
# Remove links
tweetsAAPL.text <- gsub("http\\w+", "", tweetsAAPL.text)
#convert all text to lower case
tweetsAAPL.text <- tolower(tweetsAAPL.text)
# Remove tabs
tweetsAAPL.text <- gsub("[ |]t]{2,}", "", tweetsAAPL.text)
# Remove blank spaces at the beginning
tweetsAAPL.text <- gsub("^ ", "", tweetsAAPL.text)
# Remove blank spaces at the end
tweetsAAPL.text <- gsub(" $", "", tweetsAAPL.text)

#TODO: find how to remove retweets

#create word cloud
#create corpus
tweetsAAPL.text.corpus <- Corpus(VectorSource(tweetsAAPL.text))
#clean up by removing stop words
tweetsAAPL.text.corpus <- tm_map(tweetsAAPL.text.corpus, function(x)removeWords(x, stopwords()))
#wordcloud(tweetsAAPL.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)

# get sentiment of the tweets
tweetsAAPL.text.sentiment <- get_sentiment(tweetsAAPL.text, method = "syuzhet")

###Sorting negative and positive tweets###
#get indexes
positiveIndexes <- tweetsAAPL.text.sentiment > 0
negativeIndexes <- tweetsAAPL.text.sentiment < 0

#separate tweets
tweetsAAPL.negativeTweets <- tweetsAAPL.text[negativeIndexes]
tweetsAAPL.positiveTweets <- tweetsAAPL.text[positiveIndexes]
#separate tweet sentiments
tweetsAAPL.positiveTweets.sentiment <- tweetsAAPL.text.sentiment[positiveIndexes]
tweetsAAPL.negativeTweets.sentiment <- tweetsAAPL.text.sentiment[negativeIndexes]





