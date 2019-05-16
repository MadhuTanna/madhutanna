library(twitteR)
library(tm)

my_consumer_key = "LczMq86RGqnumw9xO7fmhW8hL"
my_consumer_secret = "CKDs4P8Xon7Fm3UCzJ6HTUjQ924Ak14rU01kJbjjqjDjDDJocX"
my_access_token = "244501086-0dDj07bZQNKvFdY49gLq7X3MM7oQQdwdIbPAfqbX"
my_access_secret = "WYses5XpWN9AOhq8hSh9fKaqqeeAKqevO5evxZNVOcmxh"

setup_twitter_oauth(consumer_key = my_consumer_key,consumer_secret = my_consumer_secret,access_token = my_access_token,access_secret = my_access_secret)

setwd("C:/Users/MADHU/Downloads/Data Science/Term 3 Project")
getwd()

notwitts = 25

my_search_amazon = searchTwitter("#amazonIN", n=notwitts)
NewsearchAmazon = do.call("rbind",lapply(my_search_amazon,as.data.frame))
write.csv(NewsearchAmazon,file="searchAmazon.csv")

searchAmazon = read.csv("searchAmazon.csv")

test_amazon=searchAmazon$text
Amazon_icv = lapply(test_amazon,function(x) iconv(x,"latin1","ASCII",sub=""))
Amazon_icv = lapply(Amazon_icv,function(x) gsub("http.*","",x))
Amazon_icv = lapply(Amazon_icv,function(x) gsub("\\d+","",x))
Amazon_icv = lapply(Amazon_icv,function(x) gsub("[[:punct:]]","",x))
Amazon_icv = lapply(Amazon_icv,function(x) gsub("[[:cntrl:]]","",x))
Amazon_icv = lapply(Amazon_icv,function(x) tolower(x))

typeof(Amazon_icv)

positivewords = readLines("positive-words.txt")
negativewords = readLines("negative-words.txt")

#words = names(freq)

calcSentimentScore = function(oneSent) 
{
  wordsList = strsplit(oneSent, "\\s+")
  words = unlist(wordsList)
  typeof(words)

  posMatches = match(words, positivewords)
  negMatches = match(words, negativewords)
 
  posMatches = !is.na(posMatches)
  negMatches = !is.na(negMatches)
  
  score = sum(posMatches) - sum(negMatches)
  return(score)
}

Amazon_Analytics_Score = lapply(Amazon_icv,calcSentimentScore)
typeof(Amazon_Analytics_Score)

hist(as.numeric(Amazon_Analytics_Score), xlab = "", main = "Sentiment of sample tweets'", border = "black", col = "skyblue")

finalscore_amazon=0
for (i in seq(1,notwitts)) {
  finalscore_amazon=finalscore_amazon+Amazon_Analytics_Score[[i]]
  finalscore_amazon
}
finalscore_amazon

#========================== Amazon====================

my_search_Flipkart = searchTwitter("#flipkart", n=notwitts)
NewsearchFlipkart = do.call("rbind",lapply(my_search_Flipkart,as.data.frame))
write.csv(NewsearchFlipkart,file="searchFlipkart.csv")

searchFlipkart = read.csv("searchFlipkart.csv")

test_Flipkart=searchFlipkart$text
Flipkart_icv = lapply(test_Flipkart,function(x) iconv(x,"latin1","ASCII",sub=""))
Flipkart_icv = lapply(Flipkart_icv,function(x) gsub("http.*","",x))
Flipkart_icv = lapply(Flipkart_icv,function(x) gsub("\\d+","",x))
Flipkart_icv = lapply(Flipkart_icv,function(x) gsub("[[:punct:]]","",x))
Flipkart_icv = lapply(Flipkart_icv,function(x) gsub("[[:cntrl:]]","",x))
Flipkart_icv = lapply(Flipkart_icv,function(x) tolower(x))

Flipkart_Analytics_Score = lapply(Flipkart_icv,calcSentimentScore)
typeof(Flipkart_Analytics_Score)

hist(as.numeric(Flipkart_Analytics_Score), xlab = "", main = "Sentiment of sample tweets'", border = "black", col = "skyblue")

finalscore_Flipkart=0
for (i in seq(1,25)) {
  finalscore_Flipkart=finalscore_Flipkart+Flipkart_Analytics_Score[[i]]
  finalscore_Flipkart
}
finalscore_Flipkart

#=======================================Flipkart==============================================

my_search_snapdeal = searchTwitter("#snapdeal", n=notwitts)
Newsearchsnapdeal = do.call("rbind",lapply(my_search_snapdeal,as.data.frame))
write.csv(Newsearchsnapdeal,file="searchsnapdeal.csv")

searchsnapdeal = read.csv("searchsnapdeal.csv")

test_snapdeal=searchsnapdeal$text
snapdeal_icv = lapply(test_snapdeal,function(x) iconv(x,"latin1","ASCII",sub=""))
snapdeal_icv = lapply(snapdeal_icv,function(x) gsub("http.*","",x))
snapdeal_icv = lapply(snapdeal_icv,function(x) gsub("\\d+","",x))
snapdeal_icv = lapply(snapdeal_icv,function(x) gsub("[[:punct:]]","",x))
snapdeal_icv = lapply(snapdeal_icv,function(x) gsub("[[:cntrl:]]","",x))
snapdeal_icv = lapply(snapdeal_icv,function(x) tolower(x))

snapdeal_Analytics_Score = lapply(snapdeal_icv,calcSentimentScore)
typeof(snapdeal_Analytics_Score)

hist(as.numeric(snapdeal_Analytics_Score), xlab = "", main = "Sentiment of sample tweets'", border = "black", col = "skyblue")

finalscore_snapdeal=0
for (i in seq(1,25)) {
  finalscore_snapdeal=finalscore_snapdeal+snapdeal_Analytics_Score[[i]]
  finalscore_snapdeal
}
finalscore_snapdeal