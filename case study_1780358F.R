install.packages("twitteR")
install.packages("ROAuth")
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")

install.packages("NLP")
install.packages("tm")
install.packages("wordcloud")


library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(RCurl)
library(httr)
library(NLP)
library(tm)
library(wordcloud)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
requestURL<-"https://api.twitter.com/oauth/request_token"
accessURL<-"https://api.twitter.com/oauth/access_token"
authURL<-"https://api.twitter.com/oauth/authorize"


consumer_key <- "XSYQhZJRqxWuG0nuDgd0ifXYY"
consumer_secret <- "GB0i8QsaEfCqah5YH3he8rZcwUpQA6eQjnY7n94Eu8DnsaqXVV"
access_token <- "2212517660-T5NQ1altSfhBvpyvVSEvWzu8l9BwvALJ7M5HN51"
access_secret <- "H6b051g5BhHNR2o22W2Eq6k46YkubvwnGoOiR0gZSbSS1"


setup_twitter_oauth (consumer_key,consumer_secret,access_token,access_secret)


Oracle.list<- searchTwitter('#oracle', n=500)

setwd("C:/Users/acer 1/Desktop/Case study")

Oracle.df = twListToDF(Oracle.list)  
write.csv(Oracle.df, file='OracleTweets.csv', row.names=FALSE)


SAP.list<- searchTwitter('#SAP', n=500)

setwd("C:/Users/acer 1/Desktop/Case study")

SAP.df = twListToDF(SAP.list)  
write.csv(SAP.df, file='SAPTweets.csv', row.names=FALSE)



ERPSystem <- searchTwitter("ERPSystem",n=100)
ERPSystem_text <- sapply(ERPSystem, function(x) x$getText()) 

ERPSystem_textcorpus <- Corpus(VectorSource(ERPSystem_text)) 
ERPSystem_textcorpus <- tm_map(ERPSystem_textcorpus, removePunctuation)
ERPSystem_textcorpus <- tm_map(ERPSystem_textcorpus, content_transformer(tolower))
ERPSystem_textcorpus <- tm_map(ERPSystem_textcorpus, function(x)removeWords(x,stopwords()))
wordcloud(ERPSystem_textcorpus)



OracleBigData <- searchTwitter("OracleBigData",n=25)
OracleBigData_text <- sapply(OracleBigData, function(x) x$getText()) 

OracleBigData_textcorpus <- Corpus(VectorSource(OracleBigData_text)) 
OracleBigData_textcorpus <- tm_map(OracleBigData_textcorpus, removePunctuation)
OracleBigData_textcorpus <- tm_map(OracleBigData_textcorpus, content_transformer(tolower))
OracleBigData_textcorpus <- tm_map(OracleBigData_textcorpus, function(x)removeWords(x,stopwords()))
wordcloud(OracleBigData_textcorpus)


SAPBigData <- searchTwitter("SAPBigData",n=25)
SAPBigData_text <- sapply(SAPBigData, function(x) x$getText()) 

SAPBigData_textcorpus <- Corpus(VectorSource(SAPBigData_text)) 
SAPBigData_textcorpus <- tm_map(SAPBigData_textcorpus, removePunctuation)
SAPBigDatam_textcorpus <- tm_map(SAPBigData_textcorpus, content_transformer(tolower))
SAPBigData_textcorpus <- tm_map(SAPBigData_textcorpus, function(x)removeWords(x,stopwords()))
wordcloud(SAPBigData_textcorpus)


library(plyr)
library(stringr)
library(ggplot2)


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{

  require(plyr)
  require(stringr)
  scores=laply(sentences, function(sentence, pos.words, neg.words){
    sentence = gsub('[[:punct:]]', '', sentence) #Punctuation characte
    sentence = gsub('[[:cntrl:]]', '', sentence) #Control characters
    sentence = gsub('\\d+', '', sentence) # \\d ==> Digits
    sentence = tolower(sentence)
    
    word.list = str_split(sentence, '\\s+') #\\s ==> space
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
  
setwd("C:/Users/acer 1/Desktop/Case study")
  hu.liu.pos = scan('positive-words.txt', what='character', comment.char=';') #All Comments are ignored with <;> in front in the text file
  hu.liu.neg = scan('negative-words.txt', what='character', comment.char=';')
  
  pos.words = hu.liu.pos
  neg.words = hu.liu.neg
  
  
  DatasetOracle <- read.csv("OracleTweets.csv")
  DatasetOracle$text<-as.factor(DatasetOracle$text)
  
  DatasetSAP <- read.csv("SAPTweets.csv")
  DatasetSAP$text<-as.factor(DatasetSAP$text)
  
 
  Oracle.scores = score.sentiment(DatasetOracle$text, pos.words,neg.words, .progress='text')
  SAP.scores = score.sentiment(DatasetSAP$text, pos.words,neg.words, .progress='text')

  write.csv(Oracle.scores,file=paste("OracleScores.csv",sep=""),row.names=TRUE)
  write.csv(SAP.scores,file=paste("SAPScores.csv",sep=""),row.names=TRUE)
  
  
  Oracle.scores$Team = 'Oracle'
  SAP.scores$Team = 'SAP'
 
  hist(Oracle.scores$score)
  qplot(Oracle.scores$score)
  
  hist(SAP.scores$score)
  qplot(SAP.scores$score)
  
  all.scores = rbind(Oracle.scores, SAP.scores)
  ggplot(data=all.scores) + # ggplot works on data.frames, always
    geom_histogram(mapping=aes(x=score, fill=Team), binwidth=1) +
    facet_grid(Team~.) + # make a separate plot for each hashtag
    theme_bw() + scale_fill_brewer(palette = "Oranges") # plain display, nicer colors
  

  all.scores$very.pos.bool = all.scores$score >=2
  all.scores$very.neg.bool = all.scores$score <= -2
  
  twitter.df = ddply(all.scores,c('Team'), summarise,
                     very.pos.count=sum(very.pos.bool),
                     very.neg.count=sum(very.neg.bool))
  
  twitter.df$very.tot = twitter.df$very.pos.count +
    twitter.df$very.neg.count
  
  twitter.df$score= round(100*twitter.df$very.pos.count/twitter.df$very.tot)
 
