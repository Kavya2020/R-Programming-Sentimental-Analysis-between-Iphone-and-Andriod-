 
  library(tm)
library(pdftools)
library(qdapTools)
library(stringr)
  library(SnowballC)
library(wordcloud)
library(Matrix)
  library(plyr)
  library(syuzhet)
  library(stringr)
#library(lsa)
library(RColorBrewer)

filepath <-"C:/Dev/workspaceR/Project CSC 522/Samsung S10"
setwd(filepath)
file<-'s10tweets.csv'
text = file(file,open="r")
library(twitteR)
library(ROAuth)

consumer_key <-'YqHkgQcVkt5pVclIc1Bb4eit8'
consumer_secret <- 'OUhInyhAmb9WDQ4tKkQHK9GKLBBSKb2F6MGSdqYJO1k42wRdJL'


access_token <- '908445785670393856-p5Kp68fhQQlOFA7JT97NHQoQFFwtchK'
access_secret <- 'dgmZUxjF7WnlpdywAy7fermRZ4yfrAqT3CZXXOZlL2Nu2'
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)
s10.tweets = searchTwitter("Samsung+Galaxy+S10",lang = "en", n=25000)
s10tweets<- s10.tweets
head(s10tweets)
str(s10tweets)
dim(s10tweets)
s10tweets <- do.call("rbind", lapply(s10tweets, as.data.frame))
write.csv(s10tweets, file ="C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/s10twt.csv")

filepath <-"C:/Dev/workspaceR/Project CSC 522/Final Project/output csv"
setwd(filepath)
file<-'s10twt.csv'
text = file(file,open="r")
text.decomposition =readLines(text)
text.decomposition[1]

text.decomposition[2]
corpus <- Corpus(VectorSource(text.decomposition)) # colection of documents containing texts 
corpus

msgs = s10tweets[, c("text")]
head(msgs, 3)
library(ggplot2)
library(sentimentr)
d = get_nrc_sentiment(msgs)
head(d)
td = data.frame(t(d))
td_new = data.frame(rowSums(td[1:500]))

# transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<- td_new[1:8,]
qplot(sentiment, data =td_new2, weight = count, geom="bar", fill= sentiment)+
  ggtitle("Samsung Galaxy S10 tweets")

corpus <-tm_map(corpus,PlainTextDocument)
corpus <-tm_map(corpus,tolower)
corpus <-tm_map(corpus,removeNumbers) 



stopwords("english")
selfstopwords <- c("samsung","galaxy","plus","phone", "got","get", "new", "anonymous", "phones", "just","can")
corpus <- tm_map(corpus,removeWords, c(stopwords("english"), selfstopwords))
writeLines(as.character(corpus[[2]]))


corpus <-tm_map(corpus,removePunctuation)
corpus <-tm_map(corpus,stripWhitespace)
writeLines(as.character(corpus[[2]]))

corpus.tdm <-TermDocumentMatrix(corpus)
colnames(corpus.tdm)<-(1:dim(corpus.tdm)[2])
write.csv(as.matrix(corpus.tdm),file=file.path("S10_tweets.csv"))

frequency <- rowSums(as.matrix(corpus.tdm))
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)
words <- names(frequency)
wordcloud(words[1:50], frequency[1:50],
          scale = c(2, 1),colors=brewer.pal(8,"Dark2")) # color palette 

positives= readLines("positive-words.txt") #dictionary: we match if theres more +ve/-ve words in our doc 
negatives = readLines("negative-words.txt")

which_pos <-Terms(corpus.tdm) %in% positives
which_neg <- Terms(corpus.tdm) %in% negatives 

#find the +ve documents and save the matrix 
score_pos <- colSums(as.matrix(corpus.tdm[which_pos, ])) 
score_neg <- colSums(as.matrix(corpus.tdm[which_neg, ]) )
score<-score_pos-score_neg
score

tdm_pos<-corpus.tdm[ ,score>0]
write.csv(as.matrix(tdm_pos),file=file.path("sentimentAnalysis\\tdm_s9_pos.csv"))

tdm_neg<-corpus.tdm[ ,score<0]
write.csv(as.matrix(tdm_neg),file=file.path("filepath <-("C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/s10_neg.csv")
#setwd(filepath)"))
# find the neutral docs and save the matrix 
tdm_neu<-corpus.tdm[ ,score==0]
write.csv(as.matrix(tdm_neu),file=file.path("filepath <-("C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/s10_neu.csv")
#setwd(filepath)"))

# plot positive word cloud 
frequency_pos <- rowSums(as.matrix(tdm_pos))
frequency_pos <- sort(frequency_pos, decreasing=TRUE)
words_pos <- names(frequency_pos)
wordcloud(words_pos[1:50], frequency_pos[1:50],scale = c(2, 1),colors=brewer.pal(8,"Dark2"))

# plot negative word cloud 
frequency_neg <- rowSums(as.matrix(tdm_neg))
frequency_neg <- sort(frequency_neg, decreasing=TRUE)
words_neg <- names(frequency_neg)
wordcloud(words_neg[1:50], frequency_neg[1:50],scale = c(2, 0.8),colors=brewer.pal(8,"Dark2")) #scale = c(2, 0.8)

# neutral 
frequency_neu <- rowSums(as.matrix(tdm_neu))
frequency_neu <- sort(frequency_neu, decreasing=TRUE)
words_neu <- names(frequency_neu)
wordcloud(words_neu[1:50], frequency_neu[1:50],scale = c(2, 0.8),colors=brewer.pal(8,"Dark2"))

