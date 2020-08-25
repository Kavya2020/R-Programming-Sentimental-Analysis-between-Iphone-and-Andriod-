library(tm)
library(pdftools)
library(qdapTools)
library(plyr)
library(syuzhet)
library(stringr)
library(SnowballC)
library(wordcloud)
library(Matrix)
#library(lsa)
library(RColorBrewer)
library(twitteR)
library(ROAuth)

consumer_key <-'YqHkgQcVkt5pVclIc1Bb4eit8'
consumer_secret <- 'OUhInyhAmb9WDQ4tKkQHK9GKLBBSKb2F6MGSdqYJO1k42wRdJL'


access_token <- '908445785670393856-p5Kp68fhQQlOFA7JT97NHQoQFFwtchK'
access_secret <- 'dgmZUxjF7WnlpdywAy7fermRZ4yfrAqT3CZXXOZlL2Nu2'
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)
Iphone11.tweets = searchTwitter("Iphone11",lang = "en", n=25000)
Iphone11<- Iphone11.tweets
head(Iphone11)
str(Iphone11)
Iphone11 <- do.call("rbind", lapply(Iphone11, as.data.frame))
write.csv(Iphone11, file ="C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/Iphone11.csv")

#filepath <-"C:/Dev/workspaceR/Project CSC 522/Final Project/output csv"
#setwd(filepath)
file<-'Iphone11.csv'
text = file(file,open="r")



text.decomposition =readLines(text)
text.decomposition[1]
text.decomposition[2]
corpus <- Corpus(VectorSource(text.decomposition)) # colection of documents containing texts 
corpus

# analysis and the sentimental result is ploted using ggplot
msgs = Iphone11[,c("text")]
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
  ggtitle("Iphone-11 tweets")

#using corpus making data into vector
docs = Corpus(VectorSource(msgs))
docs

# removing the stop words
corpus <-tm_map(corpus,PlainTextDocument)
corpus <-tm_map(corpus,tolower)
corpus <-tm_map(corpus,removeNumbers) 

stopwords("english")
selfstopwords <- c("iphone","apple", "plus", "phone","now", "iphone")
corpus <- tm_map(corpus,removeWords, c(stopwords("english"), selfstopwords))
writeLines(as.character(corpus[[2]]))


corpus <-tm_map(corpus,removePunctuation)
corpus <-tm_map(corpus,stripWhitespace)
writeLines(as.character(corpus[[2]]))

corpus.text <-TermDocumentMatrix(corpus)
colnames(corpus.text)<-(1:dim(corpus.text)[2])
write.csv(as.matrix(corpus.text),file=file.path("C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/cleaned_Iphone11.csv"))

frequency<- rowSums(as.matrix(corpus.text))
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)
words <- names(frequency)
wordcloud(words[1:50], frequency[1:50],
          scale = c(2, 0.9),colors=brewer.pal(8,"Dark2")) # color palette 
dim(iphone11)
# analysis and the sentimental result is ploted using ggplot
msgs = Iphone11[,c("text")]
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
  ggtitle("Iphone-11 tweets")

#Extracting positive and negative words

positives= readLines("positive-words.txt") #dictionary: we match if theres more +ve/-ve words in our doc 
negatives = readLines("negative-words.txt")

which_pos <-Terms(corpus.text) %in% positives
which_neg <- Terms(corpus.text) %in% negatives 

#find the +ve documents and save the matrix 
score_pos <- colSums(as.matrix(corpus.text[which_pos, ])) 
score_neg <- colSums(as.matrix(corpus.text[which_neg, ]) )
score<-score_pos-score_neg
score

text_pos<-corpus.text[ ,score>0]
write.csv(as.matrix(text_pos),file=file.path("C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/pos.csv"))

text_neg<-corpus.text[ ,score<0]
write.csv(as.matrix(text_neg),file=file.path("C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/neg.csv"))
# find the neutral docs and save the matrix 
text_neu<-corpus.text[ ,score==0]
write.csv(as.matrix(text_neu),file=file.path("C:/Dev/workspaceR/Project CSC 522/Final Project/output csv/neu.csv"))

# plot positive word cloud 
frequency_pos <- rowSums(as.matrix(text_pos))
frequency_pos <- sort(frequency_pos, decreasing=TRUE)
words_pos <- names(frequency_pos)
wordcloud(words_pos[1:50], frequency_pos[1:50], scale = c(1, 0.7), colors=brewer.pal(8,"Dark2"))

# plot negative word cloud 
frequency_neg <- rowSums(as.matrix(text_neg))
frequency_neg <- sort(frequency_neg, decreasing=TRUE)
words_neg <- names(frequency_neg)
wordcloud(words_neg[1:50], frequency_neg[1:50], scale = c(1, 0.5), colors=brewer.pal(8,"Dark2"))


# neutral 
frequency_neu <- rowSums(as.matrix(text_neu))
frequency_neu <- sort(frequency_neu, decreasing=TRUE)
words_neu <- names(frequency_neu)
wordcloud(words_neu[1:50], frequency_neu[1:50], scale = c(2, 0.9), colors=brewer.pal(8,"Dark2"))

# to extract top 5 countries from dataset
text<-c("Spain", "Polska", "South Korea", "United States","Canada")

#Make a logical vector
arg<-grepl("Spain",text)|grepl("Polska",text)|grepl("South Korea",text)|
  grepl("United States",text)|grepl("Canada",text)
#Pick out from text
text[arg]
length(unique(text[arg]))
