##Twitter sentimental analysis"""
#here I am using the tweet file(Tweets.csv) that I have scrapped from twitter



###########step1 -wordcloud formation#############
Tweets <- read.csv("~/Tweets.csv", comment.char="#")
View(Tweets)
str(Tweets)
library(tm)
library(wordcloud2)
install.packages("syuzhet")
library(syuzhet)
library(dplyr)
library(stringr)
install.packages("tidytext")
library(tidytext)


###Build Corpus######
#corpus is a collection of document.here each tweet is considered as a document
#here we are interested only in first column
corpus<-iconv(Tweets$text,to="UTF-8")
corpus <-Tweets$text
x <- Corpus(VectorSource(corpus))
inspect(x[1:5])



####Data cleansing####
###convert to lower,remove punctuaion,numbers,whitespace,url,stopwords
x1<-tm_map(x,tolower)
inspect(x1[2])

x1<-tm_map(x1,removePunctuation)
inspect(x1[2])

x1<-tm_map(x1,removeNumbers)
inspect(x1[2])

x1<-tm_map(x1,stripWhitespace)
inspect(x1[2])

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
x1<-tm_map(x1,content_transformer(removeURL))
inspect(x1[2])
inspect(x1[6])

x1<-tm_map(x1,removeWords,stopwords('english'))
inspect(x1[2])
inspect(x1[6])

x1<-tm_map(x1,removeWords,stopwords('english'))
x1<- tm_map(x1, content_transformer(stemDocument),language = 'english')
inspect(x1[2])
inspect(x1[6])

# Remove your own stop word
# specify your stopwords as a character vector
x1<- tm_map(x1, removeWords, c("rogerfed","ufb","uff","ufa","ufc","uufef","ufuf","ufeufedufufufc"))
inspect(x1[2])
inspect(x1[6])


#Term document Matrix
#covert unstructured data into structured format using TDM
tdm<-TermDocumentMatrix(x1)
tdm
dtm<-t(tdm)
tdm<-as.matrix(tdm)
View(tdm)
w<-rowSums(tdm)
w
w_sub<-subset(w,w>=50)
w_sub
w_small<-subset(w,w>=10)
w_small
w1<-data.frame(names(w_small),w_small)
colnames(w1)<-c('word','freq')
str(w1)
w1$word<-iconv(w1$word,"UTF-8")
library(wordcloud2)
wordcloud2(w1,size=0.7,shape="star")
#most frequently used word is "thank"



########Step 2 -Sentimental Analysis#######
##packages
install.packages("syuzhet")
library("syuzhet")
install.packages("plotly")           
library(plotly)
library(tm)
library(ggplot2)
library(reshape2)
library(dplyr)


#to get sentiment scores and store in emotions
#anger,anticipation,disgust,fear,joy,sadness,surprise,trust,negative,positive
emotions<-get_nrc_sentiment(corpus)
head(emotions)
corpus[6]
get_nrc_sentiment('hero')
corpus[25]
get_nrc_sentiment('Good night')
corpus[88]
get_nrc_sentiment('lucky')

emo_bar=colSums(emotions)
emo_bar

barplot(emo_bar,main="sentiment scores",ylab="Count",col=rainbow(10))
#highest bar is for positive,lowest is disgust

emo_sum=data.frame(count=emo_bar,emotion=names(emo_bar))
emo_sum
#positive 453 and disgust 33




