####1) Extract reviews of any product from ecommerce website like snapdeal and amazon
####2) Perform sentimental analysis

#install necessary packages
library(rvest)
library(XML)
library(magrittr)
library(stringr)


############AMAZON REVIEW EXTRACTION######
aurl<-"https://www.amazon.in/Samsung-Galaxy-Storage-Additional-Exchange/product-reviews/B08444S68Q/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
Amazon_reviews<-NULL
for(i in 1:10){
  murl<-read_html(as.character(paste(aurl,i,sep="=")))
  rev<-murl %>%
    html_nodes(".review-text")%>%      #data extracted on the basis of html node
    html_text()
  Amazon_reviews<-c(Amazon_reviews,rev)
}
write.table(Amazon_reviews,"sam.text",row.names=F)    #download reviews into repository in text format(sam.text file)
getwd()   #to get working directory



###Amazon review sentimental analysis"""
#here I am using the sam.text that I have scrapped from amazon

install.packages("tm")
library(tm)

install.packages("slam")
library(slam)

install.packages("topicmodels")
library(topicmodels)

install.packages("SnowballC")
library(SnowballC)

library(wordcloud2)

library(syuzhet)

library(dplyr)

install.packages("tidytext")
library(tidytext)

## Read the text file
text<- read.table("C:/Users/server/Desktop/EXCELR/assignments/text mining/sam.text", header=TRUE, quote="\"")
text
length(text)



###Build Corpus######
#corpus is a collection of document.here each review is considered as a document
corpus<-iconv(text$x,to="UTF-8")
corpus <-text$x
x <- Corpus(VectorSource(corpus))     ## Load the data as a corpus
inspect(x[1])



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
inspect(x1[8])

x1<-tm_map(x1,removeWords,stopwords('english'))
x1<- tm_map(x1, content_transformer(stemDocument),language = 'english')
inspect(x1[2])
inspect(x1[6])



# Remove your own stop word
# specify your stopwords as a character vector
x1<- tm_map(x1, removeWords, c("samsung","phone"))
inspect(x1[2])
inspect(x1[6])



#Build Term document Matrix.Document matrix is a table containing the frequency of the words. Column names are words and row names are documents.
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
str(w1)    #237 observations and 2 variables
w1$word<-iconv(w1$word,"UTF-8")

library(wordcloud2)
install.packages("RColorBrewer")
library(RColorBrewer)

wordcloud2(w1,size=0.7,shape="star",backgroundColor = "white")



#most frequently used word is "good"
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

get_nrc_sentiment('good')
corpus[5]
get_nrc_sentiment('bad')
corpus[15]


sent_bar=colSums(emotions)
sent_bar   #high frequency for positive-340

barplot(sent_bar,main="sentiment scores",ylab="Count",col=rainbow(10))
#highest bar is for positive,lowest is disgust

sent_sum=data.frame(count=sent_bar,emotion=names(sent_bar))
sent_sum
