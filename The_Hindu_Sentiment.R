####Extract anything you choose from the internet and do some research on how we extract using R
##Programming and perform sentimental analysis.

##Sentimental analysis of Hindu Article
#install necessary packages
library(rvest)
library(XML)
library(xml2)
library(magrittr)
library(stringr)

############the_Hindu Article Extraction######
hurl<-"https://www.thehindu.com/news/national/kerala/two-more-bodies-recovered-from-pettimudy/article32368657.ece"
Hindu<-NULL
for(i in 1:4){
  murl<-read_html(as.character(paste(hurl,i,sep="=")))
  hindu<-murl %>%
    html_nodes("p")%>%      #data extracted on the basis of html node
    html_text()
  Hindu<-c(Hindu,hindu)
}
write.table(Hindu,"The_Hindu.text",row.names=F)    #download reviews into repository in text format(The_Hindu.text file)
getwd()

###The_Hindu article sentimental analysis"""
#here I am using the Hindu.text that I have scrapped from The_Hindu online portal

install.packages("tm")
library(tm)## for text mining

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
Hindu <- read.table("C:/Users/server/Desktop/EXCELR/assignments/text mining/Hindu.text", quote="\"")
View(Hindu)  #140 obs and 1 variable



###Build Corpus######
#corpus is a collection of document.here each review is considered as a document
corpus<-iconv(Hindu$x,to="UTF-8")
corpus <-Hindu$V1
x <- Corpus(VectorSource(corpus))     ## Load the data as a corpus
inspect(x[1])
inspect(x[6])




####Data cleansing####
###convert to lower,remove punctuaion,numbers,whitespace,url,stopwords
x1<-tm_map(x,tolower)
inspect(x1[1])
inspect(x1[6])

x1<-tm_map(x1,removePunctuation)
inspect(x1[1])

x1<-tm_map(x1,removeNumbers)
inspect(x1[1])

x1<-tm_map(x1,stripWhitespace)
inspect(x1[1])

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
x1<-tm_map(x1,content_transformer(removeURL))
inspect(x1[134])


x1<-tm_map(x1,removeWords,stopwords('english'))
inspect(x1[1])
inspect(x1[6])

x1<-tm_map(x1,removeWords,stopwords('english'))
x1<- tm_map(x1, content_transformer(stemDocument),language = 'english')
inspect(x1[1])
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

library(wordcloud2) ## word-cloud generator 
install.packages("RColorBrewer")## color palettes
library(RColorBrewer)

wordcloud2(w1,size=0.5,shape="star",backgroundColor = "white")
#most frequently used word is "landship","journal","articl"



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
corpus[1]
corpus[4]

get_nrc_sentiment('recov')
get_nrc_sentiment('trial')



sent_bar=colSums(emotions)
sent_bar   #high frequency for positive-340

barplot(sent_bar,main="sentiment scores",ylab="Count",col=rainbow(10))
#highest bar is for positive,lowest is disgust

sent_sum=data.frame(count=sent_bar,emotion=names(sent_bar))
sent_sum

