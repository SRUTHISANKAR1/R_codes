##1) Extract movie reviews for any movie from IMDB and perform sentimental analysis
###install necessary packages
library(rvest)
library(XML)
library(xml2)
library(magrittr)
library(stringr)


#####IMDB REVIEW EXTRACTION####
The_Lion_King<-NULL
url1<-"https://www.imdb.com/title/tt6105098/reviews?start="

for(i in 1:10){
  murl<-read_html(as.character(paste(url1,i,sep="")))
  rev<-murl %>%
    html_nodes(".review-container")%>%      
    html_text()
  The_Lion_King<-c(The_Lion_King,rev)
}
write.table(The_Lion_King,file="The_lion_king.txt")    
getwd() 
  

###IMDB THE LION KING review sentimental analysis"""
#here I am using the The_Lion_King.text that I have scrapped from IMDB site

###WORDCLOUD FORMATION

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
text <- read.csv("C:/Users/server/Desktop/EXCELR/assignments/text mining/The_lion_king.txt", sep="")
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
inspect(x1[1])

x1<-tm_map(x1,removePunctuation)
inspect(x1[1])

x1<-tm_map(x1,removeNumbers)
inspect(x1[1])

x1<-tm_map(x1,stripWhitespace)
inspect(x1[1])

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
x1<-tm_map(x1,content_transformer(removeURL))
inspect(x1[1])
inspect(x1[6])

x1<-tm_map(x1,removeWords,stopwords('english'))
inspect(x1[1])
inspect(x1[8])

x1<-tm_map(x1,removeWords,stopwords('english'))
x1<- tm_map(x1, content_transformer(stemDocument),language = 'english')
inspect(x1[1])
inspect(x1[6])

# Remove your own stop word
# specify your stopwords as a character vector
x1<- tm_map(x1, removeWords, c("The Lion King","review","movi"))
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
str(w1)    #824 observations and 2 variables
w1$word<-iconv(w1$word,"UTF-8")

library(wordcloud2)
install.packages("RColorBrewer")
library(RColorBrewer)

wordcloud2(w1,size=0.7,shape="star",backgroundColor = "white")

#most frequently used word is "help"

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

get_nrc_sentiment('ruin')

get_nrc_sentiment('disappointment')



sent_bar=colSums(emotions)
sent_bar   #high frequency for positive-2760

barplot(sent_bar,main="sentiment scores",ylab="Count",col=rainbow(10))
#highest bar is for positive,lowest is disgust

sent_sum=data.frame(count=sent_bar,emotion=names(sent_bar))
sent_sum

