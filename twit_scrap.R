# ONE:
#1) Extract tweets for any user (try choosing a user who has more tweets)
#2) Perform sentimental analysis on the tweets extracted from the above


library("twitteR")  
library("ROAuth")
library("base64enc")
library("httpuv")

#used for scrapping and extracting data from the web
cred<-OAuthFactory$new(consumerKey='jjQp2AnN22AsJnbHqUEWYnVzL',consumerSecret='iHxdBId0OxSEwbWdtMEkzxqoo4PM9WGEjJisbvQwx9tnx9ptfe',#API Key secret)
                       requestURL='https://api.twitter.com/oauth/request_token',
                       accessURL='https://api.twitter.com/oauth/access_token',
                       authURL='https://api.twitter.com/oauth/authorize')

save(cred,file="twitter authentification.Rdata")                       
load("twitter authentification.Rdata")

setup_twitter_oauth('jjQp2AnN22AsJnbHqUEWYnVzL','iHxdBId0OxSEwbWdtMEkzxqoo4PM9WGEjJisbvQwx9tnx9ptfe','1281056949937692674-xrlcKy6xaZDmYOvpjzNnZDpi4t7DjX','yZIDcpt1fnHkn69wT8wF30zpiY05Pwqjv0vsuwd45YtFA')
tweets<-userTimeline("rogerfederer",n=1000,includeRts =T) #includeRts means include retweets
TweetsDF<-twListToDF(tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF,"Tweets.csv",row.names =F)
getwd()



