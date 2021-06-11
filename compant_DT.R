##############DECISION TREE#############
#############COMPANY DATA###############
#Problem Statement:
#A cloth manufacturing company is interested to know about the segment or attributes causes high sale. 
#Approach - A decision tree can be built with target variable Sale (we will first convert it in categorical variable) & all other variable will be independent in the analysis.  


#load the data set
company<-read.csv(file.choose())
View(company)
dim(company)   #400  11
sum(is.na(company))  #no null values
attach(company)

str(company)
#here target variable is numerical,Shelveloc,Urban and US chr
#convert chr to factor
company$ShelveLoc<-factor(company$ShelveLoc,labels=c(0,1,2))
#Bad(0),Good(1) and Medium(2) 
company$Urban<-factor(company$Urban,labels=c(0,1))  #no=0,yes=1
company$US<-factor(company$US,labels=c(0,1))

#Target variable should convert into categorical. consider sales>10 "High" and sales<10 "Low"
Sale<-ifelse(company$Sales<=10,"Low","High")
Company<-data.frame(company,Sale)
dim(Company)  # 400  12
attach(Comapny)
Company$Sale<-as.factor(Company$Sale)   #convert into factor form
str(Company)
summary(Company)   #statistical summary
table(Company$Sale)
#High  Low 
#78    322 

#Visualization
hist(Company$Sales)    #normal
hist(Company$Income)   #Not normal
hist(Company$CompPrice)  #Normal
hist(Company$Advertising)
hist(Company$Population)  #Not normal
hist(Company$Price)     #Normal
hist(Company$Age)       #Not normal
hist(Company$Education)  ##Not normal

boxplot(Company$Sales)   #outlier
boxplot(Company$CompPrice)#outlier
boxplot(Company$Income)  #No outlier
boxplot(Company$Advertising)
boxplot(Company$Population)  #No outlier
boxplot(Company$Price)     #outlier
boxplot(Company$Age)       #No outlier
boxplot(Company$Education) #No outlier
  
#import necessary packages
#install.packeges("tree")
library(tree)  #for decision tree
library(caTools)
library(caret)  #to train and test data splitting
library(C50)  #C5.0 decision Tree
install.packages("rpart")
library(rpart)
?rpart  #Recursive Partitioning and Regression Trees
library(gmodels)  #to get crosstable

#########Train and test splitting
set.seed(123)  #to get same same set of data points every time
Company_split<-sample.split(Company,SplitRatio=0.8)

Comp_train<-subset(Company,Company_split==TRUE)
View(Comp_train)
dim(Comp_train)  #320  12

Comp_test<-subset(Company,Company_split==FALSE)
View(Comp_test)
dim(Comp_test)  #80 12

####Model building using ctree#########
C_tree = ctree(Sale ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = Comp_train)
summary(C_tree)
plot(C_tree)
pred_tree <- as.data.frame(predict(C_tree,newdata=Comp_test))
pred_tree["final"] <- NULL
pred_test <- predict(C_tree,newdata=Comp_test)


mean(pred_test==Company$Sale)  #73%

confusionMatrix(pred_test,Comp_test$Sale)
#          Reference
#Prediction High Low
#High    8   7
#Low    13  72
#Accuracy : 0.8 

CrossTable(Comp_test$Sale,pred_test)  #Missclassifications are there

#################Model building- C5.0 decision tree (For classification)
C5_train <- C5.0(Comp_train[,-12],Comp_train$Sale)
plot(C5_train)
#calculate training accuracy
pred_train<-predict(C5_train,Comp_train)
mean(Comp_train$Sale==pred_train)   #100%

#confusion Matrix
#library(caret)
confusionMatrix(pred_train,Comp_train$Sale)

#Reference
#Prediction High Low
#High    62   0
#Low     0   258
#Accuracy : 1   

#test accuaracy
pred_test <- predict(C5_train,newdata=Comp_test) # predicting on test data
mean(pred_test==Comp_test$Sale) # 100% accuracy 
#confusion Matrix
confusionMatrix(pred_test,Comp_test$Sale)
