######################DECISION TREE#################
###################Fraud_Check data set###########

#Problem statement: {#Use decision trees to prepare a model on fraud data 
#treating those who have taxable_income <= 30000 as "Risky" and others are "Good"
#Data Description :
#Undergrad : person is under graduated or not
#Marital.Status : marital status of a person
#Taxable.Income : Taxable income is the amount of how much tax an individual owes to the government 
#Work Experience : Work experience of an individual person
#Urban : Whether that person belongs to urban area or not}

######## import the dataset
Fraudcheck<-read.csv(file.choose())
View(Fraudcheck)
dim(Fraudcheck)   # 600   6
attach(Fraudcheck)
sum(is.na(Fraudcheck))  #no null values


colnames(Fraudcheck)
unique(Undergrad)  #NO and YES
unique(Marital.Status)#"Single"   "Divorced" "Married"
unique(Urban)        #"YES" "NO"


str(Fraudcheck)  #Undegrad,Marital.status,urban are Chr
#convert Chr datatype to factor format
Fraudcheck$Undergrad<-factor(Fraudcheck$Undergrad,labels = c(0,1))
table(Fraudcheck$Undergrad)
Fraudcheck$Marital.Status<-factor(Fraudcheck$Marital.Status,labels = c(0,1,2))
table(Fraudcheck$Marital.Status)
Fraudcheck$Urban<-factor(Fraudcheck$Urban,labels = c(0,1))
table(Fraudcheck$Urban)
str(Fraudcheck)

summary(Fraudcheck)

#Here target variable is Taxable.Income
#convert Target variable into categorical
#So convert Taxable_Income <= 30000 as "Risky" else "Good" and store it as another variable
Income_RG<-ifelse(Fraudcheck$Taxable.Income<=30000,"Risky","Good")
#bind income_RG with original dataset and convert it into a data frame
Fraud<-data.frame(Fraudcheck,Income_RG)
View(Fraud)
attach(Fraud)
table(Fraud$Income_RG)
#Good Risky 
#476   124
str(Fraud) #Income_RG  is the target variable & which is in Chr form
Fraud$Income_RG<-as.factor(Fraud$Income_RG)

#Visualization
hist(Fraud$Taxable.Income) #not normal #Are non-parametric and therefore do not require normality assumptions of the data.
hist(Fraud$City.Population)# not normal
hist(Fraud$Work.Experience) #not normal

boxplot(Fraud$Taxable.Income)#no outliers
boxplot(Fraud$City.Population) #no outliers
boxplot(Fraud$Work.Experience)  #no outliers

pairs(Fraud) 

#import necessary packages
#install.packeges("tree")
library(tree)  #for decision tree
library(caTools)
library(caret)  #to train and test data splitting
library(C50)  #C5.0 decision Tree
install.packages("rpart")
library(rpart)
?rpart  #Recursive Partitioning and Regression Trees


#########Train and test splitting
set.seed(123)  #to get same same set of data points every time
Fraud_split<-sample.split(Fraud$Income_RG,SplitRatio=0.8)

Fraud_train<-subset(Fraud,Fraud_split==TRUE)
View(Fraud_train)
dim(Fraud_train)  #480,7

Fraud_test<-subset(Fraud,Fraud_split==FALSE)
View(Fraud_test)
dim(Fraud_test)  #120,7


#################Model building- C5.0 decision tree (For classification)
Fc5.0_train <- C5.0(Fraud_train[,-7],Fraud_train$Income_RG)
plot(Fc5.0_train)
#calculate training accuracy
pred_train<-predict(Fc5.0_train,Fraud_train)
mean(Fraud_train$Income_RG==pred_train)   #100%

#confusion Matrix
#library(caret)
confusionMatrix(pred_train,Fraud_train$Income_RG)
#
#Reference
#Prediction Good Risky
#   Good    381     0
#   Risky    0    99
#Accuracy : 1          
#95% CI : (0.9923, 1)
#No Information Rate : 0.7938     
#P-Value [Acc > NIR] : < 2.2e-16 
# 'Positive' Class : Good  

#test accuaracy
pred_test <- predict(Fc5.0_train,newdata=Fraud_test) # predicting on test data
mean(pred_test==Fraud_test$Income_RG) # 100% accuracy 
#confusion Matrix
confusionMatrix(pred_test,Fraud_test$Income_RG)
##Reference
#Prediction Good Risky
#    Good    95     0
#    Risky    0    25
#Accuracy : 1 
#'Positive' Class : Good 

#Crosstable
library(gmodels)
CrossTable(Fraud_test$Income_RG,pred_test) #all are correctly classified


###############Decision Tree Model building  using party(ctree)
install.packages("party")
library(party)
F_tree = ctree(Income_RG ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = Fraud)
summary(F_tree)  
plot(F_tree)
pred_tree <- as.data.frame(predict(F_tree,newdata=Fraud_test))
pred_tree["final"] <- NULL
pred_test <- predict(F_tree,newdata=Fraud_test)


mean(pred_test==Fraud$Income_RG)  #79.33%

confusionMatrix(pred_test,Fraud_test$Income_RG)
#Prediction Good Risky
#Good    95    25
#Risky    0     0

#Accuracy : 0.7917 


######## CART  #############
#Classification And Regression Tree
##useful for classification and regression problems
library(rpart)  #library for CART
install.packages("rpart.plot")
library(rpart.plot)
# Building a regression tree using rpart 
# Simple model
model1 <- rpart(Income_RG~.,data=Fraud_train,method="anova")
model1
#1) root 480 78.58125 1.20625  
#2) Taxable.Income>=30329 381  0.00000 1.00000 *
#3) Taxable.Income< 30329 99  0.00000 2.00000 *
plot(model1)
rpart.plot(model1,type=3,digits=3,fallen.leaves = TRUE)
text(model1)


pred_Fraud <- predict(model1,Fraud_train)
head(pred_Fraud)
pred_Fraudtest <- predict(model1,Fraud_test)
head(pred_Fraudtest)
#crosstable
CrossTable(Fraud_test$Income_RG,pred_Fraudtest)  #one missclassification

