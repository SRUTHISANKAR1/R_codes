####Implement a KNN model to classify the animals in to categories
#####KNN-Zoo

#load the data set  
Zoo<-read.csv("C:/Users/server/Downloads/Zoo.csv")
View(Zoo)#101 obs 18 variables
str(Zoo)  #all are integers
sum(is.na(Zoo))   #zero null values

#convert variable to factor
names <- c(1:18)
Zoo[,names] <- lapply(Zoo[,names] , factor)
str(Zoo)

#first column is animal.name .we can ignore it
zoo<-Zoo[-1]
head(zoo)

##packages
install.packages('caTools')  #for train and test data split
install.packages('dplyr')    #for Data Manipulation
install.packages('class')    #KNN 
install.packages('caret')    #Confusion Matrix
install.packages('corrplot') #Correlation Plot

library(caTools)
library(dplyr)
library(caret)
library(class)
library(corrplot)

#here Type is our target variable
#table of type-to get the exact number of each type
table(zoo$type)  # 1  2  3  4  5  6  7    #41 20  5 13  4  8 10 

# table or proportions with more informativelabels
round(prop.table(table(zoo$type))*100,digits=1)

summary(zoo)   #I think no need of normalization and standardisation

#split data randomly into train and test data by using set.seed
set.seed(123)
zoo_split<-sample(1:nrow(zoo),size=nrow(zoo)*0.7,replace=FALSE)
zoo_train<-zoo[zoo_split,]
zoo_test<-zoo[-zoo_split,]

NROW(zoo_train)#70 train data
NROW(zoo_test)  # 31test data

head(zoo_train)
head(zoo_test)

#create labels for train and test data set
Train_zoo_labels<-zoo[zoo_split,17]
Test_zoo_labels<-zoo[-zoo_split,17]

head(Train_zoo_labels)
head(Test_zoo_labels)
sum(is.na(Test_zoo_labels))

NROW(Train_zoo_labels)# 70 datapoints in train data
NROW(Test_zoo_labels)#31

#Build KNN model by using class library
#create a prediction model

test_pred<-knn(train=zoo_train,test=zoo_test,cl=Train_zoo_labels,k=1)
test_pred
table(test_pred)

#install.packages("gmodels") # gmodels for creating crosstable
library(gmodels)
# create the cross tabullation of predicted vs actual

str(Test_zoo_labels)
str(test_pred)  # both are factors

CrossTable(x=Test_zoo_labels,y=test_pred,prop.chisq=FALSE)

#check accuracy
accuracy.1<-100*sum(Test_zoo_labels==test_pred)/NROW(Test_zoo_labels)
accuracy.1  #96.77

###Confusionmatrix  by using caret package
confusionMatrix(table(test_pred,Test_zoo_labels))


i=1
k_value=1
for(i in 1:10){
  knn_pred<-knn(train=zoo_train,test=zoo_test,cl=Train_zoo_labels,k=i)
  k_value[i]<-100*sum(Test_zoo_labels==knn_pred)/NROW(Test_zoo_labels)
  k=i
  cat(k,"=",k_value[i],'\n')
}

#1 = 96.77419 
#2 = 90.32258 
#3 = 87.09677 
#4 = 87.09677 
#5 = 87.09677 
#6 = 83.87097 
#7 = 83.87097 
#8 = 83.87097 
#9 = 83.87097 
#10 = 83.87097
