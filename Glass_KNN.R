#Prepare a model for glass classification using KNN
##################KNN-Glass.csv################


#Load the dataset
Glass<-read.csv("C:/Users/server/Downloads/glass.csv")
#214obs and 10 variables
head(Glass)  


str(Glass)
summary(Glass)
sum(is.na(Glass)) #no null values

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
#rename the type datapoints if needed
#Glass$Type<-factor(Glass$Type,levels = c(1,2,3,4,5,6,7),labels = c("building_windows_float_processed","building_windows_non_float_processed","vehicle_windows_float_processed","vehicle_windows_non_float_processed","containers","tableware","headlamps"))

str(Glass)  #Type is int..so convert to factor
Glass$Type<-as.factor(Glass$Type)
str(Glass$Type)

#table of type-to get the exact number of each type
table(Glass$Type)

# table or proportions with more informativelabels
round(prop.table(table(Glass$Type))*100,digits=1)

summary(Glass)


#either normalize or standardise the data
#create normalization features
#normalize<-function(x){
 # return((x-min(x))/(max(x)-min(x)))
#}
#normalize(c(1,2,3,4,5))  #check normalization

#normalize the data
#Glass_norm<-as.data.frame(lapply(Glass[1:9],normalize)) #ignore "type" column
#head(Glass_norm)
#summary(Glass_norm$RI)

####I am standardising the data using scale function
standard.Glass <-as.data.frame(scale(Glass[,1:9]))
head(standard.Glass)


sum(is.na(standard.Glass))

#split data randomly into train and test data by using set.seed
set.seed(123)
Glass_split<-sample(1:nrow(standard.Glass),size=nrow(standard.Glass)*0.7,replace=FALSE)
Glass_train<-standard.Glass[Glass_split,]
Glass_test<-standard.Glass[-Glass_split,]

NROW(Glass_train)#149 train data
NROW(Glass_test)  #65 test data

head(Glass_train)
head(Glass_test)


#create labels for train and test data set
Train_glass_labels<-Glass[Glass_split,10]
Test_glass_labels<-Glass[-Glass_split,10]

head(Train_glass_labels)   ##[1] 3 7 6 1 7 5   ###Levels: 1 2 3 5 6 7
head(Test_glass_labels)   ##[1] 1 1 1 1 1 1    ###Levels: 1 2 3 5 6 7

sum(is.na(Test_glass_labels))

NROW(Train_glass_labels)#149 datapoints in train data
NROW(Test_glass_labels)  #65 datapoints


#Build KNN model by using class library
#create a prediction model

test_pred<-knn(train=Glass_train,test=Glass_test,cl=Train_glass_labels,k=1)
test_pred
table(test_pred) ####1  2  3  5  6  7     ####19 29  6  4  3  4 

#install.packages("gmodels") # gmodels for creating crosstable
library(gmodels)
# create the cross tabullation of predicted vs actual

str(Test_glass_labels)
str(test_pred)  # both are factors

CrossTable(x=Test_glass_labels,y=test_pred,prop.chisq=FALSE)

#check accuracy
accuracy.1<-100*sum(Test_glass_labels==test_pred)/NROW(Test_glass_labels)
accuracy.1   ###67.6923

###Confusionmatrix  by using caret package
confusionMatrix(table(test_pred,Test_glass_labels))  #so many misclassifications are there

############Test_glass_labels
#test_pred  1  2  3  5  6  7
#########1 14  1  2  0  1  1
#########2  5 19  3  0  0  2
#########3  3  1  2  0  0  0
#######  0  1  0  2  0  1
#########6  0  0  0  0  3  0
#########7  0  0  0  0  0  4

##Overall Statistics

#Accuracy : 0.6769          
#95% CI : (0.5495, 0.7877)
#No Information Rate : 0.3385          
#P-Value [Acc > NIR] : 2.687e-08       

#Kappa : 0.5561


#use loop to get better accuracy and better k value
i=1
k_value=1
for(i in 1:30){
  knn_pred<-knn(train=Glass_train,test=Glass_test,cl=Train_glass_labels,k=i)
  k_value[i]<-100*sum(Test_glass_labels==knn_pred)/NROW(Test_glass_labels)
  k=i
  cat(k,"=",k_value[i],'\n')
}

#1 = 67.69231    #best
#2 = 61.53846 
#3 = 60 
#4 = 55.38462 
#5 = 58.46154 
#6 = 56.92308 
#7 = 58.46154 
#8 = 56.92308 
#9 = 55.38462 
#10 = 56.92308 
#11 = 58.46154 
#12 = 55.38462 
#13 = 55.38462 
#14 = 53.84615 
#15 = 56.92308 
#16 = 55.38462 
#17 = 56.92308 
#18 = 53.84615 
#19 = 50.76923 
#20 = 53.84615 
#21 = 52.30769 
#22 = 53.84615 
#23 = 52.30769 
#24 = 50.76923 
#25 = 52.30769
#26 = 50.76923 
#27 = 49.23077 
#28 = 50.76923 
#29 = 50.76923 
#30 = 50.76923 





