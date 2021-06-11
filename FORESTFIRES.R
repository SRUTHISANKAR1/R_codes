##################SUPPORT VECTOR MACHINE######################

#Problem statement
#######classify the Size_Categorie using SVM

#month	month of the year: 'jan' to 'dec'
#day	day of the week: 'mon' to 'sun'
#FFMC	FFMC index from the FWI system: 18.7 to 96.20
#	DMC index from the FWI system: 1.1 to 291.3
#DC	DC index from the FWI system: 7.9 to 860.6
#	ISI index from the FWI system: 0.0 to 56.10
#temp	temperature in Celsius degrees: 2.2 to 33.30
#RH	relative humidity in %: 15.0 to 100
#wind speed in km/h: 0.40 to 9.40
#rain	outside rain in mm/m2 : 0.0 to 6.4
#Size_Categorie 	the burned area of the forest ( Small , Large)

#target variable is  "size_category"

#load the dataset
forestfires<-read.csv(file.choose())
View(forestfires)  
dim(forestfires)  # 517  31
colnames(forestfires)
attach(forestfires)

######EXPLORATORY DATA ANALYSIS#####

#actually here dummy variables are created for month and day columns .so remove the original column and remove dummy variable trap
Forestfires<-forestfires[-c(1,2,12,19)]
head(Forestfires) #4 columns removed
dim(Forestfires)   #517  27
colnames(Forestfires)
attach(Forestfires)
str(Forestfires) #num,int and chr types of data present
sum(is.na(Forestfires))  #no null values
summary(Forestfires)


#here output variable is in Chr. label encode it and convert it into factor
unique(Forestfires$size_category)   #"small" "large"
Forestfires$size_category<-factor(Forestfires$size_category,labels=c(0,1))
head(Forestfires$size_category)
str(Forestfires)


#convert all variables to factor
columns<-c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area","daymon","daysat","daysun","daythu","daytue","daywed","monthaug",     
             "monthdec","monthfeb","monthjan","monthjul",     
             "monthjun","monthmar","monthmay","monthnov",     
             "monthoct","monthsep","size_category")
columns
Forestfires[,columns] <- lapply(Forestfires[,columns] ,factor)
str(letters)
str(Forestfires)
sum(is.na(Forestfires))
  summary(Forestfires)


############train -test data splitting#####################
library(caTools)
set.seed(123)#for getting same random numbers repeatedly(all the time)  .Pseudo random number generation.
FF_split=sample.split(size_category,SplitRatio=0.8)  #new column created
FF_train=subset(Forestfires,FF_split==TRUE)
View(FF_train)
dim(FF_train)  #413  27
FF_test=subset(Forestfires,FF_split==FALSE)
View(FF_test)
dim(FF_test)   #104  27 


#############SVM _model building############################
library(caret)
library(e1071)
library(kernlab)
model1<-ksvm(size_category~.,data=Forestfires,kernel="vanilladot")
model1  #Number of Support Vectors : 360 ,Training error : 0.009671
model1_pred<-predict(model1,FF_test)
head(model1_pred)
x=table(model1_pred,FF_test$size_category)
#check the agreement between predictions and test data
agreement<-model1_pred==FF_test$size_category
table(agreement) #FALSE  TRUE 
                 # 2   102
prop.table(table(agreement))
agreement
#FALSE       TRUE 
#0.01923077 0.98076923 
#98% accuracy- model is good

##we can create several  models by using differnt kernel tricks
#use different kernel tricks like # "rbfdot", "polydot", "tanhdot","laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"



