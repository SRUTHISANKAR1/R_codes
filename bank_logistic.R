#################LOGISTIC REGRESSION#######################################
################Bank_full.csv##############################################
#PS:Output variable -> y
##y -> Whether the client has subscribed a term deposit or not 
#Binomial ("yes" or "no")


#############load the dataset
bank.full <- read.csv("C:/Users/server/Downloads/bank-full.csv", sep=";")
View(bank.full)   #45211 obs and 17 variables


########understanding of data
class(bank.full)  #check whether the data is data.frame or not
attach(bank.full)
head(bank.full) #view head of dataset
sum(is.na(bank.full))  #no null values
summary(bank.full) #descriptive statistics.... I think there are some outliers .because max is very high
str(bank.full)   #large number of categorical variables
colnames(bank.full)  #to get columns in bank.full data


############to get the character columns
chrs <- sapply(bank.full, is.character)
chrCols <- names(bank.full[, chrs])
chrCols


############To get the unique terms and counts of each chr variables
table(job)
table(marital)
table(education)
table(default)
table(housing)
table(loan)
table(contact)
table(month)
table(poutcome)
table(y)   #imbalanced


#############categorical variables to numeric###########
#remove dummy variable trap
bank.full$job<-as.numeric(factor(bank.full$job))-1
bank.full$marital<-as.numeric(factor(bank.full$marital))-1
bank.full$education<-as.numeric(factor(bank.full$education))-1
bank.full$default<-as.numeric(factor(bank.full$default))-1
bank.full$housing<-as.numeric(factor(bank.full$housing))-1
bank.full$loan<-as.numeric(factor(bank.full$loan))-1
bank.full$contact<-as.numeric(factor(bank.full$contact))-1
bank.full$month<-as.numeric(factor(bank.full$month))-1
bank.full$poutcome<-as.numeric(factor(bank.full$poutcome))-1
bank.full$y<-as.numeric(factor(bank.full$y))-1

#################boxplot
boxplot(bank.full$age,main="age") #outliers are there
boxplot(bank.full$job,main="job")#no outlier
boxplot(bank.full$marital,main="marital")
boxplot(bank.full$education,main="education") #no outlier
boxplot(bank.full$default,main="default")     #outlier   only yes/no .so graph looks like this
boxplot(bank.full$balance,main="balance")   #large outliers
boxplot(bank.full$housing,main="housing")
boxplot(bank.full$loan,main="loan")#outlier
boxplot(bank.full$contact,main="contact")
boxplot(bank.full$day,main="day") #no outlier
boxplot(bank.full$month,main="month") #no outlier
boxplot(bank.full$duration,main="duration")  #outlier
boxplot(bank.full$campaign,main="campaign")   #outlier
boxplot(bank.full$pdays,main="pdays")  #outlier
boxplot(bank.full$previous,main="previous")   #outlier
boxplot(bank.full$poutcome,main="poutcome")   #outlier
boxplot(bank.full$y,main="y")


################Model Building
#create a logistic regression model to predict Whether the client has subscribed a term deposit or not 
gl_model1<-glm(y~.,data = bank.full,family = binomial)
summary(gl_model1)
#job is not significant
#AICvlue is very high


bank.full$predict<-predict(gl_model1,data=bank.full,type="response") #prediction
confusion=table(bank.full$y,bank.full$predict>0.5)
confusion
  #confusionmatrix
# FALSE  TRUE
#0 39136   786
#1  4144  1145

#accuracy,precision,sensitivity,specificity, and Fscore -to understand whether the model is good or not  
accuracy=sum(diag(confusion))/sum(confusion)
accuracy #0.8909557   (89%)
error_rate=1-accuracy
error_rate  #(10%)


#precision=TP/(TP+FP)
precision=(1145)/(1145+786)
precision  #0.59295 (59%)


#sensitivity=TP/(TP+FN)
sensitivity=1145/(1145+4144)
sensitivity   #0.216487  (21%)


specificity=39136/(39136+786)    #TN/TN+FP
specificity   #0.9803116    (98%)

F_score=2*(precision*sensitivity)/(precision+sensitivity)
F_score    #0.31717   (31.7%)

data.frame(accuracy,precision, sensitivity,F_score)
#  accuracy  precision   sensitivity   F_score
#1 0.8908451 0.592957    0.216487      0.3171745


gl_model1$fitted.values   #to compare predicted values with actual values
y_pred<-ifelse(bank.full$predict>0.5,1,0)  #predicted>50% is in level1, else level 0
y_pred


Bank_final<-cbind(bank.full,y_pred)   #combine y_pred to the original dataset
table(y)       
table(y_pred)


library(car)
avPlots(gl_model1,id.n=2,id.cex=0.7)  #29183thobs outlier
influenceIndexPlot(gl_model1,id.n=3) # index plots for infuence measures
#29183-cooks distance>1
influencePlot(gl_model1,id.n=3)   #29183 outlier

#model<-glm(y~.,data=bank.full[-29183,],family=binomial)
#summary(model)
#model$predict<-predict(model,data=bank.full[-29183,],type="response") #prediction
#confusion1=table(bank.full[-29183,]$y,model$predict>0.5)
#confusion1
#accuracy1=sum(diag(confusion1))/sum(confusion1)
#accuracy1   #0.890157   no difference


################ROC curve##############################
# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(bank.full$predict,bank.full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
#high area 0-1 Model is good

library(ggplot2)
plot(gl_model1$fitted.values)



#################################################################################
#job is not significant.create a logistic regression model by removing job column
Bank<-bank.full[,-2]    #removing job column from bank.full data set and created a new data set Bank
gl_model2<-glm(y~.,data=Bank,family=binomial)   
summary(gl_model2)  #AIC bit increased
exp(coef(gl_model2))

Bank$predict<-predict(gl_model2,data=Bank,type='response')
cm=table(Bank$y,Bank$predict>0.5)
cm    #confusion matrix
#  FALSE  TRUE
#0  39139   783
#1 4152   1137

#accuracy=(TP+TN) /TOTAL
accuracy1=sum(diag(cm))/sum(cm)
accuracy1=(39139+1137)/(39139+783+4152+1137)
accuracy1  #0.8908451 accuracy has reduced











