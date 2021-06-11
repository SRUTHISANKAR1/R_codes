######Calories_consumed-> predict weight gained using calories consumed#####
###x=Calory calories_consumed
###y=weight gained
calories <- read.csv("C:/Users/server/Downloads/calories_consumed.csv")
View(calories_consumed)
library(psych)
library(lattice)
library(dplyr)
attach(calories)
summary(calories)
is.null(calories)

######Exploratory Data Analysis#####
#plots-dotplot,boxplot,histogram,scatter plot,qqnorm,qqline

dotplot(Weight.gained..grams.,main="WG")
dotplot(Calories.Consumed,main="Calories Consumed")
      ###extreme values or outliers are there in both cases

boxplot(Weight.gained..grams.,main="WG",col="dodgerblue4",horizontal = T) #no outliers
boxplot(Calories.Consumed,main="Calories Consumed",col="dodgerblue4",horizontal = T)#no outliers

hist(Weight.gained..grams.,main="WG",col="red")#not normal
hist(Calories.Consumed,main="Calories Consumed",col="red")# normal

plot(Weight.gained..grams.,Calories.Consumed,main="scatter plot",xlab="Calories.consume",ylab="Weight.gained..grams.",col="red")
       #linear
qqnorm(calories$Weight.gained..grams.)
qqline(calories$Weight.gained..grams.)#notnormal
qqnorm(calories$Calories.Consumed)
qqline(calories$Calories.Consumed)#normal


####Shapiro-Wilk normality test####
shapiro.test(calories$Weight.gained..grams.)##p-value = 0.006646  p value <0.05   so not normal
shapiro.test(calories$Calories.Consumed) ###p-value = 0.4887  p value >0.05    so normal


#correlation-cor(). for understanding if there is strong relation between dependent and independent variable
cor(Calories.Consumed,Weight.gained..grams.)   ###0.946991   strong relation. bcz its greater than 0.85
plot(Calories.Consumed,Weight.gained..grams.,main="cor")  #linear ,positive ,strong

     ####model building#####
#create linear regression model using lm function
#name<-lm(Y~X)#dependent variable~independent variable

model_raw<-lm(Weight.gained..grams.~Calories.Consumed,data=calories)
summary(model_raw)
        ##Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
        #Calories.Consumed is significant
        #Intercept(beta zero)= -625.75   Calories.Consumed(beta1)=0.42016 
        #Residual standard error: 111.6
        # 1577.201+2.134(Calories.Consumed)=Weight.gained..grams.
plot(model_raw,main="model_raw")  #no outliers
hist(model_raw$residuals)
model_raw$fitted.values


# we have to calculate error rate. for calculating error rate,
#first we have to calculate the predictions
conf<-predict(model_raw,interval="confidence")  #conf interval
conf<-as.data.frame(conf) # converting conf to dataframe  #4.482599(fit),-95.02772(lower),103.99291(upper)
#prediction interval
head(conf)
pred<-predict(model_raw,interval="predict")
pred<-as.data.frame(pred)   #4.482599,-258.20569	267.1709
       ##prediction interval range is wider than conf interval
head(pred)
#####RMSE#####
sqrt(sum(model_raw$residuals^2)/nrow(calories))   #103.3025  huge error,not acceptable


 ######Transformations to get better R2 value#############

#sqrt transformations
model1<-lm(Weight.gained..grams.~sqrt(Calories.Consumed),data=calories)
summary(model1)
      #Residual standard error: 131.5 
      #Multiple R-squared:  0.8567,	Adjusted R-squared:  0.8448  both reduced


##logarithmic transformation
model2<-lm(Weight.gained..grams.~log(Calories.Consumed),data=calories)
summary(model2)
      #Residual standard error: 152.3
      #Multiple R-squared:  0.8077,	Adjusted R-squared:  0.7917    reduced


#####exponential transformation###
model3<-lm(log(Weight.gained..grams.)~Calories.Consumed,data=calories)
summary(model3)
     #Multiple R-squared:  0.8776,	Adjusted R-squared:  0.8674 #Residual: 0.3314
     ####R2 value reduced

##polynomial model
model4<-lm(Weight.gained..grams.~Calories.Consumed+I(Calories.Consumed^2),data=calories)
summary(model4)
      #Multiple R-squared:  0.9521,	Adjusted R-squared:  0.9433 Residual: 79.43 
      #R2 value improved than first model. Error reduced
###RMSE###
sqrt(sum(model4$residuals^2)/nrow(calories))   #70.40752   Reduced
##Hence we can consider model 4 as best model
plot(model4)
 
             ####TRANSFORMATIONS OVERVIEW####

#1.model_raw##    Multiple R-squared:0.8968,  Adjusted R-squared:0.8882  Residual: 111.6
#2.model1= (sqrt)=Multiple R-squared:0.8567,  Adjusted R-squared:0.8448  Residual: 131.5
#3.model2=(log)=  Multiple R-squared:0.8077,  Adjusted R-squared:0.7917  Residual: 152.3
#4.model3=(exp)=  Multiple R-squared:0.8776,  Adjusted R-squared:0.8674  Residual: 0.3314
#model4=(poly)=   Multiple R-squared:0.9521,	Adjusted R-squared:0.9433  Residual: 79.43

predctn<-predict(model4,calories)
head(predctn)
calories$pred_weight<-predctn
head(calories)

library(ggplot2)

ggplot(data=calories,aes(x=Weight.gained..grams.,color="actual"))+
  geom_density(aes(x=Weight.gained..grams.,color="actual"))+
  geom_density(aes(x=pred_weight,color="predict"))+
  scale_color_manual(values=c("predict"="blue","actual"="red"))+
  labs(title="density plt between actual weight gain and predicted weight gain",
       x="weight gain",y= "")



