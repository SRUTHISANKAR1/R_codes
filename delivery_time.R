#Delivery_time -> Predict delivery time using sorting time
del_time <- read.csv("C:/Users/server/Downloads/delivery_time.csv")
View(del_time)
library(lattice)
summary(del_time)
attach(del_time)

###EDA####
#data visualization. dotplot,boxplot,histogram,scatter plot

dotplot(del_time$Delivery.Time,main="Del Time") #extreme valuesor outliers present
dotplot(del_time$Sorting.Time,main="Sort time")

boxplot(del_time$Delivery.Time,main="Del Time",col="dodgerblue4",horizontal = T)
boxplot(del_time$Sorting.Time,main="Sort time",col="dodgerblue4",horizontal = T)
#no outliers

hist(del_time$Delivery.Time,main="Del Time",col="dodgerblue4")
hist(del_time$Sorting.Time,main="Sort time",col="dodgerblue4")

#normality check
qqnorm(del_time$Delivery.Time,main="Del Time")
qqline(del_time$Delivery.Time,main="Del Time")#normal
qqnorm(del_time$Sorting.Time,main="Sort time")
qqline(del_time$Sorting.Time,main="Sort time")#normal

shapiro.test(del_time$Delivery.Time)#p-value = 0.8963  normal
shapiro.test(del_time$Sorting.Time)#  p-value = 0.1881 normal

#Scatterplot
plot(del_time$Delivery.Time,del_time$Sorting.Time,main="scatter plot",xlab="Sorting.Time",ylab="Delivery.Time")
#linear plositive

cor(Delivery.Time,Sorting.Time) #0.8259973   strong correlation bcz >0.8
plot(Delivery.Time,Sorting.Time,main="cor",col="red") #strong linear positive correlation

##model building
#create linear regression model using lm function
#name<-lm(Y~X)#dependent variable~independent variable
model_raw<-lm(Delivery.Time~Sorting.Time)
summary(model_raw)#Multiple R-squared:  0.6823,	Adjusted R-squared: 0.6655  Residual standard error: 2.935
#low R2 value
plot(model_raw)# no outliers
###confidence
conf<-predict(model_raw,interval="confidence")
conf<-as.data.frame((conf))
head(conf)
confint(model_raw,level=0.95)
#prediction interval
pred<-predict(model_raw,interval="predict")
pred<-as.data.frame(pred)  
head(pred)   #wider range

##RMSE##
sqrt(sum(model_raw$residuals^2)/nrow(del_time))#2.79165

###transformations to get better model to get higher R2 value,hence better model
#sqrt transformation
mod1<-lm(Delivery.Time~sqrt(Sorting.Time),data = del_time)
summary(mod1)#Multiple R-squared:  0.6958,	Adjusted R-squared:  0.6798 (improved),Res error: 2.872
#still low R2 value
#RMSE
sqrt(sum(mod1$residuals^2)/nrow(del_time))#2.731543  reduced

###logarithmic transformation
mod2<-lm(Delivery.Time~log(Sorting.Time),data=del_time)
summary(mod2)#Multiple R-squared:  0.6954,	Adjusted R-squared:  0.6794 Res: 2.873
sqrt(sum(mod2$residuals^2)/nrow(del_time))#2.733171

###Exponential Transformation
mod3<-lm(log(Delivery.Time)~Sorting.Time,data=del_time)
summary(mod3)#Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 #Resr: 0.1755 
sqrt(sum(mod3$residuals^2)/nrow(del_time))#0.1669628   low rmse
#better than the above model

###Quadratic model
mod4<-lm(sqrt(Delivery.Time)~Sorting.Time,data=del_time)
summary(mod4)#Multiple R-squared:  0.704,	Adjusted R-squared:  0.6885 Res: 0.349
sqrt(sum(mod4$residuals^2)/nrow(del_time))#0.3323459 rmse

##polynomial transformation
mod5<-lm(log(Delivery.Time)~Sorting.Time+I(Sorting.Time^2),data=del_time)
summary(mod5)#Multiple R-squared:  0.6934,	Adjusted R-squared:  0.6594 #Res: 2.962
sqrt(sum(mod5$residuals^2)/nrow(del_time))#2.742148 rmse

##cubic transformation
mod6<-lm(Delivery.Time~Sorting.Time+I(Sorting.Time^2+I(Sorting.Time^3)),data=del_time)
summary(mod6)#Multiple R-squared:  0.6913,	Adjusted R-squared:  0.657  Res: 2.972

###model overview###
#model_raw=Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 Residual: 2.935
#mod1=     Multiple R-squared:  0.6958,	Adjusted R-squared:  0.6798 Residual: 2.872
#mod2=     Multiple R-squared:  0.6954,	Adjusted R-squared:  0.6794 Residual: 2.873
#mod3=     Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 Residual: 0.1755
#mod4=     Multiple R-squared:  0.704,	Adjusted R-squared:  0.6885 Residual: 0.349
#mod5=     Multiple R-squared:  0.6934,	Adjusted R-squared:  0.6594 Residual: 2.962
#mod6=     Multiple R-squared:  0.6913,	Adjusted R-squared:  0.657  Residual: 2.972

#mod3 is the best model with highR2 value and low residual error, 0.1669628   low rmse
conf<-predict(mod3,interval="confidence")
conf<-as.data.frame((conf))
head(conf)
pred<-predict(mod3,interval="predict")
pred<-as.data.frame(pred)
head(pred)

prediction<-predict(mod3,del_time)
head(prediction)
del_time$del_pred<-prediction
head(del_time)   # no match between actual and predicted

#next better model is model 4
conf1<-predict(mod4,interval="confidence")
conf1<-as.data.frame((conf1))
head(conf1)
pred1<-predict(mod4,interval="predict")
pred1<-as.data.frame(pred)
head(pred1)

predn1<-predict(mod4,del_time)
head(predn1)
del_time$del_pred1<-predn1
head(del_time)  # no match between actual and predicted

#next best model is model1
conf2<-predict(mod1,interval="confidence")
conf2<-as.data.frame((conf2))
head(conf2)
pred2<-predict(mod1,interval="predict")
pred2<-as.data.frame(pred2)
head(pred2)

predn2<-predict(mod1,del_time)
head(predn2)
del_time$del_pred2<-predn2
head(del_time) #actual and predicted are almost same

plot(mod1)
library(ggplot2)

ggplot(data=del_time,aes(x=Delivery.Time,color="actual"))+
  geom_density(aes(x=Delivery.Time,color="actual"))+
  geom_density(aes(x=del_pred2,color="predict"))+
  scale_color_manual(values=c("predict"="blue","actual"="red"))+
  labs(title="density plt between actual del_time and predicted del_time",
       x="del time",y= "")
     
