# Emp_data -> Build a prediction model for Churn_out_rate 

emp_data <- read.csv("C:/Users/server/Downloads/emp_data (1).csv")
View(emp_data)
library(lattice)
library(psych)  
summary(emp_data)
is.null(0)
attach(emp_data)


####################EDA####################
###Data Visualization##

dotplot(emp_data$Salary_hike,main="Salary_hike")
dotplot(emp_data$Churn_out_rate,main="Churn-rate")

boxplot(emp_data$Salary_hike,main="Salary_hike",col="dodgerblue4",horizontal = T)#no outliers
boxplot(emp_data$Churn_out_rate,main="Churn-rate",col="dodgerblue4",horizontal = T)#no outliers

hist(emp_data$Salary_hike,main="Salary_hike",col="dodgerblue4")#not normal
hist(emp_data$Churn_out_rate,main="Churn-rate",col="dodgerblue4")#not normal

plot(emp_data$Salary_hike,emp_data$Churn_out_rate,main="scatter plot",xlab="Salary_hike",ylab="Churn_out_rate")
#linear,negative 

qqnorm(Salary_hike)
qqline(Salary_hike)#normal
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)# not normal

##normality check-shapiro test
shapiro.test(Salary_hike)#p-value = 0.5018  from p value its Normal
shapiro.test(Churn_out_rate)# p-value = 0.7342 from p value its Normal

#correlation analysis
cor(Salary_hike,Churn_out_rate)#-0.9117216  strong negative correlation
plot(Salary_hike,Churn_out_rate) #linear negative correlation


##### model building####
#linear regression model
model_raw<-lm(Churn_out_rate~Salary_hike,data=emp_data)
summary(model_raw)  #Slary_hike is significant
#Residual standard error: 4.469 on 8 degrees of freedom
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101   Good value
###RMSE###
sqrt(sum(model_raw$residuals^2)/nrow(emp_data))  ##3.997528

plot(model_raw)#two extreme values 1$10

###outlier removal
new<-emp_data[-10,]
View(new_emp)
new_emp<-new[-1,]
View(new_emp)
model1<-lm(Churn_out_rate~Salary_hike,data=new_emp)
summary(model1)
###Multiple R-squared:  0.9049,	Adjusted R-squared:  0.889 ##Residual: 2.552 
#R2 value improved

###RMSE###
sqrt(sum(model1$residuals^2)/nrow(new_emp))   #2.210328


conf<-predict(model1,interval="confidence")
conf<-as.data.frame((conf))
pred<-predict(model1,interval="predict")
pred<-as.data.frame(pred)  #prediction interval wider than confidence interval

prediction<-predict(model1,new_emp)
head(prediction)
new_emp$churn_pred<-prediction
head(new_emp)

model2<-lm(Churn_out_rate~log(Salary_hike),data=emp_data)
summary(model2) #Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 #Res: 4.233

model3<-lm(log(Churn_out_rate)~Salary_hike,data=emp_data)
summary(model3)#Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8577 Res: 0.0519 


model4<-lm(Churn_out_rate~sqrt(Salary_hike),data=emp_data)
summary(model4)#Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9662 Res: 1.886

model5<-lm(Churn_out_rate~Salary_hike+I(Salary_hike^2),data = emp_data)
summary(model5)#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101  Res: 4.469

model6<-lm(Churn_out_rate~Salary_hike+I(Salary_hike^2)+I(Salary_hike^3),data=emp_data)
summary(model6)#Multiple R-squared:  0.9893,	Adjusted R-squared:  0.984 Res: 1.298 
sqrt(sum(model6$residuals^2)/nrow(emp_data))  #1.0052
pred<-predict(model6,emp_data)
head(pred)
emp_data$churn_pred<-pred
head(emp_data)
#Model6 is the best model

#################models overview###################
#model_raw  Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 Residual : 4.469 
#model1     Multiple R-squared:  0.9049,	Adjusted R-squared:  0.889  Residual: 2.552
#model2     Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 Residual: 4.233
#model3     Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8577 Residual: 0.0519
#model4     Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9662 Residual: 1.886
#model5     Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 Residual: 4.469
#model6     Multiple R-squared:  0.9893,	Adjusted R-squared:  0.984  Residual: 1.298
plot(model6)

library(ggplot2)
ggplot(data=emp_data,aes(x=Churn_out_rate,color="actual"))+
  geom_density(aes(x=Churn_out_rate,color="actual"))+
  geom_density(aes(x=churn_pred,color="predict"))+
  scale_color_manual(values=c("predict"="blue","actual"="red"))+
  labs(title="density plt between actual churn_out_rate and predicted churn_out_rate",
       x="churn_out",y= "")
