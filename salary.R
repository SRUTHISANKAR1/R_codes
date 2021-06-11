#predict Salary with years of experience
Salary_data <- read.csv("C:/Users/server/Downloads/Salary_Data.csv")
View(Salary_data)
library(lattice)
library(psych)
attach(Salary_data)
summary(Salary_data)
#x=Years of experience, y=Salary

###EDA###
#Data visualization-dotplot,boxplot,histogram,scatterplot
dotplot(Salary_data$YearsExperience,main="Years")
dotplot(Salary_data$Salary,main="Salary")  ## cant find any outliers

boxplot(Salary_data$YearsExperience,main="Years",col="yellow",horizontal = T)#no outliers
boxplot(Salary_data$Salary,main="Salary",col = "yellow",horizontal = T)#no outliers

hist(Salary_data$YearsExperience,main="Years",col="dodgerblue4")
hist(Salary_data$Salary,main="Salary",col="dodgerblue4")

plot(Salary_data$YearsExperience,Salary_data$Salary,main="scatter plot",xlab="YearsExperience",ylab="Salary")#linear positive
qqnorm(Salary,main="Salary")
qqline(Salary,main="Salary")
qqnorm(YearsExperience,main="Years")
qqline(YearsExperience,main="Years")# I think its not normal

#Shapiro test for normality check
shapiro.test(YearsExperience)##p-value = 0.1034  Normal
shapiro.test(Salary)     # p -value = 0.01516   not normal

###correlation-cor(). for understanding if there is strong relation between dependent and independent variable
cor(YearsExperience,Salary)#0.9782416  High correlation
plot(YearsExperience,Salary,main="cor",col="red")


#model building
#create linear regression model using lm function
#name<-lm(Y~X)#dependent variable~independent variable
###first model
model_raw<-lm(Salary~YearsExperience,data=Salary_data)
summary(model_raw)#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554
#Residual standard error: 5788
#good R2 and adj R2 values
#Salary= 25792.2+9450(YearsExperience)
plot(model_raw)


##Confidence interval
confint(model_raw,level=0.95)
conf<-predict(model_raw,interval="confidence")
conf<-as.data.frame((conf))
head(conf)
new.data<-data.frame(YearsExperience=7.5)
predict(model_raw,newdata = new.data,interval="confidence")

#prediction interval
pred<-predict(model_raw,interval="predict")
pred<-as.data.frame(pred)
head(pred)
new<-data.frame(YearsExperience=7.5)
predict(model_raw,newdata=new,interval="predict") #prediction interval wider than conf interval

####RMSE=sqrt[(sum((error)^2))/n
sqrt(sum(model_raw$residuals^2)/nrow(Salary_data))# 5592.044

  

 ###transformations to get better model to get higher R2 value,hence better model###
#model2:sqrt transformation
mod_sqrt<-lm(Salary~sqrt(YearsExperience),data=Salary_data)
summary(mod_sqrt)#Multiple R-squared:  0.931,	Adjusted R-squared:0.9285 reduced from raw model
#Residual standard error: 7329
#sqrt(YearsExperience  issignificant

#model3:logarithmictransformation
mod_log<-lm(Salary~log(YearsExperience),data=Salary_data)
summary(mod_log)#Multiple R-squared:  0.8539,	Adjusted R-squared:  0.8487  again reduced
#Residual standard error: 10660
#bad model

#model4exponential transformation
mod_exp<-lm(log(Salary)~YearsExperience,data=Salary_data)
summary(mod_exp)# less than raw model, Multiple R-squared:0.932,Adjusted R-squared:  0.9295 
#Residual standard error: 0.09789

#model5:polynomial transformation
mod_poly<-lm(log(Salary)~YearsExperience+I(YearsExperience*YearsExperience),data=Salary_data)
summary(mod_poly)#Multiple R-squared:  0.9486,	Adjusted R-squared:  0.9448  Residual: 0.08664 

#model6:cubic model
mod_cub<-lm(Salary~YearsExperience+I(YearsExperience*YearsExperience)+I(YearsExperience^3),data=Salary_data)
summary(mod_cub)#Multiple R-squared:  0.9636,	Adjusted R-squared:  0.9594 Resi: 5524
#model with betterR2 and adjusted R2 value buthigh residual error

plot(mod_cub) #7 and 20 are extreme values
hist(mod_cub$residuals)
hist(model_raw$residuals) #normal distribution
sqrt(sum(mod_cub$residuals^2)/nrow(Salary_data)) #5142.642    #reduced from 5592.044

#(model_raw) Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554, res= 5788
#(mod_sqrt)  Multiple R-squared:  0.931,	Adjusted R-squared:  0.9285  Res: 7329
#(mod_log)   Multiple R-squared:  0.8539,	Adjusted R-squared:  0.8487  Res: 10660
#(mod_exp)   Multiple R-squared:  0.932,  Adjusted R-squared:  0.9295  Res: 0.09789
#(mod_poly)  Multiple R-squared:  0.957,	Adjusted R-squared:  0.9448  Res: 0.08664
#(mod_cub)   Multiple R-squared:  0.9636,	Adjusted R-squared:  0.9594 Resi: 5524


conf1<-predict(mod_cub,interval="confidence")
conf1<-as.data.frame((conf))
head(conf1)
confint(mod_cub,level=0.95)
new.data<-data.frame(YearsExperience=7.5)
predict(mod_cub,newdata = new.data,interval="confidence")
predict(mod_cub,interval = "predict")


pred1<-predict(mod_cub,Salary_data)
head(pred1)
Salary_data$salary_pred<-pred1
head(Salary_data)
plot(mod_cub)
#better model


pred2<-predict(model_raw,Salary_data)
head(pred2)
Salary_data$salary_pred1<-pred2
head(Salary_data)
library(ggplot2)
ggplot(data=Salary_data,aes(x=salary,color="actual"))+
  geom_density(aes(x=salary,color="actual"))+
  geom_density(aes(x=salary_pred1,color="predict"))+
  scale_color_manual(values=c("predict"="blue","actual"="red"))+
  labs(title="density plt between actual salary and predicted salary",
       x="Salary",y= "")


