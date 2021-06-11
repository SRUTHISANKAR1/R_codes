#######################MULTIPLE LINEAR REGRESSION####################
################TOYOTA COROLLA DATA SET#################################
#PS:Consider only the below columns and prepare a prediction model for predicting Price.

#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
corolla=read.csv(file.choose())
View(corolla)
#1436obs and38 variables
attach(corolla)
Corolla<-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")] #take the relevent variables
View(Corolla)
#Here target variable is Price

summary(Corolla)   #get the descriptive statistics
colnames(Corolla)
str(Corolla)


sum(is.na(Corolla))   #zero null values


#pairplot
pairs(Corolla)  #from the pair plot I can understand that price mailny depends on Age_08_04


#correlation matrix
cor(Corolla)
#price and age:there is a strong negative correlation -0.87
#there is no collinearity problem


#Visualization
hist(Price,main="Price",color="blue") #positively skewed
hist(Age_08_04,main="Age_08_04",color="red")   #negatively skewed
hist(KM,main="KM",color="blue") #positively skewed
hist(HP,main="HP",color="blue") #negatively skewed
hist(cc,main="cc",color="blue") #not enough data
hist(Doors,main="Doors",color="blue") 
hist(Quarterly_Tax,main="Quarterly_Tax",color="blue") 
hist(Weight,main="Weight",color="blue")


#Boxplot
boxplot(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight,col=c("red","orange"),main="Boxplot",names=c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight"))
#except Doors all others have outlier


#create linear model using all the variables
model1<-lm(Price~.,data=Corolla)
summary(model1) #low R2 value,High RMSE
#cc and Doors are not significant pvalue>0.05


#we can chack whether cc and Doors are individually contributing(significant)to output
m1<-lm(Price~cc,data=Corolla)
summary(m1)   #cc is significant
m2<-lm(Price~Doors,data=Corolla)
summary(m2)  #significant
cor(cc,Doors) #no multicollinearity  #0.0799033
#Build a model with cc+Doors
m3<-lm(Price~cc+Doors,data=Corolla)
summary(m3)  #Both are significant


##Partial correlation analysis- install library corpcor
install.packages("corpcor")
library(corpcor)
cor(Corolla)
cor2pcor(cor(Corolla))
##correlation-  correlation between two variables affected by other variables
#but in Partial correlation is pure correlation between two variables
#no collinearity problem

vif(model1)  #no variable has vif>20 so no collinearity problem

#Assumptions_model1
#model should be linear
#error should be IID normal. check scedasticity(variance constatnt or not)
install.packages("car")
library(car)
plot(model1)  #residual vs fitted there is a pattern .so model is not good


##2.errors need to be IID normal(independently and identically distributed
#Model deletion diagnostics-cook's distance, influential plot(High leverage)
qqPlot(model1,id.n=5,main="qqplot")
#from residual vs leverage plot, 222 and 81 can be considered as outliers(out ofthe range of cooks distance )
influence.measures(model1)
influenceIndexPlot(model1,id.n=3)
#from cook's distance and hat_values it is visible that 81 is an outlier
influencePlot(model1,id.n=3)  #81 is an influencer/outlier.remove it


##create a linearmodel by deleting 81st observation
model2<-lm(Price~.,data=Corolla[-81,])
summary(model2)  #R2 value improved 0.8694   R2 value>0.8 is considered as best model
#RMSE also reduced
#now cc became significant
#Doors still insignificant

##Added variable plot -to check whether "Doors" has to be removed or not
avPlots(model2)
#avPlot between Price and Doors is perfectly parallel. so which has no effect on price .remove it
model3<-lm(Price~Price+Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=Corolla[-81,])
summary(model3)  #no change in R2 value

plot(model3) #remove 222nd oservation also 
model3<-lm(Price~Price+Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=Corolla[-81,-222])
summary(model3)  #no change in R2 value


#Akaike test-lower the Akaike value,better is the model
library('MASS')
stepAIC(model1)    #AIC=20693.89   #high Akaike value
stepAIC(model2)   #AIC=20613.41
stepAIC(model3)   # AIC=20613.41
#model2 and model3 have almost same Akaike value