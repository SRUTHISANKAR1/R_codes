Cutlets<- read.csv("C:/Users/server/Downloads/Cutlets (1).csv")
View(Cutlets)
attach(Cutlets)
#35 observations 2variables
# continuous data  with 2 proportions to compare
is.na(Cutlets$Unit.A)
is.na(Cutlets$Unit.B)

#Ho is  Diameter of cutlets of Unit.A=Unit.B,No significant difference
#Ha is Diameter of Cutlets of Unit A not equal to Cutlets of Unit B. There is a significant difference

#step1
#Normality test=shapiro test
shapiro.test(Unit.A)  #p-value = 0.32    greater than 0.05 Ho fly
shapiro.test(Unit.B)  #p-value = 0.5225  greater than 0.05 Ho fly
#Ho= y is normally distributed
#Ha =y is not distributed normally
# Unit.A and Unit.B are normally distributed


#STEP2
#Check external conditions are same or not
#here external conditions are not same. both units may be different
#so we cannot go for paired T test

#Step3
#next we need to check are variance equal or not?
#Ho is- variances are equal
#Ha- 
var.test(Unit.A,Unit.B)#p-value = 0.3136
#P value greater than 0.05. high p, Ho fly...equal variance

#############2 sample T Test ##################

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
#p-value = 0.4723    greater than 0.05
#P high, Ho fly

##conclusion:
#There is no significant difference between cutletsof two units

