labTAT<-read.csv("C:/Users/server/Downloads/LabTAT (1).csv", header=TRUE)
View(labTAT)
attach(labTAT)
#continuous data with 4 proportions to compare
#121 obs and 4 variables

#here Ho is average TAT among different laboratories are equal
#Ha is average TAT among different laboratories are different

##check normality by using Shapiro test
#p value>0.05 normal(Ho), p<0.05 not normal(Ha)
shapiro.test(Laboratory.1) #0.5508
shapiro.test(Laboratory.2) #0.8637
shapiro.test(Laboratory.3) #0.4205
shapiro.test(Laboratory.4) #0.6619

#stacked data
Stacked_Data <- stack(labTAT)
View(Stacked_Data)#480 0bs with 2 variables
attach(Stacked_Data)
shapiro.test(values)# p-value = 0.1175  greater than 0.05 so normal

##check variances are equal or not
#Compare the variances of k samples, where k can be more than two samples(here we have 4 samples-lab1,2,3,4)
library(car)
leveneTest(Stacked_Data$values~Stacked_Data$ind, data = Stacked_Data)#p value=0.05161

################# One-way Anova ########(ANALYSIS OF VARIANCE)
#used if only one attribute.. comparing samples on the basis of TAT
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)# p value less than 0.05 Reject Ho, accept Ha

#conclusion: accept Ha. So average TAT among different laboratories are different

