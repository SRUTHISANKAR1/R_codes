BuyerRatio <- read.csv("C:/Users/server/Downloads/BuyerRatio (2).csv")
View(BuyerRatio)
#business problem: Sales of products in four different regions is tabulated for males and females.
#Find if male-female buyer ratios are similar across regions.

#discrete data with  more than 2 proportions(east,west,south,north- so we can use Chi_Squared test
#here H0  buyer ratio of male across the region= buyer ratio of female across the region
#Ha is buyer ratio of male across the region not equal to buyer ratio of female across the region

#swap rows and columns . Then remove first row
df <- data.frame(t(BuyerRatio[-1]))
colnames(BuyerRatio) <- BuyerRatio[, 1]
attach(df)
table(Males,Females)
#Chi-square test
chisq.test(table(Country,Defective))

# here 3 is degrees of freedom=((n-1),that is here we have 4 regions .so 4-1=3, significance 0.05))
#0.6315 is p value 
##X2 from table is 7.81 but here we got 1.72.(means p value >0.05, so Ho fly) x2 from table> calculated x2 so H0 fly
##accept null hypothesis

