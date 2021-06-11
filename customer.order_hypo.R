Customer_orderform<-read.csv("C:/Users/server/Downloads/Costomer+OrderForm (1).csv")
View(Customer_orderform)

#300 observations 4variables
# Discrete and more than two proportions to compare
#Chi-square test
## here we need to check whether the defective percentage of costomer order forms varies by centre or not
#here Ho is - Defective percentage of all centres  are equal
#Ha is- Defective percentage varies

attach(Customer_orderform)

#stack data- Data stacking involves splitting a data set up into smaller data files, and stacking the values for each of the variables into a single column
stacked_Cust<-stack(Customer_orderform)
View(stacked_Cust)
attach(stacked_Cust)
table(values,ind)

chisq.test(table(values,ind))#Chi_square test
chisq.test()#p-value = 0.2771
# p value greater than 0.05. P high null fly

#Conclusion:Defective percentage of all centres  are equal

