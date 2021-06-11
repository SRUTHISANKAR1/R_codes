#Association Rules -Apriori algorithm- groceries

install.packages("arules")
library(arules)   #for market basket analysis
install.packages("arulesViz")   # visualisation
library(arulesViz)
install.packages("datasets")
library(datasets)
install.packages("Matrix")
library(Matrix)

#load data

groceries<-read.transactions("C:/Users/server/Downloads/groceries (2).csv")
View(groceries)
str(groceries)

# itemFrequencyPlot can be applicable only for transaction data 
itemFrequencyPlot(groceries,topN=20)



#Apply Apriori algorithm to get frequent items from the dataset
rules<-apriori(groceries)  #confidence=0.8, support=0.1, minlen=1, maxlen=10
arules::inspect(rules)   # 0 rule
rules.sorted<-sort(rules,by="lift")
arules::inspect(rules.sorted)

#apply apriori with different conditions(parameters)
rule1=apriori(groceries,parameter = list(minlen=1,supp=0.01,conf=0.8))
arules::inspect(rule1)  #7 rules
rule1.sorted<-sort(rule1,by="lift")
arules::inspect(rule1.sorted)   #we got several combinations which satisfy these conditions
#lhs is antecident and rhs is consequent
#for example {canned}  => {beer}
#if canned then beer

#check the redundancy (repeat some combinations)
redundant_rule1<-is.redundant(rule1)
redundant_rule1#1 rule is repeated =shows "TRUE"
summary(redundant_rule1)  #5 False, 2 TRUE

#remove redundancy
rule1<-rule1[!redundant_rule1]
rule1   #redundancy removed   5 rules

#visualization of rule1
rule1.sorted=head(sort(rule1,by="lift"),5)
plot(rule1.sorted,method="graph",control=list(nodeCol="red",edgeCol="blue"))

plot(rule1.sorted,method="grouped")   #or plot(rule1,method="grouped"
plot(rule1.sorted,method = "scatterplot")
plot(rule1.sorted,method = "graph")

#Keep changing support and confidence values to obtain different rules
#condition 2
rule2=apriori(groceries,parameter = list(minlen=1,supp=0.001,conf=0.8)) #196 rules
arules::inspect(rule2)  
rule2.sorted<-head(sort(rule2,by="lift"),10)
arules::inspect(rule2.sorted)

redundant_rule2<-is.redundant(rule2)
redundant_rule2# 120 rules are repeated =shows "TRUE"
summary(redundant_rule2)  #76 False, 120TRUE
rule2<-rule2[!redundant_rule2]
rule2


#visualization of rule1
rule2.sorted=head(sort(rule2,by="lift"),10)
plot(rule2.sorted,method="graph",control=list(nodeCol="red",edgeCol="blue"))



#condition3
rule3=apriori(groceries,parameter = list(minlen=1,supp=0.01,conf=0.8)) #7 rules
arules::inspect(rule3)  
rule3.sorted<-head(sort(rule3,by="lift"),10)
arules::inspect(rule3.sorted)

redundant_rule3<-is.redundant(rule3)
redundant_rule3#  2rules are repeated =shows "TRUE"
summary(redundant_rule3)  #5 False,  2TRUE
rule3<-rule3[!redundant_rule3]
rule3

#visualization of rule1
rule3.sorted=sort(rule3,by="lift")
plot(rule3.sorted,method="graph",control=list(nodeCol="red",edgeCol="blue"))
#similarly by changing the values of supp and confidence we can create several rules




#Change minimum length in Apriori algorithm
#first
rule1_minlength=apriori(groceries,parameter = list(minlen=2,supp=0.01,conf=0.8))
arules::inspect(rule1_minlength)  #7 rules
rule1.sorted1<-sort(rule1_minlength,by="lift")
arules::inspect(rule1.sorted1)
#second
rule1_length=apriori(groceries,parameter = list(minlen=5,supp=0.01,conf=0.8))
arules::inspect(rule1_length)  #2 rules    
rule1_length<-sort(rule1_length,by="lift")
arules::inspect(rule1_length)
#if minlength=4 then 0 rules
#if minlength=5 then zero rule


