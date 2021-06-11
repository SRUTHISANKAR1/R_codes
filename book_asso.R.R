#Association Rules -Apriori algorithm-book.csv

#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visualize the obtained rules using different plots 



install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("datasets")
library(datasets)
library(Matrix)
#load csv file
book<-read.csv("C:/Users/server/Downloads/book (1).csv")
View(book)


str(book)
#check missing values are there or not
is.na(book)
sum(is.na(book))

#convert dataset to matrix format
book<-as.matrix(book)
str(book)


# making rules using apriori algorithm 
## Keep changing support and confidence values to obtain different rules
rules=apriori(book) #7 rules
arules::inspect(rules)  #lhs is Antecident  rhs is consequence

#sort according to Lift
rules.sorted=head(sort(rules,by="lift"),5)
arules::inspect(rules.sorted)
#if a person reads ItalCook then he will definitely read CookBks

#apply apriori with different conditions(parameters)
##Keep changing minimum length, support and confidence values to obtain different rules 

rule1=apriori(book,parameter = list(minlen=1,supp=0.01,conf=0.8))#678 rules
arules::inspect(rule1)
rule1.sorted<-head(sort(rule1,by="lift"),10)
arules::inspect(rule1.sorted)

#visualization
plot(rule1.sorted,method="graph",control=list(nodeCol="red",edgeCol="blue"))
plot(rule1,method="grouped")

rule2<-apriori(book,parameter=list(minlen=3,supp=0.01,conf=0.8))  #674rules
arules::inspect(rule2)

rule3<-apriori(book,parameter = list(minlen=3,supp=0.01,conf=0.7))   #894rules



rule4<-apriori(book,parameter = list(minlen=7,supp=0.01,conf=0.7)) #17rules
arules::inspect(rule4)
rule4.sorted=head(sort(rule4,by="lift"),5)
arules::inspect(rule4.sorted)

#check redundancy in rule4
redundant_rule4<-is.redundant(rule4)
redundant_rule4
summary(redundant_rule4) #no redundancies  17 False

#visualization of rule4
plot(rule4.sorted,method="graph",control=list(nodeCol="red",edgeCol="blue"))
plot(rule4,method="grouped")
