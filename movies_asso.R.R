#Association Rules -Apriori algorithm- mymovies

#Prepare rules for the all the data sets 
#1) Try different values of support and confidence. Observe the change in number of rules for different support,confidence values
#2) Change the minimum length in apriori algorithm
#3) Visualize the obtained rules using different plots 


install.packages("arules")
library(arules)   #for market basket analysis
install.packages("arulesViz")   # visualisation
library(arulesViz)
install.packages("datasets")
library(datasets)
install.packages("Matrix")
library(Matrix)

#load data 
####while loading make heading =yes
movies<- read.csv("C:/Users/server/Downloads/my_movies (2).csv")
View(movies)
#remove first 5 columns
movies=movies[,-c(1:5)]
View(movies)
str(movies)

#convert dataset to matrix format
movies<-as.matrix(movies)
str(movies)

#check missing values are there or not
is.na(movies)


##Apply Apriori algorithm to get frequent items from the dataset
rules <- apriori(movies)   # 77rules  #minlen=1,supp=0.01,conf=0.8
arules::inspect(rules)
rules.sorted<-head(sort(rules,by='lift'),5)
arules::inspect(rules.sorted)

#apply apriori with different conditions(parameters)
##Keep changing minimum length, support and confidence values to obtain different rules 
rule1<-apriori(movies,parameter=list(minlen=1,supp=0.001,conf=0.8))
arules::inspect(rule1)  #77rules
rule1.sorted<-head(sort(rule1,by="lift"),5)
arules::inspect(rule1.sorted)

#visualization of rule
plot(rule1.sorted,method="graph",control=list(nodeCol="red",edgeCol="blue"))
#If a person watches Harry Potter1 then he will definitely watch Harry Potter2
#If a person watches LOTR1 then he will definitely watch LOTR2

plot(rule1,method="grouped")
plot(rule1.sorted,method="grouped")  
plot(rule1.sorted,method = "scatterplot")
plot(rule1.sorted,method = "graph")

#check redundancies
reduntant_rule1<-is.redundant(rule1)
reduntant_rule1
summary(reduntant_rule1)#28 False,49 True(49 repeating rules)
#remove redundancy
rule1<-rule1[!redundant_rule1]
rule1  #redundancy removed.. only 28rules


#Keep changing support and confidence values to obtain different rules

rule2<-apriori(movies,parameter=list(minlen=2,supp=0.001,conf=0.8))
arules::inspect(rule2)  #77rules
rule2.sorted<-sort(head(rule1,by="lift"),5)
arules::inspect(rule2.sorted)

rule3<-apriori(movies,parameter=list(minlen=1,supp=0.001,conf=0.7))
arules::inspect(rule3)  #79rules
rule3.sorted<-head(sort(rule3,by="lift"),5)
arules::inspect(rule3.sorted)

rule4<-apriori(movies,parameter=list(minlen=3,supp=0.0001,conf=0.7))#65 rules
arules::inspect(rule4)
rule4.sorted<-head(sort(rule4,by="lift"),5)
arules::inspect(rule4.sorted)

rule5<-apriori(movies,parameter=list(minlen=4,supp=0.01,conf=0.8)) #29rules
arules::inspect(rule5)
rule5.sorted<-head(sort(rule5,by="lift"),5)
arules::inspect(rule5.sorted)
rule5.sorted<-head(sort(rule5,by="lift"),5)
arules::inspect(rule5.sorted)

# example=if Sixth.Sense,Gladiator,Green.Mile then LOTR

#check for redundancy
redundant_rule5<-is.redundant(rule5)
redundant_rule5
summary(redundant_rule5)  #24 False, 5 True(5 redundancies)
#remove redundancy
rule5<-rule5[!redundant_rule5]
rule5   #24rules

#visualisation of rule5
plot(rule5.sorted,method='graph',control=list(nodeCol="red",edgeCol="blue"))

plot(rule5,method="grouped")


rule6<-apriori(movies,parameter=list(minlen=5,supp=0.001,conf=0.8))#5 rules
rule7<-apriori(movies,parameter=list(minlen=6,supp=0.01,conf=0.8))# zero rule

