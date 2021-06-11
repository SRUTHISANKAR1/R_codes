####K-means clustering -Airline data set
#we can use several methods to get optimum value for k


library(plyr)

#use readxl package..This is an excel file(xlsx file)
library(readxl)
data<-read_xlsx(file.choose(),2)
View(data)# 3999 obs , 12 variables

#checknull values
sum(is.na(data))   #no null values

str(data)

#ignore first column(ID number)
mydata<-data[,c(2:12)]
View(mydata)

##normalization
norm_data<-scale(mydata)   #mydata got normalised
head(norm_data)
summary(norm_data)

#method1
#kmeans
fit<-kmeans(norm_data,10)   #10 cluster
str(fit)
fit$cluster

#append cluster membership
final1<-data.frame(data,fit$cluster)
head(final1) #new column.

#reorder columns ..last column first..use setcolororder( for Fast column reordering of a data.table by reference)
library(data.table)
setcolorder(final1,neworder = c("fit.cluster"))
head(final1)  #reoredered

##aggregate=Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
###FUN=	a function to compute the summary statistics which can be applied to all data subsets.
aggregate(data[,2:12],by=list(fit$cluster),FUN=mean)
# average SAT of first cluster is 1363.33, similarly avg value of all attributes are calculated for all clusters

#Elbow curve(scree plot) & elbow point(kvalue=number of clusters. k=sqrt(n/2)
#wss=(nrow(norm_data)-1)*sum(apply(norm_data,2,var))
twss=c()
for(i in 1:15) twss[i]=sum(kmeans(norm_data,centers=i)$withinss)
plot(1:15,twss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares",main="Scree Plot")#look for an elbow in the scree plot

#method2
#to get scree plot and elbow point directly by using factoextra function
install.packages("factoextra")
library(factoextra)
fviz_nbclust(norm_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
#We can go for k=3 optimal value of k



#####k-selection-method3
#####K_means clustering- k selection(optimum no:of k) using the function kselection or doParallel####
install.packages("kselection")
library(kselection)   #for the selection of k in k-means clustering

k<-kselection(data[,-1],parallel = TRUE,k_threshold = 0.9,max_centers = 12)
k$max_centers   #12  
k   #finds 2 clusters

####k-selection-method4
# or we can use doParallel function
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4)

#we dont want 1st column.so remove it
k<-kselection(data[,-1],parallel = TRUE,k_threshold = 0.9,max_centers = 12)
k$max_centers    #by using kselection function we got the number of clusters without elbow curve
k   #finds 2 clusters




