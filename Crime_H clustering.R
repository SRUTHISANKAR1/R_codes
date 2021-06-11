#problem statement:Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.
#Data Description:
#Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States




#####HIERARCHICAL CLUSTERING######    Here we use dendrogram


crime_data <- read.csv("C:/Users/server/Downloads/crime_data.csv")
View(crime_data)
###50 universities 5variables

#here first column is different places of United States.which is not relevent..we are not taking it
mydata<-crime_data[1:50,c(2:5)]
View(mydata)  


###Normalization-to make the data scale free& unitless#########(or we use standardisation)
Norm<-scale(mydata)   #mydata got normalised
head(Norm)
summary(Norm)

#####Distance Matrix######
#####Euclidean Distance######
Dist_Matr<-dist(Norm,method = "euclidean")

####Hierarchical Clustering####
H_clust<-hclust(Dist_Matr,method = "complete")   #complete linkage


######VISUALISATION#####
######DENDROGRAM#######
plot(H_clust)
plot(H_clust,hang = -1)  #to get the dendrogram in a sequence of order.....not necessary to do this

#here we got large number of clusters..we can take limited number clusters by using the function cutree
#get 5 clusters by using cutree(cutting tree into 5 clusters)
groups<-cutree(H_clust,k=5)
##to identify the 5 clusters
rect.hclust(H_clust,k=5,border="red")   #rect.hclust is rectangular cluster

##convert the clusters to matrix format
member<-as.matrix(groups)

##merge matrix with original data
final<-data.frame(mydata,member)   #13.2 in first cluster,10 belongs to 2nd cluster and so on
head(final)

finall<-data.frame(crime_data,member)   #13.2 in first cluster,10 belongs to 2nd cluster and so on
head(finall)
     
##to get the cluster number as first column..swapping of columns
colnames(final)
final1 <- final[, c(5,1,2,3,4,)]
head(final1)


colnames(finall)
final2 <- finall[, c(6,1,2,3,4,5)]
head(final2)

#inferences:
#Alabama,Georgia,Louisiana,Mississippi,North Carolina,South Carolina&Tennessee belong to first cluster.
#Murder,Assualt,urban population &Rape rate are almost similar in the above places

#save as a csv file
write.csv(final2,file="final_crime_data.csv")

