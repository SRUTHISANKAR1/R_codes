###Perform Principal component analysis and perform clustering using first 3 principal component scores
#####(both heirarchial and k mean clustering(scree plot or elbow curve) and obtain 
#####optimum number of clusters and check whether we have obtained same number of clusters with the original data(class column we have ignored at the begining who shows it has 3 clusters)df

#########################PCA#############################
wine <- read.csv("C:/Users/server/Downloads/wine.csv")
View(wine)  #178 obs and 14 variables
str(wine)
summary(wine)

#nullvalue checking
sum(is.na(wine))

attach(wine)

#find the correlation. Before doing PCA variables would be correlated
cor(wine)

pca_wine<-princomp(wine,cor=TRUE,scores=TRUE,covmat=NULL)     #covmat is covariance matrix,WHICH IS NEED NOT TO BE CALCULATED HERE
summary(pca_wine)   #we have 14 attributes(columns)in our original data .so 14 PCA produced
str(pca_wine)


#visualization
plot(pca_wine)  #majority of information belongs to First PCA. PCA1 has highest importance(variance)

biplot(pca_wine)  #PCA1 vs PCA2

#here we are considering only first 3 PCA
#top 3 PCA Scores which represents the whole data
pca_wine$scores[,1:3]

#Club(bind )top 3 PCA Scores with original data(uni) by using cbind
Wine<-cbind(wine,pca_wine$scores[,1:3])   #25 obs and 10 variables
View(Wine)  #178 obs and 17 variables with 3 pca scores

######################H_clustering##############################
##prepare data for clustering  by considering 15,16,and 17th column(pca scores in the data)
clus_wine<-Wine[,15:17] #178 obs and 3 variables

#before clustering we need to normalise the data
norm_clus<-scale(clus_wine)
View(norm_clus)

#find the distance to find similarity by using Euclidean distance method
dist1<-dist(norm_clus,method="euclidean")

#Hierarchical clustering
fit1<-hclust(dist1,method="complete")    #complete linkage
#we can use methods like "ward.D", "ward.D2", "single", "complete", "average"
fit2<-hclust(dist1,method="ward.D")
fit3<-hclust(dist1,method="single")
fit4<-hclust(dist1,method="average")


#Dendrogram
plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)

#here we got large number of clusters..we can take limited number clusters by using the function cutree
#get 5 clusters by using cutree(cutting tree into 5 clusters)
#cut the dendrogram using cutree function
groups<-cutree(fit1,5)
groups2<-cutree(fit2,5)

##to identify the 5 clusters
rect.hclust(fit1,k=5,border="red")   #rect.hclust is rectangular cluster
rect.hclust(fit2,k=5,border="red")

###convert the clusters to matrix format to get cluster numbers
member<-as.matrix(groups)
View(member)

#bind the member column with original data by using cbind
final<-cbind(member,Wine)
View(final)  #178OBS AND  18 VAR


##aggregate=Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
###FUN=	a function to compute the summary statistics which can be applied to all data subsets.
View(aggregate(final[,-c(2,16:18)],by=list(member),FUN=mean))

#creat a csv file
write.csv(final,file="wine_clustered",row.names = F,col.names=F)
getwd()



####################K_means clusterning#################
#Elbow curve(scree plot) & elbow point(kvalue=number of clusters. k=sqrt(n/2)
#wss=(nrow(norm_data)-1)*sum(apply(norm_data,2,var))
twss=c()
for(i in 1:15) twss[i]=sum(kmeans(norm_clus,centers=i)$withinss)
plot(1:15,twss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares",main="Scree Plot")#look for an elbow in the scree plot
#elbow point=3

#to get scree plot and elbow point directly by using factoextra function
install.packages("factoextra")
library(factoextra)
fviz_nbclust(norm_clus, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
#We can go for k=3 optimal value of k


