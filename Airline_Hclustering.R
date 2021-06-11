##Perform hierarchical clustering for the airlines data to obtain optimum number of clusters. 
###Draw the inferences from the clusters obtained.

#####HIERARCHICAL CLUSTERING######    Here we use dendrogram

#use readxl package..This is an excel file(xlsx file)
library(readxl)
data<-read_xlsx(file.choose(),2)
View(data)# 3999 obs , 12 variables

#check nullvalues
sum(is.na(data))   #no null values

str(data)

#cc1_miles, cc2_miles, cc3_miles and Award are ordinal categorical variables and which can be converted to numeric by label encoding
#we cannot calculate appropriate euclidean distance for categorical values
#convert these to numeric by taking the average of the respective range.
#range:
#####1 = under 5,000
#####2 = 5,000 - 10,000
#####3 = 10,001 - 25,000
#####4 = 25,001 - 50,000
#####5 = over 50,000
# take the average of range

data$cc1_miles = ifelse(data$cc1_miles==1,2500,
                              ifelse(data$cc1_miles==2,7500,
                                     ifelse(data$cc1_miles==3,17500,
                                            ifelse(data$cc1_miles==4,37500,
                                                   ifelse(data$cc1_miles==5,50000,0)))))
data$cc2_miles = ifelse(data$cc2_miles==1,2500,
                        ifelse(data$cc2_miles==2,7500,
                               ifelse(data$cc2_miles==3,17500,
                                      ifelse(data$cc2_miles==4,37500,
                                             ifelse(data$cc2_miles==5,50000,0)))))
data$cc1_miles = ifelse(data$cc3_miles==1,2500,
                        ifelse(data$cc3_miles==2,7500,
                               ifelse(data$cc3_miles==3,17500,
                                      ifelse(data$cc3_miles==4,37500,
                                             ifelse(data$cc3_miles==5,50000,0)))))

#ignore first column(ID number)
mydata<-data[1:3999,c(2:12)]
View(mydata) 





###Normalization-to make the data scale free& unitless#########(or we use standardisation)
Norm<-scale(mydata)   #mydata got normalised
head(Norm)
summary(Norm)


#####Distance Matrix######
#####Euclidean Distance######
Dist_Matr<-dist(Norm,method = "euclidean")


####Hierarchical Clustering####
#we can use methods like "ward.D", "ward.D2", "single", "complete", "average"
H_clust<-hclust(Dist_Matr,method = "ward.D2")

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
final<-data.frame(mydata,member)   
head(final)

##to get the cluster number as first column..swapping of columns
colnames(final)
final1 <- final[, c(12,1,2,3,4,5,6,7,8,9,10,11)]
head(final1)

final2<-data.frame(data,member)   
head(final2)
final_2 <- final2[, c(1,13,2,3,4,5,6,7,8,9,10,11,12)]
head(final_2)

#inferences:

#save as a csv file
write.csv(final2,file="airline_data.csv")


