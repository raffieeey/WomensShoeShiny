source("cleaning function.R")
library('caTools')
library("cluster")
library("data.table")

library(rgl)

#data72<-fread("7210_1.csv")

#id_price<- data72 %>% group_by(id)%>% summarise(price.avg = mean(prices.amountMax, na.rm = TRUE))

#id_price<-as.data.frame(id_price)

#combined_data<-merge(data72, id_price, by="id", all.x=TRUE)
#write.csv(combined_data,"women shoe.csv")


cleaned_data<-clean("7210_1.csv")
View(head(cleaned_data))
dataset<- cleaned_data[9:11]
plot(dataset)

split = sample.split(dataset$weight_KG,SplitRatio = 0.8)
training_set = subset(dataset,split==TRUE)
testing_set = subset(dataset,split==FALSE)

wcss = vector()
#sum of square within clusters
#find ssq for 1 to 10 clusters
for (i in 1:10)wcss[i]=sum(kmeans(dataset,i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = ('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


kmeans = kmeans(x = dataset,centers = 6)
y_kmeans = kmeans$cluster

newdf <- data.frame(dataset, K=kmeans$cluster)#Include the number of cluster in the data set
pcdf <- princomp(dataset,cor=T,score=T)
summary(pcdf)#Compute the validity of each component/dimension
plot3d(pcdf$scores, col=newdf$K)#Create a 3D plot

