rm(list=ls())
getwd()
setwd("D:/Tableau/bob ross")
####Performing k-means clustering 
#importing the dataset
data<-read.csv("elements-by-episode.csv")
#rm(headlines)
data

str(data)
summary(data)
library(tidyr)
library(ggplot2)

summarised<- function(thedata){
  result<-thedata%>%
    summarise_all(sum)

result<-gather(result,Attribute,AttributeCount,1:dim(result)[2])
return(result)}
#removing title and episode as they are not required
library(dplyr)
temp<- data%>%
  select(-TITLE,-EPISODE)


print(tbl_df(summarised(temp)),n=Inf)
#obtained the sum of all the elements ever used in all the seasons

#group similar/duplicated objects
categorised<-data%>%
  mutate(MOUNTAIN= ifelse(MOUNTAIN|MOUNTAINS,1,0))%>%
  mutate(MOUNTAIN=ifelse(SNOWY_MOUNTAIN==1,0,MOUNTAIN))%>%
  select(-EPISODE,-TITLE,-GUEST,-contains("FRAME"),
         -PALM_TREES,-TREES,-TREE, -DECIDUOUS,-CONIFER,-DIANE_ANDRE,
         -STEVE_ROSS,-CIRRUS, -CLOUDS, -CUMULUS, -MOUNTAINS)

#finding out the number of clusters
wss<-0
set.seed(18)
for(i in 1:15){
  km<- kmeans(categorised, centers = i, nstart = 20)
  wss[i]<-km$tot.withinss
}
#wss=(within summ of squares)total distance of data points from their respective cluster centroids
plot(1:15,wss,type="b",
     xlab = "Clusters", ylab="WSS")
#bent is between 4&6
#taking k as 5

k<-5
set.seed(18)
km<-kmeans(categorised, centers = k, nstart = 20, iter.max = 50)
categorised$CLUSTER<-km$cluster
#Printing 

for(i in 1:k){
  print(paste("#####Cluster:", i , "#####"))
  summarydata<-categorised%>%
    group_by(CLUSTER)%>%
    filter(CLUSTER==i)
  summarydata<-summarised(summarydata)
  summarydata<-summarydata%>%
    filter(AttributeCount>=20)%>%
    arrange(desc(AttributeCount))
  print(tbl_df(summarydata),n=Inf)
}

#Cluster 1: Rivers and waterfalls
#Cluster 2: Beaches
#Cluster 3: Wintery Scenes
#Cluster 4: Trails and Structures
#Cluster 5: Lakes

data$CLUSTER <- km$cluster

set.seed(18)
taggedData <- data %>%
  group_by(CLUSTER) %>%
  sample_n(3) %>%
  arrange(CLUSTER) %>%
  select(CLUSTER, TITLE)

print(tbl_df(taggedData), n=Inf)
theme_set(theme_light())

