library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)
#library(flipMultivariates)
library(psych)
library(rstudioapi)
library(rstatix)
library(tictoc)
library(cowplot)
library(readr)

# Set seed for reproducability
set.seed(42)

# Set working directory using RStudio API
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


data.all<-read.csv('Data/CleanedData_AllTracks.csv')


# Set data types
data.all$X <- NULL
data.all$X.1 <- NULL
#data$label <- as.factor(data$label)
data.all$playlistID <- as.factor(data.all$playlistID)
data.all$TrackID <- as.factor(data.all$TrackID)
data.all$mode <- as.factor(data.all$mode)

# deduplicate


dupIndex <- duplicated(data.all[,c('TrackID')])
data.unique <- data.all[!dupIndex,]


data <- data.unique

# Do K-means on the de-duplicated sleep data ----


# Sub divisions for Clustering
dataForClustering <- data[,c(15:23)]
infoForClustering <- data[,c(11)]


#Feature scaling
dFC.s = scale(dataForClustering, center = TRUE, scale = TRUE)

#Code to use if ever we want to scale back
dFC.orig = t(apply(dFC.s, 1, function(r)r*attr(dFC.s,'scaled:scale') + attr(dFC.s, 'scaled:center')))
summary(dataForClustering)
summary(dFC.s)

# Using the elbow method to find the optimal number of clusters


wcss = vector()
kClusters = list()

# reducing to k-max = 10 here, as we have no reason to believe there would be more
for (i in 1:10) {
  thisK <- kmeans(x = dFC.s, centers = i, iter.max=20000)
  #wcss[i] = sum(kmeans(dFC.s, i, iter.max=1000)$withinss)
  wcss[i] = sum(thisK$withinss)
  kClusters[[i]] = t(apply(thisK$centers, 1, function(r)r*attr(dFC.s,'scaled:scale') + attr(dFC.s, 'scaled:center')))
}


plot(1:10,
     wcss,
     type = 'b',
     main = paste('Selection of most suitable k'),
     xlab = 'k',
     ylab = 'Within-cluster sum-of-squares')


# Going for cluster plots instead
library(factoextra)
thisK <- kmeans(x = dFC.s, centers = 2, iter.max=20000)
plotForK2 <- fviz_cluster(thisK, data=dFC.s)
plotForK2




thisK <- kmeans(x = dFC.s, centers = 3, iter.max=20000)
plotForK3 <- fviz_cluster(thisK, data=dFC.s)
plotForK3

thisK <- kmeans(x = dFC.s, centers = 4, iter.max=20000)
plotForK4 <- fviz_cluster(thisK, data=dFC.s)
plotForK4


thisK <- kmeans(x = dFC.s, centers = 5, iter.max=20000)
plotForK5 <- fviz_cluster(thisK, data=dFC.s)
plotForK5

thisK <- kmeans(x = dFC.s, centers = 8, iter.max=20000)
plotForK8 <- fviz_cluster(thisK, data=dFC.s)
plotForK8


write(wcss, file='Data/kMeansWCSS.csv')
# Why did we choose 3 in the end? Is that dependent on the results of the wcss?

# Fitting K-Means to the dataset
kmeans = kmeans(x = dFC.s, centers = 3, iter.max=20000)
kmeans.labels = kmeans$cluster

write(kmeans$centers, file='Data/kMeans_centers_k3.csv')

data$clusterID <- kmeans.labels
data$clusterID<- as.factor(data$clusterID)

# Section to save data.unique, so that we have all the data.
write.csv(data, 'Data/dataAllTracks_unique_k3.csv')
# Uncomment below if want to read in
#data.unique <- read.csv('dataUniqueWithClusters3.csv')

# Add descriptive stats per K-means ----

descriptiveClusters <- describeBy(data[c(15:23)], data$clusterID, IQR=TRUE, fast=FALSE)
write.csv(descriptiveClusters[1], 'Data/c1_Descriptive.csv')
write.csv(descriptiveClusters[2], 'Data/c2_Descriptive.csv')
write.csv(descriptiveClusters[3], 'Data/c3_Descriptive.csv')


# Now make a table with median values per cluster
mC1 <- descriptiveClusters[1]$'1'
mC1 <- t(t(mC1$median))
mC2 <- descriptiveClusters[2]$'2'
mC2 <- t(t(mC2$median))
mC3 <- descriptiveClusters[3]$'3'
mC3 <- t(t(mC3$median))



medianDF <- data.frame(mC1)
medianDF$cluster2 <- mC2
medianDF$cluster3 <- mC3



colnames(medianDF) <- c('Cluster1', 'Cluster2', 'Cluster3')

rownames(medianDF) <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness',
                        'liveness', 'valence', 'tempo')

write.csv(t(medianDF), 'Data/ClustersMedian3.csv')
