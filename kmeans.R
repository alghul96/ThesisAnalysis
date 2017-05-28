# install.packages("flexclust")
#library(stats)
library(flexclust)
library(cluster)

#### K-MEANS EUCLIDEAN CLUSTERING ####

# Creating the kmeans object
# kmeansClusters = kmeans(clusterdataf, centers = 5)

kmeansClusters = stepFlexclust(clusterdataf, k = 3, nrep = 10000, FUN = cclust, multicore = TRUE)

summary(kmeansClusters)

kcenters = kmeansClusters@centers 
daisy(kcenters, metric = "manhattan") # distances beetween the cluster centers

kgroup_3 = kmeansClusters@cluster


#### DIFFERENT K k-MEANS CLUSTERING ####

### For 2 to 15 clusters 
kmeansClusters_rep = stepFlexclust(clusterdataf, k = 2:8, nrep = 600, FUN = cclust, multicore = TRUE)


#########################################
#### Choosing the number of clusters ####
#########################################

plot(kmeansClusters_rep) # from the within sum of squares, we see that it become stable after 2/3 clusters 

par(mfrow = c(1,3))
for(i in 1:3){
  print(cluster.stats(distances, kmeansClusters_rep[[i]]@cluster))
  plot(silhouette(kmeansClusters_rep[[i]]@cluster, distances), cex.names=0.6)
}

# according to the average silhouette, we choose three clusters