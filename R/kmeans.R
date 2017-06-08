# install.packages("flexclust")
#library(stats)
library(flexclust)
library(cluster)
library(fpc)

clusterdataf = datafexam # select the dataframe to use for clustering


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



#####################################
##### ON REDUCED DATAFRAME ##########
#####################################

library(flexclust)
library(fpc)
library(cluster)

clusterdataf = datafexam_reduced
kmeansClusters_rep_reduced = stepFlexclust(clusterdataf, k = 2:8, nrep = 1000, FUN = cclust, multicore = TRUE)
plot(kmeansClusters_rep_reduced) # from the within sum of squares, we see that it become stable after 2 or 3 clusters 

distances_reduced = daisy(clusterdataf, 
                    type = list(asymm = c(1:ncol(datafexam_reduced))), # threating the variables as asymmetric binaries
                    metric = "gower")

# average silhouette width stats

par(mfrow = c(1,3))
for(i in 1:3){
  print(cluster.stats(distances_reduced, kmeansClusters_rep_reduced[[i]]@cluster))
  plot(silhouette(kmeansClusters_rep_reduced[[i]]@cluster, distances_reduced), cex.names=0.6)
}


mostFollowed_byclust(kmeansClusters_rep_reduced[[1]]@cluster, percentage = 0.5) # looking frequencies inside two clusters



#### MORE THAN TWO GROUPS ####


mostFollowed_byclust(kmeansClusters_rep_reduced[[2]]@cluster, percentage = .5) # 3 clusters
mostFollowed_byclust(kmeansClusters_rep_reduced[[3]]@cluster, percentage = .5) # 4 clusters



