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
kmeansClusters_rep = stepFlexclust(clusterdataf, k = 2:15, nrep = 600, FUN = cclust, multicore = TRUE)


# Choosing the number of clusters

plot(kmeansClusters_rep)



kgroup_4 = kmeansClusters_rep[[3]]@cluster
kgroup_3 = kmeansClusters_rep[[2]]@cluster

mostFollowed_byclust(kgroup_3)
