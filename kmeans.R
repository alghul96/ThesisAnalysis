# install.packages("flexclust")
#library(stats)
library(flexclust)
library(cluster)

#### K-MEANS EUCLIDEAN CLUSTERING ####

# Creating the kmeans object
# kmeansClusters = kmeans(clusterdataf, centers = 5)

kmeansClusters = stepFlexclust(clusterdataf, k = 5, nrep = 1000, FUN = cclust, multicore = TRUE)

summary(kmeansClusters)

kcenters = kmeansClusters@centers 
daisy(kcenters, metric = "manhattan") # distances beetween the cluster centers

# Cluster of each instance

# kgroup = kmeansClusters$cluster
kgroup_5 = clusters(kmeansClusters)

# Most seen exams in each group

mostFollowed(rownames(clusterdataf)[kgroup_5 == 1]) # Group 1
# kcenters[1,order(kcenters[1,])] # similar result
mostFollowed(rownames(clusterdataf)[kgroup_5 == 2]) # Group 2
mostFollowed(rownames(clusterdataf)[kgroup_5 == 3]) # Group 3
mostFollowed(rownames(clusterdataf)[kgroup_5 == 4]) # Group 4
mostFollowed(rownames(clusterdataf)[kgroup_5 == 5]) # Group 5


hist(kgroup_5, breaks = 5)



#### DIFFERENT K k-MEANS CLUSTERING ####

### For 2 to 15 clusters 
kmeansClusters_rep = stepFlexclust(clusterdataf, k = 2:15, nrep = 600, FUN = cclust, multicore = TRUE)


# Choosing the number of clusters

plot(kmeansClusters_rep)

clusterdistances = daisy(kmeansClusters_rep[[14]]@centers, metric = "manhattan")
plot(hclust(clusterdistances, method = "ward.D"))


kgroup_4 = kmeansClusters_rep[[3]]@cluster

mostFollowed(rownames(clusterdataf)[kgroup_4 == 1]) # Group 1
mostFollowed(rownames(clusterdataf)[kgroup_4 == 2]) # Group 2
mostFollowed(rownames(clusterdataf)[kgroup_4 == 3]) # Group 3
mostFollowed(rownames(clusterdataf)[kgroup_4 == 4]) # Group 4
