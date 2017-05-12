# install.packages("flexclust")
#library(stats)
library(flexclust)
library(cluster)

#### K-MEANS EUCLIDEAN CLUSTERING ####

# Creating the kmeans object
# kmeansClusters = kmeans(clusterdataf, centers = 5)
kmeansClusters = cclust(clusterdataf, k = 5)

summary(kmeansClusters)

kcenters = kmeansClusters@centers 
daisy(kcenters, metric = "manhattan") # distances beetween the cluster centers

# Cluster of each instance

# kgroup = kmeansClusters$cluster
kgroup_5 = clusters(kmeansClusters)

# Most seen exams in each group

mostFollowed(rownames(clusterdataf)[kgroup_5 == 1]) # Group 1
mostFollowed(rownames(clusterdataf)[kgroup_5 == 2]) # Group 2
mostFollowed(rownames(clusterdataf)[kgroup_5 == 3]) # Group 3
mostFollowed(rownames(clusterdataf)[kgroup_5 == 4]) # Group 4
mostFollowed(rownames(clusterdataf)[kgroup_5 == 5]) # Group 5

hist(kgroup, breaks = 5)



#### REPEATIVELY K-MEANS CLUSTERING ####

kmeansClusters_rep = stepFlexclust(clusterdataf, k = 5, nrep = 1000, FUN = cclust, multicore = TRUE) # for five

daisy(kmeansClusters_rep@centers, metric = "manhattan") # seeing the distances


### For 3 to 8 clusters 
kmeansClusters_rep = stepFlexclust(clusterdataf, k = 3:8, nrep = 500, FUN = cclust, multicore = TRUE)

kmeansClusters_rep
kmeansClusters_rep[[2]] # works as a list

# Choosing the number of groups

clusterdistances = daisy(kmeansClusters_rep[[6]]@centers, metric = "manhattan")
plot(hclust(clusterdistances, method = "ward.D"))


kgroup_4 = kmeansClusters_rep[[2]]@cluster

mostFollowed(rownames(clusterdataf)[kgroup_4 == 1]) # Group 1
mostFollowed(rownames(clusterdataf)[kgroup_4 == 2]) # Group 2
kmeansClusters_rep[[2]]@centers[2,] # same output
mostFollowed(rownames(clusterdataf)[kgroup_4 == 3]) # Group 3
mostFollowed(rownames(clusterdataf)[kgroup_4 == 4]) # Group 4


