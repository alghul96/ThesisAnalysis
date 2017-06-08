library(cluster)
library(stats)
library(hybridHclust)

clusterdataf = datafexam # select the dataframe to use for clustering


#### HIERARCHICAL AGGLOMERATIVE CLUSTER COMPUTATION ####


distances = daisy(clusterdataf, 
                  type = list(asymm = c(1:ncol(clusterdataf))), # threating the variables as asymmetric binaries
                  metric = "gower")


ComplClusters = hclust(distances, method = "complete")
WardClusters = hclust(distances, method = "ward.D") # threating the variables as numeric


ComplClusters = agnes(distances, diss = TRUE, method = "complete")
WardClusters = agnes(distances, diss = TRUE, method = "ward")



#### IDENTIFYING MOST FOLLOWED COURSES IN CLUSTERS ####

# The following code produces an interactive deindogram showing the most followed
# exams for each group of variables considered (works only with hclust)

#### Plotting an interactive deindogram to see the courses

# Complete method
plot(ComplClusters, labels = other_info$immyear, which.plot = 2)
identify(ComplClusters, mostFollowed) # click over a branch to see the most followed courses!

# Ward method
plot(WardClusters, labels = other_info$immyear, which.plot = 2)
identify(WardClusters, mostFollowed) # click over a branch to see the most followed courses!


hgroup_3 = cutree(WardClusters, k = 3)

# Seeing the groups
mostFollowed_byclust(hgroup_3)



### DATA REPRESENTATION ###

#install.packages("dendextend")
#install.packages("circlize")
library(dendextend)
library(circlize)

# create a dendrogram
dend <- as.dendrogram(WardClusters)

# modify the dendrogram to have some colors in the branches and labels
dend <- dend %>% 
  color_branches(k=4) %>% 
  color_labels

# plot the radial plot
par(mar = rep(0,4))
circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .9, labels = FALSE)



#### NOTES FOR THE PAPER ####

# complete height
ComplClusters$merge[27]
min(as.matrix(distances)["0000364326", "0000366086"])

# the first unit merged to an already formed cluster was unit 212, at stage 27
WardClusters$merge[9, ]
WardClusters$height[9]
WardClusters$merge[27, ]
WardClusters$height[27]

cbind(
  t(clusterdataf[c(129, 165,222),]),
  apply(clusterdataf[c(129,165),], 2, mean),
  apply(clusterdataf[c(129,165),], 2, var))

# via euclidean distance
sqrt(2/3 * sqrt(sum((t(clusterdataf[222,]) -  apply(clusterdataf[c(129,165),], 2, mean))^2)))


########################################
#### HYBRID HIERARCHICAL CLUSTERING ####
########################################

HybridClusters = hybridHclust(clusterdataf)


plot(HybridClusters)
identify(HybridClusters, mostFollowed) # click over a branch to see the most followed courses!

hygroup_3 = cutree(HybridClusters, k = 3)

table(hgroup_3, hygroup_3)





#####################################
##### ON REDUCED DATAFRAME ##########
#####################################

library(cluster)
library(stats)
library(hybridHclust)

clusterdataf = datafexam_reduced

distances_reduced = daisy(as.factor.dataframe(clusterdataf), 
                          type = list(asymm = c(1:ncol(datafexam_reduced))), # threating the variables as asymmetric binaries
                          metric = "gower")


WardClusters_reduced = hclust(distances_reduced, method = "ward.D")
HybridClusters_reduced =  hybridHclust(clusterdataf)


plot(WardClusters_reduced, labels = F)
identify(WardClusters_reduced, FUN = mostFollowed)


plot(HybridClusters_reduced, labels = F)
identify(HybridClusters_reduced, FUN = mostFollowed)
