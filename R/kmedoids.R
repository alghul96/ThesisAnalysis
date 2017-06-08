library(flexclust)
library(fpc)
library(cluster)

distances = daisy(clusterdataf, 
                  type = list(asymm = c(1:ncol(clusterdataf))), 
                  metric = "gower")

kmedoidsClusters = pam(distances, k = 3, diss = TRUE)

kmedgroup_3 = kmedoidsClusters$cluster


##### EVALUATING THE NUMBER OF CLUSTERS #######

pc = pamk(distances, krange = 1:14, criterion = "asw", diss = TRUE)
pc

kmedgroup_3 = pc$pamobject$cluster
pc$pamobject$medoids

mostFollowed_byclust(kmedgroup_3)

cluster.stats(distances, kmedoidsClusters$cluster)




#####################################
##### ON REDUCED DATAFRAME ##########
#####################################

library(flexclust)
library(fpc)
library(cluster)


pc_reduce = pamk(distances_reduced, krange = 1:14, criterion = "asw", diss = TRUE)
kmedgroup_reduced = pc_reduce$pamobject$cluster

mostFollowed_byclust(kmedgroup_reduced)
