library(flexclust)
library(fpc)
library(cluster)

distances = daisy(clusterdataf, 
                  type = list(asymm = c(1:ncol(clusterdataf))), 
                  metric = "gower")

kmedoidsClusters = pam(distances, k = 4, diss = TRUE)

kgroup_4 = kmedoidsClusters$cluster


##### EVALUATING THE NUMBER OF CLUSTERS #######

pc = pamk(distances, krange = 1:14, criterion = "asw", diss = TRUE)
pc

kgroup_3 = pc$pamobject$cluster
pc$pamobject$medoids



cluster.stats(distances, kmedoidsClusters$cluster)
