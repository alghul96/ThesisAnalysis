library(flexclust)
library(fpc)
library(cluster)

distances = daisy(clusterdataf, 
                  type = list(asymm = c(1:ncol(clusterdataf))), 
                  metric = "gower")

kmedoidsClusters = pam(distances, k = 4, diss = TRUE)

kmedoidsClusters = stepFlexclust(x = distances, k = 4, nrep = 1000, FUN = pam, multicore = TRUE)



##### EVALUATING THE NUMBER OF CLUSTERS #######

pc = pamk(distances, krange=1:14, criterion="asw", diss = TRUE)
pc
pc$pamobject$cluster
pc$pamobject$medoids

cluster.stats(distances, kmedoidsClusters$cluster)
