library(spatstat)
library(fpc)

results = list(hgroup_3, kgroup_3, kmedgroup_3, other_info[,3])

courses_mixtgroup = mostFollowed_byclust(other_info[,3], percentage = 0.7) # exam frequentation in mixture-model clusters
courses_kmeagroup = mostFollowed_byclust(kgroup_3, percentage = 0.7) # in k-means clusters
courses_hgroup = mostFollowed_byclust(hgroup_3, percentage = 0.7) # in hierarchical clusters
courses_kmedgroup = mostFollowed_byclust(kmedgroup_3, percentage = 0.7) # in k-medoids


exam_frequencies = list(courses_hgroup, courses_kmeagroup, courses_kmedgroup, courses_mixtgroup)


###############################
#### RESULTS COMPARISONS ######
###############################

par(mfrow = c(2,2))
for(i in results){
  hist(i, main = "")
}


comparison = cbind(courses_mixtgroup[2:3], courses_kmeagroup[2:1], courses_hgroup[1:2], courses_kmedgroup[1:2])

round(comparison, 2) # comparing the two major clusters


### MEDOIDS/PROTOTYPES COMPARISON

par(mfrow = c(3,1))
image(round(t(kcenters)), axes = FALSE, main = "K-means Centroids")
abline(h = c(.25, .75), col = 0)
text(x = .8, y = seq(0, 1, l = 3), labels = c("Sist. e Reti", "Intell. Art.", "Vecchio Ord."), col = "0")

image(mixtprototypes[, c(3,2,1)], axes = FALSE, main = "Mixture-Model Prototypes")
abline(h = c(.25, .75), col = 0)
text(x = .8, y = seq(0, 1, l = 3), labels = c("Sist. e Reti", "Intell. Art.", "Vecchio Ord."), col = "0")

image(t(clusterdataf[kmedoidsClusters$medoids,]), axes = FALSE, main = "K-medoids Medoids")
abline(h = c(.25, .75), col = 0)
text(x = .8, y = seq(0, 1, l = 3), labels = c("Sist. e Reti", "Intell. Art.", "Vecchio Ord."), col = "0")


###########################################
##### CHOOSING THE NUMBER OF CLUSTERS #####
###########################################

# Average silhouette for different clustering methods

for(i in results){
  print("_________________________")
  print(cluster.stats(distances, i))
  plot(silhouette(i, distances), cex.names=0.6)
  
}


# Jaccard similarity coefficient

temp = clusterboot(clusterdataf, clustermethod = kmeansCBI, krange = 3)
plot(temp)
temp = temp$result$partition
rm(temp)



##############################
#### CLUSTER COMPOSITION #####
##############################

plot(
  as.im(as.matrix(courses_mixtgroup), 
        W = owin(c(1,90), c(1,140))),
  main = "Courses Frequencies Beetween Clusters")
abline(v = c(30.5, 60.5), col = "white", lwd = 2)

       
# Vediamo come quelli del terzo cluster sono quelli di anni precedenti alle immatricolazioni del 2012 
table(other_info[,1], clustergroup[,3])

most_frequented_byclust = courses_mixtgroup[apply(courses_mixtgroup, 1, sum) > 0.3,]


###########################
#### REDUCED DATAFRAME ####
###########################

courses_mixtgroup_red = mostFollowed_byclust(mixtgroup_reduced, X = datafexam_reduced, verbose = F)

plot(courses_mixtgroup_red)

most_frequented_byclust_red = courses_mixtgroup_red[apply(courses_mixtgroup_red, 1, sum) > 0.2,]

