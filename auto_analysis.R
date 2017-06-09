#==========================================================#
#                                                          #
#                     SUMMARY SCRIPT                       #
#                                                          #
#==========================================================#


# ========================================================= #
# Analysis is performed on a binary dataset. The first part #
# aims to convert a university dataset in a valid dataset.  #
#                                                           #
# Second part performs the analysis. Required functions are #
# in the "external_functions.R".                            #
# The required packages are listed below this box.          #
# For the complete analysis in needed also Python 2.x       #
# even though also Python 3.x might work (not tested)       #
#                                                           #
# The complete analysis can be found in the R folder.       #
#                                                           #
# ========================================================= #

source("R/external_functions.R")
# install.packages(c("readxl", "flexclust", "cluster", "fpc", "PythonInR"))



#### DATA PREPARATION ####

library(readxl)
laureati = read_excel("data/laureati.xls", sheet = "recode 3") # import of the third recode
laureati = as.data.frame(laureati)

datafexam = convert_dataframe(laureati) # pay attention to the intial dataframe!
image(as.matrix(datafexam), xlab = "Matricole", ylab = "Esami", col = c("white", "red")) # check of the dataframe


# creation of another dataset for additional informations
other_info = data.frame(row.names = row.names(datafexam))
other_info[,"immyear"] = datafexam_ordered[,"immyear"]





#### CLUSTER ANALISYS ####

setwd("D:/Box Sync/#UNI/Materiale tesi/Analysis/ThesisAnalysis")

### Unkown number of clusters
# Might take long time
results0 = studyplan_finder(
          datafexam,
          technique = list("k-means", "k-medoids", "ward", "prob"),
          nclust = 2:8, # results from 2 to 8 clusters
          nsim = 500)  # results based on 500 simulation per method


results0$kmeans # we choose three clusters for k-means

hist(results0$mixture$clusters) # mixture-model expectation maximization converged to three main clusters

plot(results0$ward, labels = other_info$immyear) # ward also seems to be well defined with 3 clusters
identify(results0$ward, mostFollowed) # click over a branch to see the most followed courses!


### With 3 clusters

setwd("D:/Box Sync/#UNI/Materiale tesi/Analysis/ThesisAnalysis")

# can take up to 16 minutes
results = studyplan_finder(
          datafexam,
          technique = list("k-means", "k-medoids", "ward", "prob"),
          nclust = 3, # results just for 3 clusters
          nsim = 5000)  # results based on 5000 simulation per method


# Saving the clusters into groups

clustergroup =  data.frame(row.names = row.names(datafexam))

clustergroup[,"kmeans"] = results$kmeans@cluster # those are from the k-means
clustergroup[,"mixture"] = results$mixture$clusters # those are the clusters obtained from the mixture model
clustergroup[,"ward"] = cutree(results$ward, k = 3)
clustergroup[,"kmedoids"] = results$kmedoids[[1]]$clustering




##### CLUSTERING PROCEDURE COMPARISON #####


# Cluster frequencies between clustering procedures
par(mfrow = c(2,2))
for(i in names(clustergroup)){
  hist(clustergroup[,i], main = i)
}
par(mfrow = c(1,1))


# Inside cluster exam frequencied for various methods
exam_frequencies = list()
for(i in names(clustergroup)){
  
  cat("\nInside cluster exam frequencies for", i, "clustering methods\n")
  exam_frequencies[[i]] = mostFollowed_byclust(clustergroup = clustergroup[,i], X = datafexam, percentage = 0.6, graph = F)
  
  }


# comparison of the exams frequencies beetween the clustering methods
comparison = cbind(exam_frequencies$ward[1:2], exam_frequencies$kmeans[2:1], exam_frequencies$kmedoids[1:2], exam_frequencies$mixture[1:2])
round(comparison, 2)


# medoids, centroids and prototypes comparison
par(mfrow = c(3,1))
image(round(t(results$kmeans@centers)), axes = FALSE, main = "K-means Centroids")
abline(h = c(.25, .75), col = 0)
text(x = .8, y = seq(0, 1, l = 3), labels = c("Sist. e Reti", "Intell. Art.", "Vecchio Ord."), col = "0")

image(mixtprototypes[, c(3,2,1)], axes = FALSE, main = "Mixture-Model Prototypes")
abline(h = c(.25, .75), col = 0)
text(x = .8, y = seq(0, 1, l = 3), labels = c("Sist. e Reti", "Intell. Art.", "Vecchio Ord."), col = "0")

image(t(clusterdataf[results$kmedoids[[1]]$medoids,]), axes = FALSE, main = "K-medoids Medoids")
abline(h = c(.25, .75), col = 0)
text(x = .8, y = seq(0, 1, l = 3), labels = c("Sist. e Reti", "Intell. Art.", "Vecchio Ord."), col = "0")




#### COMPOSIZIONE DEI CLUSTER ####

library(spatstat)

plot(
  as.im(as.matrix(exam_frequencies$mixture), 
        W = owin(c(1,90), c(1,140))),
  main = "Courses Frequencies Beetween Clusters")
abline(v = c(30.5, 60.5), col = "white", lwd = 2)


# We see that the third cluster is made by people enrolled before 2012
table(other_info[,1], clustergroup[,3])

most_frequented_byclust = exam_frequencies$mixture[apply(exam_frequencies$mixture, 1, sum) > 0.3,]
round(most_frequented_byclust, 2)




# ====================================== #
#           REDUCED DATASET              #
# ====================================== #

# Now that we've seen the main composition of the full dataset, we only focus into the main two groups. 
# We'll first find if there are any particular differences between the two. After that, we force the number 
# of clusters to 4, and we'll look at that composition. 


# reduced dataframe
datafexam_reduced = datafexam[as.numeric(row.names(datafexam)) > 390000, ] # Removing older students
exams_frequencies_reduced = apply(datafexam_reduced, 2, mean)
datafexam_reduced = datafexam_reduced[, exams_frequencies_reduced > 0 & exams_frequencies_reduced < 1] # Removing unecessary exams

# performing the analysis on the reduced dataframe
results1 = studyplan_finder(datafexam_reduced,
                            technique = list("k-means", "k-medoids", "prob"),
                            nclust = 2, # results just for 2 clusters
                            nsim = 1000)  # results based on 1000 simulation per method



# Saving the clusters into groups
clustergroup_reduced =  data.frame(row.names = row.names(datafexam_reduced))

clustergroup_reduced[,"kmeans"] = results1$kmeans@cluster # those are from the k-means
clustergroup_reduced[,"mixture"] = results1$mixture$clusters # those are the clusters obtained from the mixture model
clustergroup_reduced[,"kmedoids"] = results1$kmedoids[[1]]$clustering


par(mfrow = c(1,3))
exam_frequencies_red = list()
for(i in names(clustergroup_reduced)){
  
  cat("\nInside cluster exam frequencies for", i, "clustering methods on reduced dataframe\n")
  exam_frequencies_red[[i]] = mostFollowed_byclust(clustergroup = clustergroup_reduced[,i], X = datafexam_reduced, percentage = 0.6, graph = T)
  
}


# Most followed courses inside clusters for mixture model clustering
exam_frequencies_red$mixture[apply(exam_frequencies_red$mixture, 1, sum) > 0.2,]




#### CLUSTERING FOR MORE THAN TWO CLUSTERS #####

results2 = studyplan_finder(datafexam_reduced, technique = list("k-means", "ward"), nclust = 3:4, nsim = 1000)


# Frequency of exams in clusters
mostFollowed_byclust(results2$kmeans[[1]]@cluster, percentage = .5, graph = F) # 3 clusters
mostFollowed_byclust(results2$kmeans[[2]]@cluster, percentage = .5, graph = F) # 4 clusters


# Ward deindogram 
plot(results2$ward, labels = FALSE)
identify(results2$ward, FUN = mostFollowed)
