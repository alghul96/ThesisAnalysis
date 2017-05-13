library(cluster)
library(stats)

#### HIERARCHICAL CLUSTER COMPUTATION ####

clusterdataf = datafexam # select the dataframe to use for clustering

distances = daisy(clusterdataf, 
                  type = list(asymm = c(1:ncol(clusterdataf))), # threating the variables as asymmetric binaries
                  metric = "gower")


ComplClusters = hclust(distances, method = "complete")
WardClusters = hclust(distances, method = "ward.D") # threating the variables as numeric

#### IDENTIFYING MOST FOLLOWED COURSES IN CLUSTERS ####

# The following code produces an interactive deindogram showing the most followed
# exams for each group of variables considered


# building a function to see most followed exam names given a vector of students

mostFollowed = function(k, percentage = 0.7){
  
  course_freq = apply(clusterdataf[k, ], 2, mean)
  is_popular = (course_freq >= percentage) & (course_freq <= 1) # seeing which courses are more frequent
  
  most_followed = course_freq[is_popular] # selection of the courses
  most_followed_ordered = most_followed[order(course_freq[is_popular], decreasing = T)] # ordering
  
  print(length(k))
  print(as.data.frame(most_followed_ordered))
  cat("____________________________________________________________________\n")

}


#### Plotting an interactive deindogram to see the courses

# Complete method
plot(ComplClusters, labels = other_info$immyear)
identify(ComplClusters, mostFollowed) # click over a branch to see the most followed courses!

# Ward method
plot(WardClusters, labels = other_info$immyear)
identify(WardClusters, mostFollowed) # click over a branch to see the most followed courses!


hist(cutree(ComplClusters, h = .83))




### DATA REPRESENTATION ###

#install.packages("dendextend")
#install.packages("circlize")
library(dendextend)
library(circlize)

# create a dendrogram
dend <- as.dendrogram(ComplClusters)

# modify the dendrogram to have some colors in the branches and labels
dend <- dend %>% 
  color_branches(k=4) %>% 
  color_labels

# plot the radial plot
par(mar = rep(0,4))
circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .9, labels = FALSE)