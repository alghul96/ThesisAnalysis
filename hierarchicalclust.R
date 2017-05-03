library(cluster)

distances = daisy(datafexam_ordered[,-201], 
                  type = list(asymm = c(1:ncol(datafexam_ordered))), # threating the variables as asymmetric binaries
                  metric = "gower")


ComplClusters = hclust(distances, method = "complete")


#### IDENTIFYING MOST FOLLOWED COURSES IN CLUSTERS ####

# The following code produces an interactive deindogram showing the most followed
# exams for each group of variables considered


# building a function to see most followed exam names given a vector of students

mostFollowed = function(k, percentage = 0.6){
  
  course_freq = apply(datafexam_ordered[k, ], 2, mean)
  is_popular = (course_freq >= percentage) & (course_freq <= 0.99) # seeing which courses are more frequent
  
  most_followed = course_freq[is_popular] # selection of the courses
  most_followed_ordered = most_followed[order(course_freq[is_popular], decreasing = T)] # ordering
  
  print(most_followed_ordered)
  cat("____________________________________________________________________\n")

}


# Plotting an interactive deindogram to see the courses
plot(ComplClusters, labels = other_info$immyear)
identify(ComplClusters, mostFollowed) # click over a branch to see the most followed courses!



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
#circlize_dendrogram(dend, dend_track_height = 0.8) 
circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .4, labels = FALSE)
