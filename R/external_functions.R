as.factor.dataframe = function(x){
  
  output_dataframe = data.frame(row.names = rownames(x))
  
  for(i in names(x)){
    
        output_dataframe[, i] = factor(x[ ,i])
    
  }
  return(output_dataframe) 
}



mostFollowed = function(k, percentage = 0.7){
  
  course_freq = apply(clusterdataf[k, ], 2, mean)
  is_popular = (course_freq >= percentage) & (course_freq <= 1) # seeing which courses are more frequent
  
  most_followed = course_freq[is_popular] # selection of the courses
  most_followed_ordered = most_followed[order(course_freq[is_popular], decreasing = T)] # ordering
  
  print(length(k))
  print(as.data.frame(most_followed_ordered))
  cat("____________________________________________________________________\n")
  
}



mostFollowed_byclust = function(clustergroup, X = clusterdataf, percentage = 0.7, graph = TRUE, verbose = TRUE){
  
  cluster_number = levels(factor(clustergroup))
  
  if(graph == TRUE) hist(clustergroup, main = "Distribution of clusters", breaks = length(cluster_number), col = "lightblue")
  
  exam_per_group = data.frame(row.names = colnames(X))
  
  for(i in 1:length(cluster_number)){
    
    k = clustergroup == as.numeric(i) # select the cluster instances
  
    course_freq = apply(X[k, ], 2, mean) # compute the frequencies of group k in dataframe
    is_popular = (course_freq >= percentage) & (course_freq <= 1) # selecting more frequent courses
    
    most_followed = course_freq[is_popular] # selection of the courses
    most_followed_ordered = most_followed[order(course_freq[is_popular], decreasing = T)] # ordering
    
    exam_per_group[,i] = course_freq
    
    if (verbose == TRUE){
      print(paste("Group ", i, "of dimention", length(names(X)[k == T])))
      print(as.data.frame(most_followed_ordered))
      cat("____________________________________________________________________\n")
    }

    
  }
  
  return(exam_per_group)
  
}
