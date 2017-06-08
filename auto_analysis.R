studyplan_finder = function(binarydataf, technique, nclust, nsim = 1, return_class = TRUE){
  
  library(flexclust)
  library(cluster)
  library(fpc)
  
  if(class(binarydataf) != "data.frame") warning("Input should be a binary data frame.")
  if(class(technique) != "list") stop("Should provide a list of valid clustering techniques. Please input a list of techniques.")
  if(!(technique %in% list("k-means"))) stop("Should provide a list of valid clustering techniques, e.g. list('k-means')")
  if(nclust == 0) stop("At least one cluster is needed to perform analysis.")
  
  
  # K - means
  if("k-means" %in% technique){
    cat("\nPerforming k-means clustering. It might take a while depending on number of simulations...")
    
    
    
  }
  
  
}


studyplan_finder(clusterdataf, technique = list("k-means"), nclust = 1)
