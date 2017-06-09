library(flexclust)
library(cluster)
library(fpc)
library(hybridHclust)

#### AS FACTOR DATAFRAME #####

as.factor.dataframe = function(x){
  
  output_dataframe = data.frame(row.names = rownames(x))
  
  for(i in names(x)){
    
        output_dataframe[, i] = factor(x[ ,i])
    
  }
  return(output_dataframe) 
}


#### MOST FOLLOWED #####

mostFollowed = function(k, percentage = 0.7){
  
  course_freq = apply(clusterdataf[k, ], 2, mean)
  is_popular = (course_freq >= percentage) & (course_freq <= 1) # seeing which courses are more frequent
  
  most_followed = course_freq[is_popular] # selection of the courses
  most_followed_ordered = most_followed[order(course_freq[is_popular], decreasing = T)] # ordering
  
  print(length(k))
  print(as.data.frame(most_followed_ordered))
  cat("____________________________________________________________________\n")
  
}

#### MOST FOLLOWED BYCLUST  #####


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


#### STUDYPLAN FINDER #####

studyplan_finder = function(binarydataf, technique, nclust, nsim = 1){
  
  if(class(binarydataf) != "data.frame") warning("Input should be a binary data frame.")
  if(class(technique) != "list") stop("Should provide a list of valid clustering techniques. Please input a list of techniques.")
  if(length(nclust) == 1) if(nclust == 0) stop("At least one cluster is needed to perform analysis.")
  
  results = list()
  
  
  cat("\nComputing distance matrix for binary variables...\n")
  distances = daisy(binarydataf, 
                    type = list(asymm = c(1:ncol(binarydataf))), # threating the variables as asymmetric binaries
                    metric = "gower")
  
  
  # K - means
  if("k-means" %in% technique){
    cat("\nPerforming k-means clustering. It might take a while depending on number of simulations...\n")
    kmeans_result = stepFlexclust(binarydataf, k = nclust, nrep = nsim, FUN = cclust, multicore = TRUE, verbose = FALSE)
    results["kmeans"] = list(kmeans_result)
    
  }
  
  
  # K-medoids
  if("k-medoids" %in% technique){
    cat("\nPerforming k-medoids clustering...\n")
    kmedoids_result = pamk(distances, krange = nclust, criterion = "asw", diss = TRUE)
    results["kmedoids"] = list(kmedoids_result)
    
  }
  
  
  # Ward method
  if("ward" %in% technique){
    cat("\nPerforming Ward hierarchical clustering...\n")
    ward_result = hclust(distances, method = "ward.D")
    results["ward"] = list(ward_result)
  }
  
  
  # Hybrid hierarchial method
  if("hybrid" %in% technique){
    cat("\nPerforming Hybrid hierarchical clustering...\n")
    hybrid_result = hybridHclust(binarydataf)  
    results["hybrid"] = list(hybrid_result)
  }
  
  
  # probability based clustering
  
  if("prob" %in% technique){
    library(PythonInR)
    cat("\nPerforming Bayes Bernoulli Mixture Model clustering. It might take a while depending on number of simulations...\n")
    setwd("Python")
    pyConnect()
    pyExec("import pandas")
    
    cat("\tTransferring variables to Python...\n")
    pySet("data", value = binarydataf, usePandas = TRUE)
    pySet("nclusters", value = max(nclust), usePandas = TRUE)
    pySet("nsimulations", value = nsim, usePandas = TRUE)
    
    cat("\tExecuting the script...")
    pyExecfile("simpleModelbased.py") # running 
    
    mixtgroup = pyGet("modelclust", simplify = FALSE) + 1
    mixtprob = pyGet("clustprob", simplify = TRUE)
    mixtprototypes = pyGet("prototype", simplify = TRUE)
    
    pyExit()
    mixture_result = list(clusters = mixtgroup, clusterprob = mixtprob, prototypes = mixtprototypes)
    results["mixture"] = list(mixture_result)
    
  }
  
  
  
  #cluster.stats(distances, kmeans_result[[i]]@cluster)$avg
  cat("\n")
  return(results)
  
}



convert_dataframe = function(X){
  
  datafexam = data.frame(NULL) # initializing the dataframe
  
  for(i in 1:nrow(X)){
    mat = X[i, 1]
    exam = X[i, 3]
    
    datafexam[mat, exam] = 1
  }
  
  datafexam[is.na(datafexam)] = 0 # filling the NULLs
  
  return(datafexam)
}