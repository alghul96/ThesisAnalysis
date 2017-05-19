#install.packages("mclust")
library(mclust)
library(PythonInR)
#finitmixtClusters = Mclust(clusterdataf, G = 5)

# PLEASE TRY NOT TO RUN IN RSTUDIO

autodetectPython()
pyConnect()
pyExecfile('Python/mixtureclustering.py')
pyExec("print modelclust")
other_info["mixtgroup"] = pyGet("modelclust", simplify = FALSE) + 1

pyExit()

mixtgroup_3 = as.vector(other_info["mixtgroup"])


# see what groups follow most
mostFollowed(rownames(clusterdataf)[mixtgroup_3 == 1]) # Group 1
mostFollowed(rownames(clusterdataf)[mixtgroup_3 == 2]) # Group 2
mostFollowed(rownames(clusterdataf)[mixtgroup_3 == 3]) # Group 3