#install.packages("mclust")
# library(mclust)
library(PythonInR)

# PLEASE TRY NOT TO RUN IN RSTUDIO

setwd("D:/Box Sync/#UNI/Materiale tesi/Analysis/ThesisAnalysis/Python")
pyConnect() # connecting to python session

pyExecfile('mixtureclustering.py')

other_info["mixtgroup"] = pyGet("modelclust", simplify = FALSE) + 1
mixtgroup_3 = as.vector(other_info["mixtgroup"])
mixtprob = pyGet("clustprob", simplify = TRUE)
mixtprototypes = pyGet("prototype", simplify = TRUE)

pyExit()


# see what groups follow most
mostFollowed_byclust(other_info[,3])

table(other_info[,3])

names(other_info)[other_info[3] == 3]

# let's give a look at the predicted class and to the probabilities
cbind(mixtgroup_3, mixtprob) 

hist(mixtprob[,3])

cbind(names(clusterdataf), mixtprototypes)
image(mixtprototypes, col = c(0,2))
