#install.packages("mclust")
library(mclust)
library(PythonInR)
#finitmixtClusters = Mclust(clusterdataf, G = 5)

# PLEASE DO NOT RUN IN RSTUDIO

autodetectPython()
pyConnect()
pyExecfile('Python/mixtureclustering.py')

pyExit()
