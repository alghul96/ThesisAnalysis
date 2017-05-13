# Analysis for my bachelor thesis in Statistics
Code for the analysis of my bachelor thesis. The analysis regards student careers from Msc in Informatic Engineering of University of Bologna.
Supervisor: Professor Claudio Sartori from Department of Computer Science and Engineering of University of Bologna.

## Contents
- **Carriere.xlsx**: first dataset
- **Laureati.xls**: second dataset
- **Analysis.Rproj**: Rstudio Project
- **datapreparation_v2.R**: Where data are imported and prepared for the Analysis. Contents:
  - Data import
  - Creation of a sparse binary matrix for Cluster Analysis
- **hierarchicalclust.R**: Script regarding hierarchical clustering methods. Contents:
  - Computation of the distance matrix
  - Creation of cluster via Divisive Methods
  - An Interactive deindogram
- **kmeans.R**: Script regarding k-means clustering methods.
  - Computation for k-means for k clusters
  - Computation for k-means for different cluster dimentions
  - Methods for choosing the number of clusters
- **model-based.R**: Script containing Mixture Model Based clustering methods. 
