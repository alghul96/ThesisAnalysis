#### Import of laureati.xls ####
library(readxl)
laureati <- read_excel("D:/Box Sync/#UNI/Materiale tesi/Analysis/laureati.xls", sheet = "Base")

#### Creation of the analysis dataset ####

class(laureati[1,1])

datafcod = data.frame(NULL) # initializing the dataframes
datafexam = data.frame(NULL)


for(i in 1:nrow(laureati)){
  
  mat = laureati[i, 1]
  cod = laureati[i, 2]
  exam = laureati[i, 3]
    
  datafcod[mat, cod] = 1
  datafexam[mat, exam] = 1
  
}

par(mfrow = c(1,2))
image((as.matrix(datafcod)), col = "red") # plotting of the data matrices
image((as.matrix(datafexam)), col = "red") 

# Ordering the dataframe by exam name
datafexam_ordered = datafexam[order(colnames(datafexam))]
image(as.matrix(datafexam_ordered), xlab = "Matricole", ylab = "Esami", col = "red")


##### SECOND IMPORT #####

carriere <- read_excel("D:/Box Sync/#UNI/Materiale tesi/Analysis/carriere.xlsx", sheet = "Foglio2")

for(i in 1:nrow(laureati)){
  
  if (!is.na(carriere$`Data laurea`[i])){ # if the student is graduated
    
    mat = carriere[i, 1]
    cod = carriere[i, 2]
    
    datafcod[mat, cod] = 1
    
  }
 
}
# We see that dimentions do not change, thus the students are the same for both dataframes


#### PREPARING DATASET FOR THE ANALYSIS ####

datafexam_ordered[is.na(datafexam_ordered)] = 0
image(as.matrix(datafexam_ordered), xlab = "Matricole", ylab = "Esami", col = c("white", "red"))
