##############################
# From the previous version ##
# the number of variables   ##
# is reduced of 71          ##
##############################


library(readxl)
laureati <- read_excel("D:/Box Sync/#UNI/Materiale tesi/Analysis/ThesisAnalysis/laureati.xls", sheet = "recode 2") # import of the recoded dataset

#### CREATION OF THE ANALYSIS DATASET ####

datafexam = data.frame(NULL) # initializing the dataframe


for(i in 1:nrow(laureati)){
  
  mat = laureati[i, 1]
  exam = laureati[i, 3]
  
  datafexam[mat, exam] = 1
  
}

datafexam[is.na(datafexam)] = 0 # filling the NULLs


# Ordering the dataframe by exam name
datafexam_ordered = datafexam[order(colnames(datafexam))]


# creation of another dataset for additional informations
other_info = data.frame(row.names = row.names(datafexam_ordered))
other_info[,"immyear"] = datafexam_ordered[,"immyear"]


#### VISUALIZATION OF THE TWO DATASETS ####

image(as.matrix(datafexam), xlab = "Matricole", ylab = "Esami", col = c("white", "red"))
image(as.matrix(datafexam_ordered), xlab = "Matricole", ylab = "Esami", col = c("white", "red"))
