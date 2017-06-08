##############################
# From the previous version ##
# the number of variables   ##
# is reduced of 72          ##
##############################


library(readxl)
# laureati <- read_excel("D:/Box Sync/#UNI/Materiale tesi/Analysis/ThesisAnalysis/laureati.xls", sheet = "recode 2") # import of the recoded dataset
laureati = read_excel("D:/Box Sync/#UNI/Materiale tesi/Analysis/ThesisAnalysis/laureati.xls", sheet = "recode 3") # import of the third recode


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


#### FINAL CHECKS ####

datafexam_ordered[(datafexam_ordered == 1 | datafexam_ordered == 0) == FALSE]



#### EXPORTING DATA FOR PYTHON ####
write.csv(datafexam, "Python/clusterdata2.csv", row.names = FALSE)



#========================================#
####          REDUCED DATASET         ####
#========================================#


# cbind(row.names(datafexam)[as.numeric(row.names(datafexam)) < 400000], !(row.names(datafexam)[as.numeric(row.names(datafexam)) < 400000] %in% row.names(datafexam)[kgroup_3 == 3]))


datafexam_reduced = datafexam[as.numeric(row.names(datafexam)) > 390000, ] # Removing older students

exams_frequencies_reduced = apply(datafexam_reduced, 2, mean)
datafexam_reduced = datafexam_reduced[, exams_frequencies_reduced > 0 & exams_frequencies_reduced < 1] # Removing unecessary exams

dim(datafexam_reduced) # we now have only 86 exams for 281
