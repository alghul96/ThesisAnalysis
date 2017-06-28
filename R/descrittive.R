library(readxl)
voti = read_excel("data/laureati.xls", sheet = "descrittiva") # import of the third recode
voti = as.data.frame(voti)
head(laureati)

dim(voti)

# adding marks to everyone

#laureati_voti = laureati
for(i in 1:nrow(laureati)){
  
  for(k in 1:nrow(voti)){
    
    if((laureati$matricola[i] == voti$matricola[k]) &  (laureati$`Materia Esame (cod)`[i] == voti$`Materia Esame (cod)`[k]))
      
      laureati_voti[i, "voto"] = voti[k,"voto"]
      
      
    
  }
  
  print(i/nrow(laureati)*100)
  
}



# creating an object with careers for everyone


datafmarks = data.frame(NULL) # initializing the dataframe

for(i in 1:nrow(laureati_voti)){
  mat = laureati_voti[i, 1]
  exam = laureati_voti[i, 3]
  
  datafmarks[mat, exam] = laureati_voti[i, 4]
}

#datafmarks[is.na(datafexam)] = 0 # filling the NULLs


### MEAN FOR EACH STUDENT ###
apply(datafmarks, 1, mean, na.rm = T)


### MEAN FOR EACH EXAM ####
apply(datafmarks, 2, mean, na.rm = T)


summary(datafmarks)
mean(datafmarks$`CALCOLATORI ELETTRONICI M`, na.rm = T)


#### Histograms for major exams

hist(datafmarks$`CALCOLATORI ELETTRONICI M`, breaks = 30-18)
hist(datafmarks$`FONDAMENTI DI INTELLIGENZA ARTIFICIALE M`, breaks = 30-18)
hist(datafmarks$`SISTEMI DISTRIBUITI M`, breaks = 30-18)

