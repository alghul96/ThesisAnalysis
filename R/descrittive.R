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
hist(apply(datafmarks, 1, mean, na.rm = T), breaks = 30-18, main = "Istogramma delle medie degli studenti", col = "lightblue", xlab = "Voto in trentesimi")

### STATS FOR EACH EXAM ####
write.csv(cbind(
  apply(datafmarks, 2, median, na.rm = T),
  apply(datafmarks, 2, mean, na.rm = T),
  apply(datafmarks, 2, sd, na.rm = T)), "riassunto.csv")

summary(datafmarks)
mean(datafmarks$`CALCOLATORI ELETTRONICI M`, na.rm = T)


#### Histograms for major exams
par(mfrow = c(2,2))
hist(datafmarks$`GESTIONE DELL'INNOVAZIONE E DEI PROGETTI M`, breaks = 30-18, main = "Voti di Gestione dell'Innovazione e dei Progetti")
hist(datafmarks$`FONDAMENTI DI INTELLIGENZA ARTIFICIALE M`, breaks = 30-18, main = "Voti di Fondamenti di Intelligenza Artificiale")
hist(datafmarks$`SICUREZZA DELL'INFORMAZIONE M`, breaks = 30-18, main = "Sicurezza dell'Informazione")
hist(datafmarks$`LINGUAGGI E MODELLI COMPUTAZIONALI M`, breaks = 30-18, main = "Linguaggi e Modelli Computazionali")

write.csv(datafmarks, "voti.csv")


#### EXAMS PER SEMESTER ####

frequency_comparison = data.frame(NULL)

for(examA in colnames(datafexam_reduced)){
  for(examB in colnames(datafexam_reduced)){
   frequency_comparison[examA, examB] = sum((datafexam_reduced[, examA] == rep(1,281)) & (datafexam_reduced[, examB] == rep(1,281)))
  }
}

write.csv(frequency_comparison, "Comparison_frequenze.csv")
