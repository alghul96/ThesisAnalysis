library(spatstat)
library(ggplot2)

courses_mixtgroup = mostFollowed_byclust(other_info[,3], percentage = 0.7) # exam frequentation in mixture-model clusters
courses_kmeagroup = mostFollowed_byclust(kgroup_3, percentage = 0.7) # in k-means clusters
courses_hgroup = mostFollowed_byclust(hgroup_3, percentage = 0.7) # in hierarchical clusters


###############################
#### RESULTS COMPARISONS ######
###############################
comparison = cbind(courses_mixtgroup[2:3], courses_kmeagroup[2:1], courses_hgroup[1:2])

round(comparison, 2)

write.csv(comparison, "comparison.csv")

hist(kgroup_3)
hist(as.matrix(mixtgroup_3))

##############################
#### CLUSTER ANALYSIS ########
##############################

plot(
  as.im(as.matrix(courses_mixtgroup), 
        W = owin(c(1,90), c(1,140))),
  main = "Courses Frequencies Beetween Clusters")



# Vediamo come quelli del primo cluster sono quelli di anni precedenti alle immatricolazioni del 2012 
table(other_info[,1], other_info[,3])

most_frequented_byclust = courses_mixtgroup[apply(courses_mixtgroup[,2:3],1,sum) > 0.3, 2:3]

