library(scatterplot3d)
library(rgl)

head(cov(datafexam))
apply(datafexam,2,sd)
cor(datafexam)

pca_data = princomp(datafexam, cor = F)
summary(pca_data)
sum((pca_data$sdev)^2)

pca_data$loadings[, 1:3]
# pca_data$loadings[, 1:3] > apply(pca_data$loadings[, 1:3], 2, mean)

# scatterplot3d(pca_data$scores[, 1], pca_data$scores[, 2], pca_data$scores[, 3], 
#               color = kgroup,
#               angle = 70)

plot3d(pca_data$scores[, 1], pca_data$scores[, 2], pca_data$scores[, 3], 
       col = kgroup_4)
open3d()
plot3d(pca_data$scores[, 1], pca_data$scores[, 2], pca_data$scores[, 3], 
       col = kgroup_5)
