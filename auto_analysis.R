source("R/external_functions.R")


results = studyplan_finder(
  clusterdataf,
  technique = list("k-means", "k-medoids", "ward", "prob"),
  nclust = 2,
  nsim = 500
)

names(results)
