
clusterdataf = datafexam_ordered
clusterdataf = apply(clusterdataf, 2, factor)
clusterdataf = as.data.frame(clusterdataf)

str(clusterdataf)
names(clusterdataf)
clusterdataf = clusterdataf[, -c(49, 83)]

monaClusters = mona(clusterdataf)

plot(monaClusters, col = c(2,0), nmax.lab = 2)


# we see that the Monothetic divisive methods do not lead to good results

other_info[monaClusters[["step"]] < 3, "monGroups"] = 1
other_info[monaClusters[["step"]] >= 3 & monaClusters[["step"]] < 9, "monGroups"] = 2
other_info[monaClusters[["step"]] >= 9, "monGroups"] = 3

monagroup = other_info$monGroups

mostFollowed(rownames(datafexam_ordered)[monaClusters[["step"]] < 6] )
mostFollowed(rownames(datafexam_ordered)[monaClusters[["step"]] >= 6 & monaClusters[["step"]] < 9])
mostFollowed(rownames(datafexam_ordered)[monaClusters[["step"]] >= 9])

