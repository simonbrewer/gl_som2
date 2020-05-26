## Estimate transitions

require(markovchain)
require(kohonen)

load("largeSOM.RData")
clus.df = data.frame(site = as.factor(sitesn), ages, cluster = rep(NA, length(ages)))

for (i in 1:nnodes) {
  nodeID = which(poll.som$unit.classif == i)
  if (length(nodeID) > 0) {
    clus.df$cluster[nodeID] = som.clus.ord[i]
    
  }
}

ages.uniq = unique(sort(ages))
nages = length(ages.uniq)

sitesn.uniq = levels(clus.df$site)
nsites = length(sitesn.uniq)

transitions = NULL

for (i in 1:nsites) {
  clus.sub = subset(clus.df, site == sitesn.uniq[i])
  clus.sub$diff = c(diff(clus.sub$cluster), NA)
  transitions = c(transitions, clus.sub$ages[which(clus.sub$diff >0)])
}
