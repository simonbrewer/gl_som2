require(markovchain)
require(kohonen)

load("largeSOM.RData")
clus.df = data.frame(site = as.factor(sitesf), ages, cluster = rep(NA, length(ages)))

for (i in 1:nnodes) {
  nodeID = which(poll.som$unit.classif == i)
  print(nodeID)
  if (length(nodeID) > 0) {
    clus.df$cluster[nodeID] = som.clus.ord[i]
    
  }
}

ages.uniq = unique(sort(ages))
nages = length(ages.uniq)

sitesf.uniq = levels(clus.df$site)
nsites = dim(sites)[1]

poll.clus.mat = matrix(NA, nrow = nages, ncol = nsites)

for (i in 1:dim(clus.df)[1]) {
  siteID = which(sitesf.uniq == clus.df$site[i])
  ageID = which(ages.uniq == clus.df$ages[i])
  
  poll.clus.mat[ageID, siteID] <- clus.df$cluster[i]
}

# modID = which(ages==0)
# clusts[modID] <- NA
# 
# ## Markov chain based on the nodes
# clusts.mc = markovchainFit(clusts)
# 
# # pdf("somMC.pdf")
# plot(clusts.mc$estimate)
# # dev.off()
# 
# communicatingClasses(clusts.mc$estimate)
# recurrentClasses(clusts.mc$estimate)
# absorbingStates(clusts.mc$estimate)
# transientStates(clusts.mc$estimate)
# steadyStates(clusts.mc$estimate)
# canonicForm(clusts.mc$estimate)
# period(clusts.mc$estimate)
# 
# ## Need Markov Chain based on the original samples
# ## Node based MC has non-standard age delta between nodes
# ## Need matrix with row per age and column per site and fill with cluster attribution