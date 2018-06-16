require(markovchain)

load("largeSOM.RData")
clusts = som.clus.ord
modID = which(ages==0)
clusts[modID] <- NA

## Markov chain based on the nodes
clusts.mc = markovchainFit(clusts)

# pdf("somMC.pdf")
plot(clusts.mc$estimate)
# dev.off()

communicatingClasses(clusts.mc$estimate)
recurrentClasses(clusts.mc$estimate)
absorbingStates(clusts.mc$estimate)
transientStates(clusts.mc$estimate)
steadyStates(clusts.mc$estimate)
canonicForm(clusts.mc$estimate)
period(clusts.mc$estimate)

## Need Markov Chain based on the original samples
## Node based MC has non-standard age delta between nodes
## Need matrix with row per age and column per site and fill with cluster attribution