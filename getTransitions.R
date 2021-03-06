## Estimate transitions

require(markovchain)
require(kohonen)
require(ggpubr)

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

write.csv(transitions, "transitions.csv", row.names = FALSE)

plot.df = data.frame(transitions)
gghistogram(plot.df, x = "transitions", binwidth = 1000,
            main = "Pollen state transitions over time", xlab = "ageBP") +
  scale_x_reverse() 

mybrks = seq(-500, 11500, by = 1000)
mymids = seq(0, 11000, by = 1000)
nbit = 1000
ntrans = length(transitions)
count.mat = matrix(NA, nrow = length(mymids), ncol = nbit)
for (i in 1:nbit) {
  trans.hist = hist(sample(ages.uniq, ntrans), plot = FALSE, breaks = mybrks)
  count.mat[,i] = trans.hist$counts
}

gghistogram(plot.df, x = "transitions", binwidth = 1000,
            main = "Pollen state transitions over time", xlab = "ageBP") +
  scale_x_reverse() + geom_hline(yintercept = 70 / length(mymids), lty = 2)  + 
  geom_hline(yintercept = 8.16, lty = 4)
