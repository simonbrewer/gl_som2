set.seed(3680)

require(spdep)
require(kohonen)
require(RColorBrewer)
require(ggplot2)
require(reshape2)

## Sites 
sites = read.csv("./sitesinabox3_use.csv")

## Pre-processing
gl <- read.csv("./composite/GreatLakesAll_500.csv")
nsample = dim(gl)[1]
sitesf <- rep(NA, nsample)
sitesn <- rep(NA, nsample)
sites.list = strsplit(as.character(gl$Sample), "_")
for (i in 1:nsample) {
  sitesf[i] <- as.character(sites$site.name[which(sites$site.id==sites.list[[i]][2] & 
                                                    sites$dataset.id==sites.list[[i]][3])])
  sitesn[i] <- as.character(paste0(sites.list[[i]][2],"_",sites.list[[i]][3]))
}

ages <- gl$YrBP

## ------------------------------------------------------------------------
poll <- gl[,3:24]
pollSum <- apply(poll,1,sum)
poll <- poll/pollSum*100
poll.s<- sqrt(poll)

## Quick boxplot to check values
plot.df = melt(poll.s, variable.name = "Taxa", value.name = "Abundance")
x = ggplot(plot.df, aes(x=Taxa, y=Abundance)) + geom_boxplot() 
x = x + ggtitle("All abundance values") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(x)

## Self-organizing map
gridX<-31
gridY<-30
grid.som <-somgrid(gridX, gridY, "hexagonal")

## ----cache=FALSE---------------------------------------------------------
poll.som <- som(as.matrix(poll.s), grid = grid.som, rlen=1500)

## Clustering goes here
## ------------------------------------------------------------------------
som.nb = graph2nb(gabrielneigh(grid.som$pts),sym=TRUE)
#plot(som.nb, grid.som$pts)

## ------------------------------------------------------------------------
lcosts <- nbcosts(som.nb, poll.som$codes[[1]])
nb.w <- nb2listw(som.nb, lcosts, style="B")

## ------------------------------------------------------------------------
nbclus = 6
som.mst <- mstree(nb.w,nbclus-1)
plot(som.mst, grid.som$pts, col=2,       
     cex.lab=.7, cex.circles=0.035, fg="blue")

## ------------------------------------------------------------------------
som.skat <- skater(som.mst[,1:2], poll.som$codes[[1]], ncuts=(nbclus-1))
som.clus = som.skat$groups

## Assign ages to nodes
nnodes = gridX*gridY
nodeAge = rep(NA, nnodes)
grid.dist = unit.distances(poll.som$grid)
for (i in 1:nnodes) {
  nodeID = which(poll.som$unit.classif == i)
  if (length(nodeID) >0) {
    nodeAge[i] = mean(ages[nodeID])
  } else {
    nID = which(grid.dist[i,] < 2)
    nodeID = which(poll.som$unit.classif %in% nID)
    nodeAge[i] = mean(ages[nodeID], na.rm=TRUE)
  }
}
som.clus.ord = reorder(som.clus, nodeAge*-1, FUN=median, na.rm=TRUE)
som.clus.ord = factor(som.clus.ord, labels=1:6)
table(som.clus)

save.image("./largeSOM.RData")
## STOP HERE
