library(kohonen)

load("largeSOM.RData")

## ------------------------------------------------------------------------
## Visualisation
plot(poll.som, type="changes", main="Training: SOM 31x30")

## ------------------------------------------------------------------------
myRCBage = function(n, pal="YlGnBu") {
  brewer.pal(n, pal)
}
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

plot(poll.som, type="property", property=nodeAge/1000, main="Mean node age in ka BP",
     palette.name=myRCBage, ncolors=9)
#add.cluster.boundaries(poll.som, poll.som.kmean$cluster)

## ------------------------------------------------------------------------
plot(som.skat, grid.som$pts, cex.circles=0.035, cex.lab=.7)

## ------------------------------------------------------------------------
myRCBcls = function(n, pal="Dark2") {
  brewer.pal(n, pal)
}
plot(poll.som, type="property", property=as.numeric(som.clus), main="Cluster map", 
     palette.name=myRCBcls, ncolors=nbclus, heatkeywidth = .5)
add.cluster.boundaries(poll.som, som.clus)

## ------------------------------------------------------------------------
plot.df = cbind(melt(poll.som$codes[[1]], varnames = c("Grid","Taxa"),
                     value.name = "Score"), Clus=as.factor(rep(som.clus, 22)))

x = ggplot(plot.df, aes(x=Taxa, y=Score, fill=Clus)) + geom_boxplot() + facet_wrap(~Clus)
x = x + ggtitle("All abundance values") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(x)

## ------------------------------------------------------------------------
plot.df = data.frame(age=nodeAge, cluster=as.factor(som.clus))
plot.df$cluster.ord = reorder(plot.df$cluster, plot.df$age*-1, FUN=median, na.rm=TRUE)

x = ggplot(plot.df, aes(x=reorder(cluster, age*-1, FUN=median, na.rm=TRUE), y=age, fill=cluster)) + 
  geom_boxplot() + scale_fill_brewer(palette="Dark2")
x = x + theme_bw() + scale_x_discrete("Cluster") + scale_y_continuous("Age BP")
x = x + ggtitle("Cluster occurrence by age")
print(x)

x = ggplot(plot.df, aes(x=reorder(cluster, age*-1, FUN=median, na.rm=TRUE), y=age, fill=cluster)) + 
  geom_violin() + scale_fill_brewer(palette="Dark2")
x = x + theme_bw() + scale_x_discrete("Cluster") + scale_y_continuous("Age BP")
x = x + ggtitle("Cluster occurrence by age")
print(x)

x = ggplot(plot.df, aes(age, fill=cluster.ord)) + 
  geom_density(alpha=0.5) + scale_fill_brewer(palette="Dark2")
x = x + theme_bw() 
x = x + ggtitle("Cluster occurrence by age")
print(x)

