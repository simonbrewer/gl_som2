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
sites.list = strsplit(as.character(gl$Sample), "_")
for (i in 1:nsample) {
  sitesf[i] <- as.character(sites$site.name[which(sites$site.id==sites.list[[i]][2] & 
                                      sites$dataset.id==sites.list[[i]][3])])
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

save.image("./largeSOM.RData")
## STOP HERE