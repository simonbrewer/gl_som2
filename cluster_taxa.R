library(dplyr)
library(ggpubr)

dat = read.csv("taxa_clusters.csv")

all.means = tapply(dat$Score, dat$Taxa, mean)
clus.means = tapply(dat$Score, list(dat$Taxa, dat$Cluster), mean)

clus.mean.mt = melt(clus.means, varnames = c("Taxa", "Cluster"), value.name = "sqrtp")
clus.mean.mt$all = rep(all.means, 6)
clus.mean.mt$diff = clus.mean.mt$sqrtp - clus.mean.mt$all

ggdotchart(clus.mean.mt, x = "Cluster", y = "diff", facet.by = "Taxa") + geom_hline(yintercept = 0)
