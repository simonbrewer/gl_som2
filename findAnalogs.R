require(kohonen)
require(analogue)
require(dplyr)

load("largeSOM.RData")

## Read in modern pollen samples and land cover values
nampd = read.csv("data/whitmoreetal2005_pollen.csv")
nampd.veg = read.csv("data/whitmoreetal2005_avhrr.csv")
nampd.veg = nampd.veg[which(!is.na(nampd.veg$NASLCR)),]

## Merge to avoid samples with missing land cover
nampd.all = merge(nampd, nampd.veg, by="ID2")

## Split into pollen and landcover datasets
nampd.poll = nampd.all[,14:147]
nampd.slcr = nampd.all[,167:211]
nsample = dim(nampd.poll)[1]

## Get the Whitmore to N22 conversion table
nampd.mat = read.csv("data/whitmore_taxa_mat.csv")
nampd.taxa = nampd.mat[,1]
nampd.mat  = nampd.mat[,-1]

## Subset the great lakes pollen set
gl.poll = gl[,3:24]
gl.taxa = names(gl.poll)

## Blank matrix for counts
out.count = matrix(0, ncol=length(gl.taxa), nrow=nsample)

## Loop through the Great Lakes taxa and find corresponding Whitmore taxa
for (i in 1:length(gl.taxa)) {
  
  taxa.id = which(!is.na(nampd.mat[,i]))  
  if (length(taxa.id) == 0) {
    out.count[,i] <- 0
  } else {
    count.tmp = nampd.poll[,taxa.id]
    if (length(taxa.id) > 1) {
      out.count[,i] <- apply(count.tmp,1,sum)
    } else {
      out.count[,i] <- count.tmp
    }
  }
}

colnames(out.count) <- gl.taxa

out.sum = apply(out.count,1,sum)
out.count = out.count[which(!is.na(out.sum)),]
out.perc = (out.count / apply(out.count,1,sum) ) * 100
colnames(out.perc) <- gl.taxa

## Test for similar ranges
poll.ref = as.data.frame(sqrt(out.perc))
poll.fos = as.data.frame(poll.som$codes)

hist(poll.ref$Picea, freq=FALSE)
hist(poll.fos$Picea, freq=FALSE, add=TRUE)

###############################################################################
## Run analogs
###############################################################################
## For SOM nodes
nnodes = dim(poll.fos)[1]
out.analog.som <- analog(poll.ref, poll.fos, method="chord")
for (i in 1:nnodes) {
  ana.sort = sort(out.analog.som$analogs[,i])[1]
  ana.id = as.numeric(substr(names(ana.sort),2,6))
}

###############################################################################
## For clusters
nbana = 10
clus.codes = aggregate(poll.som$codes, by=list(som.clus.ord), median)[,-1]
out.analog.clus <- analog(poll.ref, clus.codes, method="chord")
# clus.lcr = data.frame(naslcr = rep(NA, 6),
#                       glec21 = rep(NA, 6))
clus.lcr = matrix(NA, nrow=6, ncol=nbana)
for (i in 1:6) {
  ana.sort = sort(out.analog.clus$analogs[,i])[1:nbana]
  ana.id = as.numeric(substr(names(ana.sort),1,6))
  # clus.lcr$naslcr[i] <- nampd.veg$NASLCR[ana.id]
  # clus.lcr$glec21[i] <- nampd.veg$GLEC21[ana.id]
  clus.lcr[i,] <- nampd.veg$NASLCR[ana.id]
}

stop()
