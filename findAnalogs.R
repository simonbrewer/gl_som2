require(kohonen)
require(analogue)
require(dplyr)

load("largeSOM.RData")

nampd = read.csv("data/whitmoreetal2005_pollen.csv")
nampd.poll = nampd[,14:147]
nsample = dim(nampd)[1]
nampd.mat = read.csv("data/whitmore_taxa_mat.csv")
nampd.taxa = nampd.mat[,1]
nampd.mat  = nampd.mat[,-1]

gl.poll = gl[,3:24]

gl.taxa = names(gl.poll)

out.count = matrix(0, ncol=length(gl.taxa), nrow=nsample)

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

## Run analogs
# out.analog <- analog(as.data.frame(out.perc), gl.poll, method="SQchord")
## For SOM nodes
out.analog.som <- analog(as.data.frame(sqrt(out.perc)), 
                         as.data.frame(poll.som$codes), method="chord")

## For clusters
clus.codes = aggregate(poll.som$codes, by=list(som.clus.ord), mean)[,-1]
out.analog.som <- analog(as.data.frame(sqrt(out.perc)), 
                         as.data.frame(clus.codes), method="chord")

stop()
