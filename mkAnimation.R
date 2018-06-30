## Make animated maps

set.seed(3980)

## NEED TO RUN clustMC_pollen.Rmd FIRST ##

require(kohonen)
require(RColorBrewer)
require(ggmap)
library(maps)
library(animation)

load("largeSOM.RData")
#clustsamp = poll.som.kmean$cluster[poll.som$unit.classif]
gl$Site = sitesn

samplon = rep(NA, length(ages))
samplat = rep(NA, length(ages))

for (i in 1:dim(sites)[1]) {
  cID = which(gl$Site == paste0(sites$site.id[i],"_",sites$dataset.id[i]))
  samplon[cID] = sites$lon[i]
  samplat[cID] = sites$lat[i]
}

map.df = data.frame(site=gl$Site, long=samplon, lat=samplat, 
                    ages=clus.df$ages, cluster=factor(clus.df$cluster))

usamap <- map_data("state")

usamap <- map_data("state")
allages = sort(unique(map.df$ages))
saveGIF({
  for (i in length(allages):1) {
    #   map.df2 = subset(map.df, ages==allages[i])
    #   map.df2$clust = factor(map.df2$clust, levels=c(1,2,3,4,5,6))
    mymaps = ggplot(subset(map.df, ages==allages[i]), aes(long,lat, fill=cluster))
    mymaps = mymaps + annotation_map(usamap, fill="white", col='grey50')
    mymaps = mymaps + geom_point(size=5, pch=21)
    mymaps = mymaps + scale_x_continuous(limits=c(-92,-84))
    mymaps = mymaps + scale_y_continuous(limits=c(45,48))
    mymaps = mymaps + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
    mymaps = mymaps + ggtitle(paste("SOM classes:",
                                    sprintf("%05d", allages[i]),"cal. bp"))
    print(mymaps)
  }
}, movie.name = "map_points_all.gif", interval = 0.1)


