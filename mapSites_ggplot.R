require(rgdal)
require(ggplot2)
require(plyr)
require(raster)
require(ggmap)

clip_region <- function(x) {
  bbox <- extent(x = c(-92.5, -82.5), y = c(44.5, 48.5))
  crop(x, bbox)
}

load("largeSOM.RData")
sites.df = read.csv("./sitesinabox3_use.csv")
samplon = rep(NA, length(ages))
samplat = rep(NA, length(ages))

for (i in 1:dim(sites)[1]) {
  cID = which(sitesn == paste0(sites$site.id[i],"_",sites$dataset.id[i]))
  samplon[cID] = sites$lon[i]
  samplat[cID] = sites$lat[i]
}

boxplot(ages ~ cluster, clus.df, horizontal=TRUE, ylim=c(12000,0))

map.df = data.frame(site=sitesn, long=samplon, lat=samplat, 
                    ages=clus.df$ages, cluster=factor(clus.df$cluster))

gl.shp <- readOGR("./data/Great_Lakes", "Great_Lakes")

gl.crop <- clip_region(gl.shp)
gl.crop <- fortify(gl.crop, region="OBJECTID")

png("map_points.png", width=1200, height=800)
mymap = ggplot(map.df, aes(long,lat, fill=cluster)) 
mymap = mymap + annotation_map(gl.crop, col="black", fill="white")
mymap = mymap + geom_point(size=3, pch=21)
mymap = mymap + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
mymap = mymap + facet_wrap(~ages)
print(mymap)
dev.off()

pdf("map_points_all.pdf")
allages = sort(unique(map.df$ages))
for (i in length(allages):1) {
  #   map.df2 = subset(map.df, ages==allages[i])
  #   map.df2$clust = factor(map.df2$clust, levels=c(1,2,3,4,5,6))
  mymap = ggplot(subset(map.df, ages==allages[i]), aes(long,lat, fill=cluster))
  mymap = mymap + annotation_map(gl.crop, col="black", fill="white")
  mymap = mymap + geom_point(size=5, pch=21)
  mymap = mymap + scale_x_continuous(limits=c(-92,-84))
  mymap = mymap + scale_y_continuous(limits=c(45,48))
  mymap = mymap + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
  mymap = mymap + ggtitle(paste("SOM classes:",
                                  sprintf("%05d", allages[i]),"cal. bp"))
  print(mymap)
}
dev.off()

require(animation)
saveGIF({
  for (i in length(allages):1) {
    #   map.df2 = subset(map.df, ages==allages[i])
    #   map.df2$clust = factor(map.df2$clust, levels=c(1,2,3,4,5,6))
    mymap = ggplot(subset(map.df, ages==allages[i]), aes(long,lat, fill=cluster))
    mymap = mymap + annotation_map(gl.crop, col="black", fill="white")
    mymap = mymap + geom_point(size=5, pch=21)
    mymap = mymap + scale_x_continuous(limits=c(-92,-84))
    mymap = mymap + scale_y_continuous(limits=c(45,48))
    mymap = mymap + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
    mymap = mymap + ggtitle(paste("SOM classes:",
                                    sprintf("%05d", allages[i]),"cal. bp"))
    print(mymap)
  }
}, movie.name = "map_points_all.gif", interval = 0.1)

