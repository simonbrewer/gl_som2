set.seed(3980)

## NEED TO RUN clustMC_pollen.Rmd FIRST ##

require(kohonen)
require(RColorBrewer)
require(ggmap)
load("largeSOM.RData")
#clustsamp = poll.som.kmean$cluster[poll.som$unit.classif]
clustsamp = som.clus.ord

gl$Site = sitesn

site.crd = data.frame(lon=sites$lon, lat=sites$lat)

## Distance function
calcDist = function(x) {
  nx = dim(x)[1]
  dists = rep(NA,nx)
  myr = rep(NA,nx)
  mytheta = rep(NA,nx)
  for (i in 1:(nx-1)) {
    dists[i] = sqrt((site.crd[i,1] - site.crd[(i+1),1])^2 + (site.crd[i,2] - site.crd[(i+1),2])^2)
    if (dists[i] > 0) {
      x = site.crd[(i+1),1] - site.crd[i,1]
      y = site.crd[(i+1),2] - site.crd[i,2]
      
      myr[i] = sqrt(x^2 + y^2)
      mytheta[i] = atan(y/x) * 180 / pi
    }
  }
  myout=data.frame(dists=dists, r=myr, theta=mytheta)
  return(myout)
}

## Time series
tage = NULL
tdist = NULL
sites.v = unique(gl$Site)

for (i in 1:length(sites.v)) {
  sID = which(gl$Site == sites.v[i])
  
  s.ages = ages[sID]
  s.clus = as.numeric(clustsamp[sID])
  
  ## Transitions
  tID = which(diff(s.clus)!=0)
  tage = c(tage, s.ages[tID])
  
  ## Distances
  site.crd <- poll.som$grid$pts[poll.som$unit.classif,][sID,]
  tdist = c(tdist, calcDist(site.crd)$dists)
  
}

gl$age500 = round(ages/500)/0.002

plot(gl$age500, tdist)

pdf("tdist.pdf")
plot(sort(unique(gl$age500)), tapply(tdist, gl$age500, mean, na.rm=TRUE),
     xlab="AgeBP", ylab="Mean distance", type='b')
dev.off()

## Maps
llcrds = data.frame(lon=sites$lon, lat=sites$lat)
llcrds$ent = sites.v

samplon = rep(NA, length(ages))
samplat = rep(NA, length(ages))

for (i in 1:length(sites)) {
  cID = which(gl$Site == llcrds$ent[i])
  samplon[cID] = llcrds$lon[i]
  samplat[cID] = llcrds$lat[i]
}

boxplot(ages ~ som.clus.ord, horizontal=TRUE, ylim=c(12000,0))

map.df = data.frame(site=gl$Site, long=samplon, lat=samplat, 
                    ages=ages, clust=factor(clustsamp, levels=c(1,2,3,4,5,6)))

png("map_points.png", width=1200, height=800)
mymaps = ggplot(map.df, aes(long,lat, fill=clust))
mymaps = mymaps + geom_point(size=3)
mymaps = mymaps + facet_wrap(~ages)
mymaps = mymaps + scale_fill_brewer(palette="Set1")
mymaps
dev.off()

library(maps)
usamap <- map_data("state")
png("map_points2.png", width=1200, height=800)
#pdf("map_points2.pdf", width=1200, height=800)
#postscript("map_points2.eps", paper="special", width=1200, height=800)
mymaps = ggplot(map.df, aes(long,lat, fill=clust))
mymaps = mymaps + annotation_map(usamap, fill="white", col='grey50')
mymaps = mymaps + geom_point(size=3, pch=21)
mymaps = mymaps + scale_x_continuous(limits=c(-90,-84.5))
mymaps = mymaps + scale_y_continuous(limits=c(45.5,47.5))
mymaps = mymaps + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
mymaps = mymaps + facet_wrap(~ages)
print(mymaps)
# mymaps = ggplot(subset(map.df, ages==allages[10]), aes(long,lat, fill=clust))
# mymaps = mymaps + annotation_map(usamap, fill="white", col='grey50')
# mymaps = mymaps + geom_point(size=3)
# #mymaps = mymaps + geom_point(shape = 1,size = 4,colour = "black")
# mymaps = mymaps + facet_wrap(~ages)
# mymaps = mymaps + scale_fill_brewer(palette="Dark2")
# mymaps
dev.off()

usamap <- map_data("state")
pdf("map_points_all.pdf")
allages = sort(unique(map.df$ages))
for (i in length(allages):1) {
#   map.df2 = subset(map.df, ages==allages[i])
#   map.df2$clust = factor(map.df2$clust, levels=c(1,2,3,4,5,6))
  mymaps = ggplot(subset(map.df, ages==allages[i]), aes(long,lat, fill=clust))
  mymaps = mymaps + annotation_map(usamap, fill="white", col='grey50')
  mymaps = mymaps + geom_point(size=5, pch=21)
  mymaps = mymaps + scale_x_continuous(limits=c(-90,-84.5))
  mymaps = mymaps + scale_y_continuous(limits=c(45.5,47.5))
  mymaps = mymaps + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
  mymaps = mymaps + ggtitle(paste("SOM classes:",
                                  sprintf("%05d", allages[i]),"cal. bp"))
  print(mymaps)
}
dev.off()

require(animation)
usamap <- map_data("state")
allages = sort(unique(map.df$ages))
saveGIF({
  for (i in length(allages):1) {
    #   map.df2 = subset(map.df, ages==allages[i])
    #   map.df2$clust = factor(map.df2$clust, levels=c(1,2,3,4,5,6))
    mymaps = ggplot(subset(map.df, ages==allages[i]), aes(long,lat, fill=clust))
    mymaps = mymaps + annotation_map(usamap, fill="white", col='grey50')
    mymaps = mymaps + geom_point(size=5, pch=21)
    mymaps = mymaps + scale_x_continuous(limits=c(-90,-84.5))
    mymaps = mymaps + scale_y_continuous(limits=c(45.5,47.5))
    mymaps = mymaps + scale_fill_brewer(palette="Dark2", drop=FALSE) + coord_fixed()
    mymaps = mymaps + ggtitle(paste("SOM classes:",
                                    sprintf("%05d", allages[i]),"cal. bp"))
    print(mymaps)
  }
}, movie.name = "map_points_all.gif", interval = 0.1)

## Differences
map.df$diffs = rep(NA, length(ages))
for (i in 1:length(sites)) {
  siteID = which(map.df$site==sites[i])
  map.df$diffs[siteID] = c(diff(as.numeric(map.df$clust[siteID])),0) 
}

map.df$diffs[which(map.df$diffs !=0)] <- 1
map.df$diffs = as.factor(map.df$diffs)
png("map_diffs2.png", width=1200, height=800)
#pdf("map_points2.pdf", width=1200, height=800)
#postscript("map_points2.eps", paper="special", width=1200, height=800)
mymaps = ggplot(map.df, aes(long,lat, color=diffs))
mymaps = mymaps + annotation_map(usamap, fill="white", col='grey50')
mymaps = mymaps + geom_point(size=3)
#mymaps = mymaps + geom_point(shape = 1,size = 4,colour = "black")
mymaps = mymaps + facet_wrap(~ages)
mymaps = mymaps + scale_fill_brewer(palette="Set1")
mymaps
dev.off()

# ## With google underlay
# xmin = min(llcrds$lon)
# xmax = max(llcrds$lon)
# ymin = min(llcrds$lat)
# ymax = max(llcrds$lat)
# 
# xc = (xmax-xmin)/2 + xmin
# yc = (ymax-ymin)/2 + ymin
# 
# #map = get_map(location=c(xmin,ymin,xmax,ymax))
# map = get_map(location=c(xc,yc), zoom=7)
# sitemap = ggmap(map, maptype="road")
# sitemap = sitemap + geom_point(aes(x=lon,y=lat),
#                                size=4, data=map.df)
# sitemap = sitemap + facet_wrap(~ages)
# sitemap = sitemap + scale_fill_brewer(palette="Set1")
# sitemap
# dev.off()
# 
# 

## With shapefile
# xmin = min(llcrds$lon)-0.1
# xmax = max(llcrds$lon)+0.1
# ymin = min(llcrds$lat)-0.1
# ymax = max(llcrds$lat)+0.1
# require(maptools)
# states = readShapeSpatial("states_21basic/states.shp")
# 
# mymap = ggplot(data=map.df, aes(long, lat))
# mymap = mymap + geom_polygon(data=states, aes(long, lat, group=group), colour="black", fill="white")
# mymap = mymap + coord_map(xlim=c(xmin,xmax), ylim=c(ymin,ymax)) 
# mymap = mymap + geom_point(aes(long, lat, colour=clust), size=4)
# mymaps = mymaps + facet_wrap(~ages)
# mymap
