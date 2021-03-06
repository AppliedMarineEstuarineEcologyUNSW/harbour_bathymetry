##############################
#Catlin Seaview Survey Functions
#Luke Hedge
#########

turn.neg<-function(x){
  x*-1
}


points.poly<-function(polygon,points){
  if(proj4string(polygon)!=proj4string(points)){
    print("CRS do not match")
  }
  inside.poly <- !is.na(over(points, as(polygon, "SpatialPolygons")))
  point.inside<-points[inside.poly, ]
  return(point.inside)
}

point.rast<-function(name, points, ncol, nrow){
  name<-raster()
  extent(name)<-extent(points)
  ncol(name)<-ncol
  nrow(name)<-nrow
  return(name)
}


raster.and.mask<-function(points, rast, z, fun, poly){
  rast.full<-rasterize(points, rast, z, fun)
  rast.crop<-crop(rast.full, extent(poly))
  rast.mask<-mask(rast.crop, poly)
  return(rast.mask)
}


aggregate.interpolate.Tps<-function(raster, aggregate.factor){
  ra<-aggregate(raster,aggregate.factor)
  xy<-data.frame(xyFromCell(ra, 1:ncell(ra)))
  v<-getValues(ra)
  tps<-Tps(xy,v)
  return(tps)
}


interpolate.plot.with.mask<-function(mask.points, model, mask, ncol,nrow){
  p<-raster(mask.points)
  extent(p)<-extent(mask.points)
  ncol(p)<-ncol
  nrow(p)<-nrow
  p<-interpolate(p,model)
  p<-mask(p, mask)
  return(p)
}


plot.with.limits<-function(plot, xlim, ylim, title){
  par(adj=0,cex.main=0.8,  oma=c(0,0,0,0), omi=c(0,0,0,0))
  plot(plot,axes=TRUE, col=colorRampPalette(c("blue", "white"))(500), xlim=xlim,ylim=ylim)
  title(paste(title))
}


raster.to.xyz<-function(raster){
  xy<-data.frame(xyFromCell(raster, 1:ncell(raster)))
  v<-getValues(raster)
  df<-data.frame(x=xy$x,y=xy$y,z=v)
  return(df)
}






