###########################
#Catlin Seaview Survey
#Dive Profiles Analysis
#Luke Hedge
##################
rm(list = ls())
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(gstat)
library(fields)
library(rgl)
library(rasterVis)
library(ggplot2)
library(marmap)

source("R/functions.R")

##Read In Data
sound_rms<- readOGR ("Data", "sh_soundings_not_spc_mga") ##read in the Shapefile
sound_spc<-read.csv("Data/Sydney_Harbour_SPC_5m_grid.csv", header=F)
colnames(sound_spc)<-c("x","y","z")
sound<-rbind(sound_spc, data.frame(x=sound_rms$coords.x1,y=sound_rms$coords.x2,z=sound_rms$DEPTH)) #Create new dataframe of both datasets
sound$z<-turn.neg(sound$z) #turn depths into negative numbers 
sound<-SpatialPointsDataFrame(coords=sound[1:2],data=sound[3]) ##turn sound into a SPDF
proj4string(sound)<-proj4string(sound_rms) #set the projection to something sensible

###Read in Catchment data and subset down to just Port Jackson using custom function
Port_Jackson<-readOGR("Data","Sydney_harbour_estuaries_catchments_4") 
Port_Jackson<-spTransform(Port_Jackson,CRS(proj4string(sound)))

Port_Jackson_catchment<-Port_Jackson[Port_Jackson$NAMETYPE=="PORT JACKSON ESTUARY",]
pj.points<-points.poly(Port_Jackson_catchment, points=sound)

##create a raster based on the extent of points in PJ
sounding.raster<-point.rast(soundings,pj.points,1000,1000)

##rasterise point data using mean depth per cell
pj.rast<-raster.and.mask(pj.points,sounding.raster,pj.points$z, mean, Port_Jackson_catchment)

##aggregate cells and create a Thin Spline Regression to interpolate onto new raster
interpolated.bathymetry<-aggregate.interpolate.Tps(pj.rast,10)

###interpolate bathymetry in sydney harbour on a 1000x1000 raster
bathy<-interpolate.plot.with.mask(pj.points, interpolated.bathymetry, Port_Jackson_catchment,1000,1000)


##Plotting bathymetry
pdf(file="/Users/lukehedge/Dropbox/SHRP/Catlin_Seaview_Survey/Output/Sydney_Harbour_Bathymetry.pdf", width=16, height=11)
par(adj=0,cex.main=0.8,  oma=c(0,0,0,0), omi=c(0,0,0,0))
plot(bathy, axes=TRUE, col=colorRampPalette(c("blue", "white", "green"))(255))
title(main="Depths Sydney Harbour - Thin Spline Interpolation")
plot(Port_Jackson_catchment, add=T)
dev.off()



#####
#individual dive profiles
#####

P<-plot.with.limits(bathy, xlim=c(338076.73 , 339821.33), ylim=c(6253660.13,6255658.90), title="Chowder Bay Dive")
rd<- bathy <= -5 & bathy >=4
plot(rd)
  












