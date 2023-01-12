##Script to demo raster remote access to SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 1-12-2023

## Installs packages
#install.packages(c("terra","sf","ggplot2","ggspatial","tidyterra","gridExtra"))

##Sets up workspace
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(gridExtra)

##Project directory
proj_dir <- "~/code/BIOL697_Remote_Sensing_2023Spring/"

##Sets working directory.
setwd(proj_dir)

##Get data frome with all available SDP data products.
sdp_prods <- read.csv("https://www.rmbl.org/wp-content/uploads/2021/04/SDP_product_table_4_26_2021.csv")
View(sdp_prods)

##Creates raster objects from cloud-based datasets.
dem_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Digital Elevation Model"])
dem_path <- paste("/vsicurl/",dem_uri,sep="")
dem <- rast(dem_path)
dem

flow_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Multi-direction Flow Accumulation"])
flow_path <- paste("/vsicurl/",flow_uri,sep="")
flow <- rast(flow_path)
flow

water_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Surface Water Cover"])
water_path <- paste("/vsicurl/",water_uri,sep="")
water <- rast(water_path)
water

##Combines them into a raster stack.
dem_flow <- c(dem,flow,water)
names(dem_flow) <- c("dem","flow","water")

##Gothic Townsite Extent
plot(dem)
gothic_extent <- draw()

gothic_extent <- ext(matrix(c(326983,328033,
                              4313306,4314244),
                              nrow=2,byrow=TRUE))

##Subsets rasters to the area of interest.
gothic_stack <- crop(dem_flow, gothic_extent)

##computes derived functions of subset maps, adding output to stack
gothic_stack$slope <- terrain(gothic_stack$dem,v="slope")
gothic_stack$aspect <- terrain(gothic_stack$dem,v="aspect")

##Doing arbitrary arithmetic on raster layers
##does raster algebra, creating a new raster map in memory if it's small
gothic_stack$flow_log <- log(gothic_stack$flow)
inMemory(gothic_stack$flow_log)

##plots all layers
plot(gothic_stack)

##samples values at random points where all layers have data
gothic_samples <- spatSample(gothic_stack,size=30, method="random",
                               as.points=TRUE,na.rm=TRUE)

##computes a hillshade layer for plotting
gothic_stack$slope_rad <- terrain(gothic_stack$dem,v="slope", filename=tempfile(),
                                  filetype="COG",unit="radians")
gothic_stack$aspect_rad <- terrain(gothic_stack$dem,v="aspect", filename=tempfile(),
                                   filetype="COG",unit="radians")
gothic_stack$hillshade <- shade(slope=gothic_stack$slope_rad,
                                aspect=gothic_stack$aspect_rad,
                                normalize=TRUE)
plot(gothic_stack$hillshade,col=grey(0:100/100), legend=FALSE)
points(gothic_samples)

##resamples rasters to a coarser resolution
template_100m <- rast(crs=crs(gothic_stack),ext=ext(gothic_stack),resolution=100)
dem_100m <- resample(gothic_stack$dem,y=template_100m,method="bilinear")
slope_100m <- resample(gothic_stack$slope,y=template_100m, method="bilinear")
flow_log_100m <- resample(gothic_stack$flow_log,y=template_100m,method="bilinear")
coarse_stack <- c(dem_100m,slope_100m,flow_log_100m)
names(coarse_stack) <- c("dem_100m","slope_100m","flow_log_100m")

##Plots resampled data.
plot(coarse_stack)

##samples values with the resampled data.
gothic_samples <- extract(coarse_stack,gothic_samples,method="bilinear",bind=TRUE,xy=TRUE)

##Converts to sf object.
gothic_sf <- st_as_sf(gothic_samples)

##Writes extracted data to disk.
st_write(gothic_sf,"gothic_samples.gpkg")

##plots original vs resampled data.
p1 <- ggplot(gothic_samples)+
  geom_point(aes(x=dem,y=dem_100m))+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("Elevation")+
  xlab("1m-Resolution DEM")+
  ylab("100m-Resolution DEM")+
  theme_bw()

p2 <- ggplot(gothic_samples)+
  geom_point(aes(x=slope,y=slope_100m))+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("Slope")+
  xlab("1m-Resolution Slope")+
  ylab("100m-Resolution Slope")+
  theme_bw()

p3 <- ggplot(gothic_samples)+
  geom_point(aes(x=flow_log,y=flow_log_100m))+
  geom_abline(aes(intercept=0,slope=1))+
  ggtitle("Log Flow Accum.")+
  xlab("1m-Resolution Flow")+
  ylab("100m-Resolution Flow")+
  theme_bw()
gridExtra::grid.arrange(p1,p2,p3,ncol=3)

##converts water data to polygons.
flow_poly <- st_as_sf(as.polygons(gothic_stack$flow_log>11)[,1])
water_poly <- st_as_sf(as.polygons(gothic_stack$water)[,1])

##makes a pretty plot with tidyterra
gplot <- ggplot()+
  geom_spatraster(data=gothic_stack$hillshade, maxcell= 9e+05)+
  geom_sf(aes(color="streams"),data=flow_poly,alpha=0.2)+
  geom_sf(aes(shape="sampling point"),data=gothic_sf)+
  scale_fill_gradient(low="black",high="white",guide="none")+
  scale_color_manual("",values=c("#094163"))+
  scale_shape_discrete("")+
  coord_sf(expand=FALSE)+
  annotation_scale(location = "bl", width_hint = 0.2) +
  labs(title="Gothic Townsite",tag="(a)")+
  theme_minimal()
gplot

##Writes plot to disk.
png("stream_sample_map.png",width=5,height=6,units="in",res=300)
gplot
dev.off()

##Writes subset of raster data to disk.
writeRaster(gothic_stack,filename="./Module1_Getting_Started/maps/Gothic_Townsite_Rasters.tif")
