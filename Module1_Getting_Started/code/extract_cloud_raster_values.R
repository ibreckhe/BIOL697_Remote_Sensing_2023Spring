##Script to demo querying SDP Data Products stored 
##in Amazon S3.

##Author: Ian Breckheimer
##Updated: 1-10-2023
#install.packages("terra)

##Sets up workspace
library(terra)


##Project directory
proj_dir <- "~/code/BIOL697_Remote_Sensing_2023Spring/"

##Sets working directory.
setwd(proj_dir)

##Path to raster datasets. These are in the cloud, 
##but you could replace these with local file paths for local datasets (i.e. "C:/data.tif")
snow2018_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/released/release2/UER_snow_depth_20180331_3m_v1.tif"
elev_path <- "/vsicurl/https://rmbl-sdp.s3.us-east-2.amazonaws.com/data_products/released/release3/UG_dem_3m_v1.tif"

##Loads raster datasets.
snow2018 <- rast(snow2018_path)
elev <- rast(elev_path)

##Loads GPS points.
plotGPS <- read.csv("./Module1_Getting_Started/data/example_points.csv")

##Converts to a spatvector
plotVect <- vect(plotGPS,geom=c("Longitude","Latitude"),crs="EPSG:4326",keepgeom=TRUE)

##Re-projects coordinates to same system as the raster.
plotGPS_tr <- project(plotVect,y=crs(elev))

##Samples rasters at coordinates, with a given buffer (in m):
plotGPS_tr$SnowDepth2018_20m <- extract(snow2018,plotGPS_tr,buffer=20,
                                        fun=mean)[,2]
plotGPS_tr$Elev_20m <- extract(elev,plotGPS_tr,buffer=20,
                                        fun=mean)[,2]
##Displays data
plot(plotGPS_tr$Elev_20m,plotGPS_tr$SnowDepth2018_20m,
     xlab="Elevation (m)",ylab="2018 Snow Depth (m)")
