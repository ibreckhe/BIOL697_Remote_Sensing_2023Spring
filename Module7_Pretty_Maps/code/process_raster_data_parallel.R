##Script to demo parallel processing of raster datasets.

##Sets up workspace
#library(rgdal)
library(raster)
library(sp)
library(TileManager)
library(foreach)
library(doFuture)
library(progressr)
library(microbenchmark)
library(profvis)

##Project directory
proj_dir <- "~/code/BIOL697_Remote_Sensing_2023Spring/Module1_Getting_Started/"

##Sets working directory.
setwd(proj_dir)

##Sets raster options
rasterOptions(maxmemory=8e+9,memfrac=0.8)

##creates a raster object from a cloud-based source
##Get data frome with all available SDP data products.
sdp_prods <- read.csv("https://www.rmbl.org/wp-content/uploads/2020/06/SDP_product_table_6_8_2020.csv")
#View(sdp_prods)

##Creates raster objects from cloud-based datasets.
dem_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Digital Elevation Model"])
dem_path <- paste("/vsicurl/",dem_uri,sep="")
dem <- raster(dem_path, progress='text')
dem

flow_uri <- as.character(sdp_prods$Data.URL[sdp_prods$Product=="Multi-direction Flow Accumulation"])
flow <- raster(paste("/vsicurl/",flow_uri,sep=""),
               progress='text')

##study area of interest
#plot(dem,maxpixels=50000)
aoi_full <- extent(matrix(c(323876,329251,
                            4311941,4315552),
                               nrow=2,byrow=TRUE))

##downloads a local copy of the data for the area of interest
local_brick <- crop(stack(dem,flow),y=aoi_full,progress='text')
names(local_brick) <- c("dem","flow")
inMemory(local_brick)
plot(local_brick[[1]])

##Creates a tiny subset for testing.
aoi_tiny <- drawExtent()
local_tiny <- crop(local_brick,aoi_tiny)

##We want to develop an algorithm to map waterfalls. This has four steps:
## 1. Threshold the flow accumulation layer to select cells with high flow (streams and rivers).
## 2. Create a map of the topographic slope.
## 3. Multiply the binary water layer with the slope map.
## 4. Threshold the slope map to select high-slope water pixels.

find_waterfalls <- function(input_rasters,flow_thresh=60000,slope_thresh=30){
  slope <- terrain(input_rasters$dem,opt="slope",unit="degrees")
  water <- input_rasters$flow > flow_thresh
  slope_water <- slope * water
  waterfalls <- slope_water > slope_thresh
  return(waterfalls)
}

##Tests function on tiny subset.
waterfall_test <- find_waterfalls(local_tiny)

##Profiles the function to see what the slow steps are.
profvis({find_waterfalls(local_tiny)})

##Re-written to (maybe) be faster.
find_waterfalls_v2 <- function(input_rasters,flow_thresh=60000,slope_thresh=30){
  slope <- terrain(input_rasters$dem,opt="slope",unit="degrees")
  waterfalls <- ((input_rasters$flow > flow_thresh) * slope) > slope_thresh
  return(waterfalls)
}

## Tests whether it really is faster.
microbenchmark(find_waterfalls(local_tiny),
               find_waterfalls_v2(local_tiny),
               times=20)

##computes output without tiling or parallelization.
##this is much faster for small areas.
start_time <- Sys.time()
waterfalls_aoi <- find_waterfalls_v2(local_brick,flow_thresh=40000)
end_time <- Sys.time()
end_time - start_time

##Plots output.
plot(waterfalls_aoi)

##set up the tiling scheme for processing larger areas.
tiles <- tileScheme(local_brick,tiledim=c(500,500),buffer=5)

#plot(dem,maxpixels=5000)
plot(tiles)

##create and register a parallel backend.
registerDoFuture()
plan("multisession",workers=2)  

## These are the indexes for the splits.
xs <- 1:length(tiles@buffs)

##create a progress bar
with_progress({
  p <- progressor(along=xs)
  
  ##creates a parallel foreach loop to process each tile.
  out_tiles <- foreach(i=xs,.packages=c("raster")) %dopar% {
                         # Iterates the progress bar
                         p()
    
                         # Gets the extent of the current tile.
                         tile_extent <- extent(bbox(tiles@buffs[[i]]@Polygons[[1]]@coords))
                         
                         # Crops to the buffered tile's extent.
                         tile <- crop(local_brick,tile_extent)
                         
                         # Finds waterfalls
                         tile_waterfalls <- find_waterfalls(tile)
                         
                         # Crops to the unbuffered extent.
                         out_extent <- extent(bbox(tiles@nbuffs[[i]]@Polygons[[1]]@coords))
                         waterfalls_crop <- crop(tile_waterfalls,out_extent)
                         
                         # Returns output
                         (waterfalls_crop)
                       }
})

end_time <- Sys.time()

total_elapsed <- end_time - start_time
total_elapsed

##Returns to normal evaluation.
plan("sequential")

##mosaics the resulting tiles.
out_tiles$ext <- aoi_full
out_tiles$filename="waterfalls_merged.tif"
out_tiles$overwrite=TRUE
wf_merged <- do.call(merge,out_tiles)

plot(wf_merged)

