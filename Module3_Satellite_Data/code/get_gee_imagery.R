##You will need to sign up for Google Earth Engine (through your Google Account) here:
#https://signup.earthengine.google.com/

##Installs Earth Engine R package and python environment.
remotes::install_github("r-spatial/rgee")
library(rgee)
ee_install(py_env = "rgee") # It is just necessary once!

##Sets credentials
library(rgee)
ee_Initialize(user = 'ibreckhe@gmail.com', drive = TRUE)

##Makes a connection to a dataset, in this case a global DEM
#ee_Initialize()
srtm <- ee$Image("USGS/SRTMGL1_003")

##Puts it on a map.
viz <- list(
  max = 4000,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)
Map$addLayer(
  eeObject = srtm,
  visParams =  viz,
  name = 'SRTM'
)