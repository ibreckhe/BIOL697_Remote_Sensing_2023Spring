## Script demoing pretty map generation in R.
library(sf)
library(terra)
library(tidyterra)
library(ggspatial)
library(ggsn)
library(ggplot2)
library(gridExtra)

##Loads data.
snow_files <- list.files("./Module7_Pretty_Maps/data/",pattern=".tif$",
                         full.names=TRUE)
snow_rast <- rast(snow_files)
names(snow_rast) <- c("2018","2019","2020","2021")

##Vector data.
roads_vect <- st_read("./Module7_Pretty_Maps/data/UG_roads_trails_osm.gpkg")

##Pulls out major roads.
roads_major <- filter(roads_vect,highway %in% c("secondary","trunk"))

##Converts to spatvector.
roads_sv <- vect(roads_major)

##Re-projects to coordinate system of raster.
roads_proj <- project(roads_sv,crs(snow_rast))

#### Basic map with terra::plot()
jet_colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

plot(snow_rast[[1]],range=c(25,220),nc=3,
     col=rev(jet_colors(255)),
     main="Snow Persistence (Day of Year)")
plot(roads_proj,add=TRUE,col="grey20")

#### Prettier maps with tidyterra.

## Simple one-panel map.

map0 <- ggplot()+
  geom_spatraster(data=snow_rast[[1]])+
  geom_spatvector(aes(color="highway"),data=roads_proj)+
  scale_color_manual("",values=c("grey40"))+
  scale_fill_whitebox_c("Day of Year",limits=c(20,220),
                        palette="muted",direction=1)+
  theme_minimal()

## Adding a scale bar and north arrow.
map0 + 
  annotation_scale(location="br")+
  annotation_north_arrow(location="bl")

## Zooming in.
map0 + scale_x_continuous(limits=c(327306.007419024, 342195.337294167),expand=c(0,0))+
       scale_y_continuous(limits=c(4289070.14096146, 4307572.32293866),expand=c(0,0))

## Facets to show multiple layers.
map1 <- ggplot()+
  geom_spatraster(data=snow_rast[[1:3]])+
  facet_wrap(facets=~lyr)+
  theme_minimal()

## Full faceted map with scale bar.

# North Arrow and Scale Bar properties.
arrow_params <- tibble::tibble(
  lyr = "2020",
  location = "br")

scale_params <- tibble::tibble(
  lyr = "2018",
  location= "br",
  width_hint=0.2)

##Full Map
map2 <- ggplot()+
  geom_spatraster(data=snow_rast[[1:3]],maxcell=5e+04)+
  geom_spatvector(aes(color="grey80"),data=roads_sv)+
  labs(title="Spring Snow Persistence",tag="(a)")+
  scale_fill_whitebox_c("Day of Year",palette="muted",direction=1,limits=c(20,220))+
  scale_x_continuous(expand=c(0,0),breaks=c(-107,-106.4))+
  scale_y_continuous(expand=c(0,0),breaks=c(38.5,38.7,38.9))+
  annotation_north_arrow(aes(location=location),
                         style=north_arrow_minimal(),
                         which_north="true",
                         data=arrow_params)+
  annotation_scale(aes(location=location,
                   width_hint=width_hint),
                   style="ticks",
                   data=scale_params)+
  facet_wrap(~lyr,ncol=4)+
  theme_minimal()+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))
#print(map2)

## PDF Export.
pdf("./Module7_Pretty_Maps/figs/snow_3panel_tidyterra.pdf",
    width=8,height=4.5)
map2
dev.off()
