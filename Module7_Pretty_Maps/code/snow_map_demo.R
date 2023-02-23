## Script demoing pretty map generation in R.
library(terra)
library(tidyterra)
library(ggsn)
library(ggplot2)
library(gridExtra)

##Loads data.
snow_files <- list.files("./Module7_Pretty_Maps/data/",pattern=".tif$",
                         full.names=TRUE)
snow_rast <- rast(snow_files)
names(snow_rast) <- c("2018","2019","2020","2021")

map1 <- ggplot()+
  geom_spatraster(data=snow_rast)+
  labs(title="Spring Snow Persistence",tag="(a)")+
  scale_fill_whitebox_c("Day of Year",palette="muted",direction=1,limits=c(20,220))+
  scale_x_continuous(expand=c(0,0),breaks=c(-107,-106.4))+
  scale_y_continuous(expand=c(0,0),breaks=c(38.5,38.7,38.9))+
  facet_wrap(~lyr,ncol=4)+
  theme_bw()+
  theme(axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))
print(map1)
