# script for Figure 2 in lakeCoSTR MS

library(tidyverse)
library(leaflet)
library(sf)
library(tmap)
library(tmaptools)
library(usmap)

sampling <- read.csv('data/in-situ locs/temp_ms_insitu.csv')

#make into simple feature; crs is WGS84
sampling_georef <- st_as_sf(sampling, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

#get the crs
st_crs(sampling_georef)

tmap_mode('view')

#make sunapee basemap
sun = tm_basemap('Esri.WorldShadedRelief') +
  tm_shape(sampling_georef) +
  tm_dots(col = 'frequency', 
          size = 0.1,
          palette = c('black', 'grey')) +
  tm_scale_bar(breaks = 3) +
  tm_compass()
sun

us <- plot_usmap('states') 
us

ne <- plot_usmap(include = c('VT', 'NH', 'ME', 'MA'))
ne
