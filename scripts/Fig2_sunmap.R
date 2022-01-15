# script for Figure 2 in lakeCoSTR MS

library(tidyverse)
library(leaflet)
library(sf)
library(tmap)
library(tmaptools)
library(raster)
library(RColorBrewer)
# library(usmap)

#point to directories
gis_dir = 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'
data_dir = 'data/in-situ locs/'

#read in sampling csv
sampling <- read.csv(file.path(data_dir, 'temp_ms_insitu.csv'))
#make into simple feature; crs is WGS84
sampling_georef <- st_as_sf(sampling, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# save layers
sun <- st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))
sun_bathy <- raster(file.path(gis_dir, 'Sunapee Bathymetry/raster_files/derivitives/sun_z_m'))

#make a map
tmap_mode('plot')
#make sunapee basemap
sunbase = tm_shape(sun_bathy) +
  tm_raster(palette = 'Blues',
            title = 'lake depth\n(meters)') +
  tm_shape(sun) +
  tm_borders() +
  tm_shape(sampling_georef) +
  tm_dots(col = 'frequency', 
          shape = 24,
             size = 0.5,
             title = 'sampling\nfrequency',
          palette = c('black', 'yellow')) +
  tm_compass(position = c('left', 'bottom')) +
  tm_scale_bar(position = c('left', 'bottom')) +
  tm_layout(inner.margins = 0.05)
sunbase
