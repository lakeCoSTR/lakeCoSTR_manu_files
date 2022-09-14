# script for Figure 2 in lakeCoSTR MS

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(raster)
# library(RColorBrewer)
# library(usmap)

#point to directories
gis_dir = 'C:/Users/steeleb/Dropbox/travel/gis/project/Sunapee/'
data_dir = 'data/in-situ locs/'
nat_gis_dir = 'F:/GIS_data_general/North_America/political_bound/'
fig_dir = 'figures_v1_12/'

tmap_mode('plot')


#read in sampling csv
sampling <- read.csv(file.path(data_dir, 'temp_ms_insitu.csv'))
#make into simple feature; crs is WGS84
sampling_georef <- st_as_sf(sampling, coords = c('lon_dd', 'lat_dd'), crs = 'EPSG:4326')

# save layers
sun <- st_read(file.path(gis_dir, 'hydrography/LS_shore_WGS.shp'))
sun_bathy <- raster(file.path(gis_dir, 'Sunapee Bathymetry/raster_files/derivitives/sun_z_m'))
northamer = st_read(file.path(nat_gis_dir, 'countries/na_land_w_countries_NAD83.shp'))
NA_wstates = st_read(file.path(nat_gis_dir, 'states/na_land_w_states_NAD83.shp'))


#get bounding box of bathy
bbox_sunapee <- st_bbox(sun_bathy) # current bounding box

xrange <- bbox_sunapee$xmax - bbox_sunapee$xmin # range of x values
yrange <- bbox_sunapee$ymax - bbox_sunapee$ymin # range of y values

#create a new one and modify
bbox_sun_new <- st_bbox(sun_bathy) 
bbox_sun_new[1] <- bbox_sun_new[1] - (0.45 * xrange) # xmin - left
bbox_sun_new[3] <- bbox_sun_new[3] + (0.2 * xrange) # xmax - right
bbox_sun_new[2] <- bbox_sun_new[2] - (0.1 * yrange) # ymin - bottom
bbox_sun_new[4] <- bbox_sun_new[4] + (0.025 * yrange) # ymax - top


#make sunapee basemap ----
#play with extent for legends to fit
sunbase = tm_shape(sun_bathy, bbox = bbox_sun_new) +
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
  tm_compass(position = c('right', 'bottom')) +
  tm_scale_bar(position = c('right', 'bottom'),
              size = 0.75) +
  tm_layout(frame.lwd = 3, frame = 'blue',
            legend.position = c('left', 'bottom'))
sunbase


#make North America

# NE focus
#adjust bounding to ~ new england
#get existing bounding box
bbox_existing <- st_bbox(northamer) # current bounding box
xrange <- bbox_existing$xmax - bbox_existing$xmin # range of x values
yrange <- bbox_existing$ymax - bbox_existing$ymin # range of y values

#create a new one and modify
bbox_NE <- st_bbox(northamer) 
bbox_NE[1] <- bbox_NE[1] + (0.29 * xrange) # xmin - left
bbox_NE[3] <- bbox_NE[3] - (0.69 * xrange) # xmax - right
bbox_NE[2] <- bbox_NE[2] + (0.43 * yrange) # ymin - bottom
bbox_NE[4] <- bbox_NE[4] - (0.51 * yrange) # ymax - top

#transform into polygon
bbox_NE <- bbox_NE %>%  
  st_as_sfc() 

# NA with better focus
NE_base = tm_shape(NA_wstates, bbox = bbox_NE) +
  tm_polygons() +
tm_shape(northamer) +
  tm_borders(lwd = 2)+
tm_shape(st_as_sfc(bbox_sunapee)) +
  tm_borders(lwd = 3, col = 'blue') +
  tm_layout(frame.lwd = 3, frame = 'red')
NE_base


#adjust bounding to ~ mexico/west coast
bbox_NA <- st_bbox(northamer) # current bounding box

bbox_NA[1] <- bbox_NA[1] + (0.13 * xrange) # xmin - left
bbox_NA[3] <- bbox_NA[3] - (0.65 * xrange) # xmax - right
bbox_NA[2] <- bbox_NA[2] + (0.15 * yrange) # ymin - bottom
bbox_NA[4] <- bbox_NA[4] - (0.32 * yrange) # ymax - top

bbox_NA <- bbox_NA %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

# NA with better focus
NA_base = tm_shape(NA_wstates, bbox = bbox_NA) +
  tm_polygons() +
tm_shape(northamer) +
  tm_borders(lwd = 2) +
tm_shape(bbox_NE) +
  tm_borders(lwd = 3, col = 'red')
NA_base


#put it all together
jpeg(file = file.path(fig_dir, 'Fig2_DataExtent.jpg'),
                     width = 8,
                     height = 15,
    units = 'cm',
    res = 600)
print(NA_base, vp = grid::viewport(0.32, 0.92, height = 0.2, width= 0.4))
print(NE_base, vp = grid::viewport(0.73, 0.92, height = 0.15, width = 0.3))
print(sunbase, vp = grid::viewport(0.5, 0.43, height = 0.87))
dev.off()
