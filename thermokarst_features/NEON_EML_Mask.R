########################################################################################################################
###                                   Spatial Intersection of Sinks and Streams                                      ###
###                                            Code by HGR 5/2020                                                    ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(tidyverse)
########################################################################################################################

### Session Settings ###################################################################################################
rasterOptions(maxmemory = 1e+12)
########################################################################################################################

### Load Data ##########################################################################################################
sinks_sf <- st_read('/scratch/hgr7/hydrologic_flow/neon_sinks_2017_poly.shp') %>% st_transform(26906)
streams_sf <- st_read('/scratch/hgr7/hydrologic_flow/streams_7000000.shp') %>% st_transform(26906)
slope25 <- raster('/scratch/hgr7/int_output/slope_25.tif')
########################################################################################################################

### Intersect sinks and streams ########################################################################################
# join lakes and streams to find lakes with an inlet or outlet
lakes_index <- st_join(sinks_sf, streams_sf)

# subset the sinks to only include lakes with inlet or outlet streams
lakes <- lakes_index %>% filter(!is.na(value))
st_write(lakes, '/scratch/hgr7/hydrologic_flow/lakes_poly_2017.shp')

# convert to raster to join with the non-thermokarst filter
lakes_raster <- rasterize(lakes, slope25)
writeRaster(lakes_raster, '/scratch/hgr7/hydrologic_flow/lakes_2017.tif')
########################################################################################################################