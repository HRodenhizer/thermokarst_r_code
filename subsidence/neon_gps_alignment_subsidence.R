########################################################################################################################
###                                  Calculate Subsidence from NEON Data                                             ###
###                                         Code by HGR 3/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(ggthemes)
library(tidyverse)
library(mapview)
########################################################################################################################

### Load Data ##########################################################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All',
                        full.names = TRUE)
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
neon <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
rm(filenames)
eml_sites <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/EML_Sites.shp')
########################################################################################################################

### NEON to GPS Difference/Vertical Alignment ##########################################################################
# Prep LiDAR for Comparison with GPS
gps_elev <- merge(brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevStack_filled_clipped.tif') %>%
                    projectRaster(to = neon),
                  brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevStack_filled_clipped.tif') %>%
                    projectRaster(to = neon),
                  brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevStack_filled_clipped.tif') %>%
                    projectRaster(to = neon))

neon_gps_mask <- mask(neon, gps_elev[[1]])

# Determine offset from GPS for both GLiHT and NEON
gps_neon_offset <- rbind.data.frame(neon_gps_mask[[1]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2017,
                                              method = 'NEON',
                                              id = seq(1, nrow(.))),
                                     neon_gps_mask[[2]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2018,
                                              method = 'NEON',
                                              id = seq(1, nrow(.))),
                                     neon_gps_mask[[3]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2019,
                                              method = 'NEON',
                                              id = seq(1, nrow(.))),
                                     gps_elev[[9]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2017,
                                              method = 'GPS',
                                              id = seq(1, nrow(.))),
                                     gps_elev[[10]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2018,
                                              method = 'GPS',
                                              id = seq(1, nrow(.))),
                                     gps_elev[[11]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2019,
                                              method = 'GPS',
                                              id = seq(1, nrow(.))))

# Summarize offset data
gps_neon_offset_summary_1 <- gps_neon_offset %>%
  pivot_wider(names_from = 'method', values_from = 'elevation') %>%
  mutate(neon.offset = GPS - NEON)

gps_neon_offset_summary <- gps_neon_offset_summary_1 %>%
  group_by(year) %>%
  summarize(mean.neon.offset = mean(neon.offset, na.rm = TRUE),
            sd.neon.offset = sd(neon.offset, na.rm = TRUE))

# Correct the vertical alignment of NEON using GPS offset
neon_gps_correction <- neon + gps_neon_offset_summary$mean.neon.offset[1:3] # gps correction

########################################################################################################################

### Calculate NEON Subsidence ##########################################################################################
# this is before the gps adjustment
neon_sub_raw <- brick(neon[[2]] - neon[[1]],
                      neon[[3]] - neon[[1]])
plot(neon_sub_raw[[1]])
plot(neon_sub_raw[[2]])
summary(neon_sub_raw)

# this is with the gps adjusted neon lidar
neon_sub <- brick(neon_gps_correction[[2]] - neon_gps_correction[[1]],
                  neon_gps_correction[[3]] - neon_gps_correction[[1]])
plot(neon_sub[[1]])
plot(neon_sub[[2]])
summary(neon_sub)

# writeRaster(neon_sub[[1]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/neon_sub_2018_gps_corrected.tif')
# writeRaster(neon_sub[[2]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/neon_sub_2019_gps_corrected.tif')

# look at values
cellStats(neon_sub_raw[[1]], stat = mean)
cellStats(neon_sub_raw[[2]], stat = mean)
cellStats(neon_sub[[1]], stat = mean)
cellStats(neon_sub[[2]], stat = mean)

# I thought there were issues at first, given how much of panguingue creek is red,
# but most red cells drop out by 0.4 or 0.5 m below 0, so it could be real elevation
# change due to erosion/thermoerosion
colors <- c( '#FF0000', '#FFFFFF','#0000FF')
breaks <- c(-4, -0.2, 0.2, 3)
plot(neon_sub[[1]], breaks = breaks, col = colors)
plot(neon_sub[[2]], breaks = breaks, col = colors)

# writeRaster(neon_sub[[1]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/lidar_subsidence/neon')
# writeRaster(neon_sub[[2]], /home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/lidar_subsidence/neon/subsidence_2017_2019.tif)
########################################################################################################################
