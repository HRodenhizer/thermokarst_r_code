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
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All',
                        full.names = TRUE)
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
neon <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
rm(filenames)
# gliht14_old <- raster('Y:/scratch/hgr7/gliht/AK_20140807_UTM6N.tif')
gliht14 <- raster('Y:/scratch/hgr7/gliht/AK_20140807_Healy_0_DTM.tif')
gliht18 <- raster('Y:/scratch/hgr7/gliht/AK_20180713_Healy_0_DTM.tif')
eml_sites <- st_read('Y:/scratch/hgr7/gps/EML_Sites.shp')

# # adjust gliht old to compare with gliht new
# gliht14_old@extent@xmin <- gliht14_old@extent@xmin - 0
# gliht14_old@extent@xmax <- gliht14_old@extent@xmax - 0
# gliht14_old@extent@ymin <- gliht14_old@extent@ymin + 4
# gliht14_old@extent@ymax <- gliht14_old@extent@ymax + 4
# 
# # compare old and new gliht14
# test_diff <- gliht14 - gliht14_old
# plot(test_diff)
# mapView(test_diff,
#         maxpixels = 9000000,
#         map.types = c('OpenStreetMap.Mapnik', 'Esri.WorldImagery'))
# compare_years <- mapView(gliht14,
#                          maxpixels = 21042021,
#                          na.color = 'transparent',
#                          map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))
# compare_years
# mapView(gliht14_old,
#         map = compare_years,
#         maxpixels = 21042021,
#         na.color = 'transparent',
#         map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))
# 
# test_sub <- gliht18 - gliht14
# plot(test_sub)
########################################################################################################################

### Data Prep/Horizontal Alignment of NEON and GLiHT ###################################################################
# # convert gliht to use utm zone 6n (from utm zone 5n) - script now reads in file in UTM 6N
# gliht_utm_6n <- projectRaster(gliht, crs = crs(neon))

# crop neon data to gliht extent
neon_crop <- crop(neon, gliht14)

# align cells of gliht to cells of neon
gliht14_align <- projectRaster(gliht14, neon_crop)
gliht18_align <- projectRaster(gliht18, neon_crop)
# writeRaster(gliht14_align, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/gliht_2014_neon_align.tif')
# writeRaster(gliht18_align, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/gliht_2018_neon_align.tif')
# gliht14_align_copy <- gliht14_align
# gliht14_align_copy[is.na(gliht14_align_copy)] <- -999999
# writeRaster(gliht14_align_copy, '/scratch/hgr7/gliht/AK_20140807_UTM6N_for_vdatum.tif')

# mask NEON cells outside of gliht footprint
neon_mask <- mask(neon_crop, gliht14_align)

# Use gliht14_align, gliht18_align, and neon_mask for further analysis


########################################################################################################################

### Calculate GLiHT to NEON Difference/Vertical Alignment ##############################################################
# GLiHT NEON Offset 2018 (prior to vertical adjustment)
offset <- gliht18_align - neon_mask[[2]]
plot(offset)
summary(offset)

# Prep LiDAR for Comparison with GPS
gps_elev <- merge(brick('Y:/scratch/hgr7/gps/AElevStack_filled_clipped.tif') %>%
                    projectRaster(to = gliht14_align),
                  brick('Y:/scratch/hgr7/gps/BElevStack_filled_clipped.tif') %>%
                    projectRaster(to = gliht14_align),
                  brick('Y:/scratch/hgr7/gps/CElevStack_filled_clipped.tif') %>%
                    projectRaster(to = gliht14_align))

gliht14_gps_mask <- mask(gliht14_align, gps_elev[[1]])
gliht18_gps_mask <- mask(gliht18_align, gps_elev[[1]])
neon_gps_mask <- mask(neon_mask, gps_elev[[1]])

# Determine offset from GPS for both GLiHT and NEON
gps_gliht_offset <- rbind.data.frame(gliht14_gps_mask %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2014,
                                              method = 'GLiHT',
                                              id = seq(1, nrow(.))),
                                     gliht18_gps_mask %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2018,
                                              method = 'GLiHT',
                                              id = seq(1, nrow(.))),
                                     neon_gps_mask[[1]] %>%
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
                                     gps_elev[[6]] %>%
                                       as.data.frame() %>%
                                       rename(elevation = 1) %>%
                                       filter(!is.na(elevation)) %>%
                                       mutate(year = 2014,
                                              method = 'GPS',
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
gps_gliht_offset_summary_1 <- gps_gliht_offset %>%
  pivot_wider(names_from = 'method', values_from = 'elevation') %>%
  mutate(gliht.offset = GPS - GLiHT,
         neon.offset = GPS - NEON)

gps_gliht_offset_summary <- gps_gliht_offset_summary_1 %>%
  group_by(year) %>%
  summarize(mean.gliht.offset = mean(gliht.offset, na.rm = TRUE),
            sd.gliht.offset = sd(gliht.offset, na.rm = TRUE),
            mean.neon.offset = mean(neon.offset, na.rm = TRUE),
            sd.neon.offset = sd(neon.offset, na.rm = TRUE))

# Offset should be 14.779 m according to vdatum (NEON should be higher than GLiHT)
# Clearly the GLiHT data are not actually in EGM96 or the NEON data and GPS data are
# not in geoid 12B
# Ignore the official offset, and use GPS within CiPEHR as the "truth"

# Correct the vertical alignment of GLiHT and NEON using GPS offset
neon_mask_gps_correction <- neon_mask + gps_gliht_offset_summary$mean.neon.offset[2:4] # gps correction
neon_mask_final <- neon_mask_gps_correction - 0.021 # offset relative to gliht after gps correction
gliht14_align_final <- gliht14_align + gps_gliht_offset_summary$mean.gliht.offset[1] # gps correction
gliht18_align_final <- gliht18_align + gps_gliht_offset_summary$mean.gliht.offset[3] # gps correction

# Create brick of final, aligned LiDAR DTMs
all_gliht <- brick(gliht14_align_final,
                   gliht18_align_final)

all_neon <- neon_mask_final

plot(all_gliht)
plot(all_neon)
# # check geolocation of GLiHT to itself - looks good with new 2014 file!
# compare_years <- mapView(gliht14_align,
#                          maxpixels = 9000000,
#                          at = seq(660, 680, 0.1),
#                          na.color = 'transparent',
#                          map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))
# compare_years
# mapView(gliht18_align,
#         map = compare_years,
#         maxpixels = 9000000,
#         at = seq(660, 680, 0.1),
#         na.color = 'transparent',
#         map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))

# # check geolocation of NEON to itself - looks pretty good!
# compare_years <- mapView(neon_mask[[1]],
#                          maxpixels = 9000000,
#                          at = seq(660, 680, 0.1),
#                          na.color = 'transparent',
#                          map.types = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"))
# compare_years
# compare_years <- mapView(neon_mask[[2]],
#                          map = compare_years,
#                          maxpixels = 9000000,
#                          at = seq(660, 680, 0.1),
#                          na.color = 'transparent',
#                          map.types = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"))
# compare_years
# mapView(neon_mask[[3]],
#         map = compare_years,
#         maxpixels = 9000000,
#         at = seq(660, 680, 0.1),
#         na.color = 'transparent',
#         map.types = c("OpenStreetMap.Mapnik", "Esri.WorldImagery"))


# # check geolocation of NEON vs. GLiHT - looks pretty good!
# compare_years <- mapView(gliht18_align,
#                          maxpixels = 9000000,
#                          at = seq(660, 680, 0.1),
#                          na.color = 'transparent',
#                          map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))
# compare_years
# mapView(neon_mask[[2]],
#         map = compare_years,
#         maxpixels = 9000000,
#         at = seq(660, 680, 0.1),
#         na.color = 'transparent',
#         map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))


# # test setting the average elevation change on the terminal moraine ridge to 0
# reference <- reclassify(gliht18_align,
#                         rcl = matrix(c(0,678,NA, 678,690,1),
#                                      ncol = 3,
#                                      byrow = TRUE))
# sub_mask <- mask(gliht_sub, reference)
# mean_ridge_offset <- cellStats(sub_mask, mean, na.rm = TRUE)
# gliht18_test_align <- gliht18_align - mean_ridge_offset
# gliht_test_sub <- gliht18_test_align - gliht14_align
# plot(gliht_test_sub)
# summary(gliht_test_sub)
# cellStats(gliht_test_sub, mean, na.rm = TRUE)
# mapView(gliht_test_sub, map.types = "Esri.WorldImagery", maxpixels = 9000000)
# 
# 
# # nasa_gliht_change <- raster('/scratch/hgr7/gliht/AK_20180713_Healy_0_mosaic_dtm.tif')
# # plot(nasa_gliht_change)
########################################################################################################################

### Calculate NEON Subsidence ##########################################################################################
# # calculate subsidence layers separately and then join into a brick
# sub18 <- neon[[2]] - neon[[1]]
# plot(sub18)
# sub18[which(is.nan(sub18@data@values))] <- NA
# sub19 <- neon[[3]] - neon[[1]]
# plot(sub19)
# sub19[which(is.nan(sub19@data@values))] <- NA
# sub <- brick(sub18, sub19)
# for (i in 1:nlayers(sub)) {
#   sub[[i]][which(is.nan(sub[[i]]@data@values))] <- NA
# }
# rm(sub18, sub19)

# this is before the gps adjustment
neon_sub_raw <- brick(neon_mask[[2]] - neon_mask[[1]],
                      neon_mask[[3]] - neon_mask[[1]])
plot(neon_sub_raw[[1]])
plot(neon_sub_raw[[2]])
summary(neon_sub_raw)

# this is with the gps adjusted neon lidar
neon_sub <- brick(all_neon[[2]] - all_neon[[1]],
                  all_neon[[3]] - all_neon[[1]])
plot(neon_sub[[1]])
plot(neon_sub[[2]])
summary(neon_sub)

# writeRaster(neon_sub[[1]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/neon_sub_2018_gps_corrected.tif')
# writeRaster(neon_sub[[2]], 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/neon_sub_2019_gps_corrected.tif')

# look at values
# summary(sub[[1]])
# cellStats(sub[[1]], stat = mean)
# boxplot(sub[[1]])
# summary(sub[[2]])
# cellStats(sub[[2]], stat = mean)
# boxplot(sub[[2]])
plot(sub[[1]])
plot(sub[[2]])

# I thought there were issues at first, given how much of panguingue creek is red,
# but most red cells drop out by 0.4 or 0.5 m below 0, so it could be real elevation
# change due to erosion/thermoerosion
colors <- c( '#FF0000', '#FFFFFF','#0000FF')
breaks <- c(-4, -0.2, 0.2, 3)
plot(sub[[1]], breaks = breaks, col = colors)
plot(sub[[2]], breaks = breaks, col = colors)

# writeRaster(sub[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2018.tif')
# writeRaster(sub[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2019.tif')
########################################################################################################################

### Calculate GLiHT Subsidence #########################################################################################
gliht_sub <- gliht18_align_final - gliht14_align_final
summary(gliht_sub)
cellStats(gliht_sub, mean, na.rm = TRUE)
boxplot(gliht_sub)
plot(gliht_sub)
# writeRaster(gliht_sub, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/gliht_sub_2018_gps_corrected.tif')
mapView(gliht_sub,
        na.color = 'transparent',
        maxpixels = 9000000,
        map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))

########################################################################################################################

### Calculate Subsidence ###############################################################################################
lidar_sub <- brick(all_neon[[1]] - all_gliht[[1]], # 2017
                   all_neon[[2]] - all_gliht[[1]], # 2018
                   all_neon[[3]] - all_gliht[[1]]) # 2019

plot(lidar_sub[[1]])
plot(lidar_sub[[2]])
plot(lidar_sub[[3]])

summary(lidar_sub)
cellStats(lidar_sub, mean, na.rm = TRUE)

gliht_neon_offset_2018 <- all_gliht[[2]] - all_neon[[2]]
plot(gliht_neon_offset_2018)
# writeRaster(gliht_neon_offset_2018, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/GLiHT Meeting with Bruce/gliht_neon_offset_2018_gps_corrected.tif')
mapView(gliht_neon_offset_2018,
        maxpixels = 21000000,
        na.color = 'transparent',
        map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))
summary(gliht_neon_offset_2018)
cellStats(gliht_neon_offset_2018, mean, na.rm = TRUE)
# mean offset between gliht and neon is 2.1 cm after gps correction
# NEON is higher than gliht
# I think I should subtract 2.1 cm from all neon images in addition to the gps correction
# because this correction applies to the whole image
########################################################################################################################
