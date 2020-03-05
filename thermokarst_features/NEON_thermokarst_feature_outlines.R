########################################################################################################################
###                       Determine Thermokarst Feature Outlines from NEON Data                                      ###
###                                         Code by HGR 2/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(ggthemes)
library(tidyverse)
########################################################################################################################

### Load Data ##########################################################################################################
filenames <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All',
                        full.names = TRUE)
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
elev <- list(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
rm(filenames)

# elev17 <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2017.tif")
# elev18 <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2018.tif")
# elev19 <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2019.tif")
########################################################################################################################

### Calculate Moving Window Median Elevation and Terrain Roughness #####################################################
# create circular focal windows with radii of 15 m, 25 m, and 35 m
radii <- c(15, 25, 35)
weights <- list()
for (i in 1:length(radii)) {
  weights[[i]] <- focalWeight(elev[[1]], radii[i], type = 'circle')
  weights[[i]][weights[[i]] > 0] <- 1
}

### calculate median elevation
## using the median elevation should not be too influenced by the relatively small portion of thermokarst within any moving window
median15 <- map(elev, ~ focal(.x, weights[[1]], fun = median))
median25 <- map(elev, ~ focal(.x, weights[[2]], fun = median))
median35 <- map(elev, ~ focal(.x, weights[[3]], fun = median))

### Calculate Roughness Metrics
# roughness18 <- terrain(elev18, opt = 'roughness')
# plot(roughness18)
# tri18 <- terrain(elev18, opt = 'TRI')
# plot(tri18)
# tpi18 <- terrain(elev18, opt = 'TPI')
# plot(tpi18)

# 5 meter focal window
# weights_5m <- focalWeight(elev18, 2, type = 'circle')*13
# 5 m mean roughness
# roughness18_5 <- focal(roughness18, w = weights_5m, fun = mean)
# tri18_5 <- focal(tri, w = weights_5m, fun = mean)
# tpi18_5 <- focal(tpi, w = weights_5m, fun = mean)
########################################################################################################################

### Calculate Microtopography (deviance from median elevation) #########################################################
### positive values are higher than the surrounding area, negative values are lower and could be thermokarst
mtopo15 <- map2(elev, median15, ~ .x - .y)
mtopo25 <- map2(elev, median25, ~ .x - .y)
mtopo35 <- map2(elev, median35, ~ .x - .y)

### plot
map(mtopo15, ~ plot(.x))
map(mtopo25, ~ plot(.x))
map(mtopo35, ~ plot(.x))
########################################################################################################################

### Reclassify Microtopography as Thermokarst ##########################################################################
## create matrices to use for cutoff values
## I'm not sure what cut-off makes sense. Vertical accuracy is ~15-22.5 cm. However, the average error over the moving window should be 0.
reclass_matrix_0cm <- matrix(c(-Inf,0,1, 0,Inf,0), ncol = 3, byrow = TRUE)
reclass_matrix_5cm <- matrix(c(-Inf,-0.05,1, -0.05,Inf,0), ncol = 3, byrow = TRUE)

# reclassify values < 0 as thermokarst
karst15 <- map(mtopo15, ~ reclassify(.x, reclass_matrix_0cm))
karst25 <- map(mtopo25, ~ reclassify(.x, reclass_matrix_0cm))
karst35 <- map(mtopo35, ~ reclassify(.x, reclass_matrix_0cm))

# reclassify values < -5 as thermokarst (15 m radius only)
karst15_5 <- map(mtopo15, ~ reclassify(.x, reclass_matrix_5cm))

### plot the thermokarst features
map(karst15, ~ plot(.x))
map(karst25, ~ plot(.x))
map(karst35, ~ plot(.x))
map(karst15_5, ~ plot(.x))
########################################################################################################################

### Test Various Combinations of Thermokarst Classification for Completion and Accuracy ################################
# matrix to reclassify after 
# This one gets all cells with at least one of the layers being thermokarst
karst_combined_1 <- list()
for (i in 1:length(karst15)) {
  karst_combined_1[[i]] <- overlay(karst15[[i]],
                                   karst25[[i]],
                                   karst35[[i]],
                                   fun = function(x,y,z){ifelse(x > 0 | y > 0 | z > 0, 1, 0)})
}

# include 31 m < -0.05 and all 51 m and 71 m
karst_combined_2 <- list()
for (i in 1:length(karst15_5)) {
  karst_combined_1[[i]] <- overlay(karst15_5[[i]],
                                   karst25[[i]],
                                   karst35[[i]],
                                   fun = function(x,y,z){ifelse(x > 0 | y > 0 | z > 0, 1, 0)})
}

# 31 m and 51 m
karst_combined_3 <- list()
for (i in 1:length(karst15)) {
  karst_combined_1[[i]] <- overlay(karst15[[i]],
                                   karst25[[i]],
                                   fun = function(x,y,){ifelse(x > 0 | y > 0, 1, 0)})
}

# 51 m and 71 m
karst_combined_4 <- list()
for (i in 1:length(karst25)) {
  karst_combined_1[[i]] <- overlay(karst25[[i]],
                                   karst35[[i]],
                                   fun = function(x,y){ifelse(x > 0 | y > 0, 1, 0)})
}
########################################################################################################################

### Fill in Holes in the Various Thermokarst Classification Rasters ####################################################
# this uses a filter where any cell that is not thermokarst, but has at least 6 cells surrounding it which are thermokarst,
# becomes thermokarst. This has to be run multiple times to make sure to catch all the cells in oddly shaped holes.
# I think 3 times should be enough, but need to check.

# Create a function to iteratively fill in all cells with at least 6 thermokarst cells in the 8 immediately surrounding cells
fill <- function(raster, weights, reclass_matrix_neighbor, reclass_matrix_thermokarst, n) {
  for (i in 1:n) {
    if (i == 1) {
      # Determine whether a cell is a hole in a thermokarst feature
      karst_neighbor <- reclassify(focal(raster, weights, fun = sum, na.rm = TRUE), rcl = reclass_matrix_neighbor)
      # Fill in the identified holes
      fill <- reclassify(overlay(raster, karst_neighbor, fun = function(x,y){x + y}), rcl = reclass_matrix_thermokarst)
    } else {
      # Again, determine whether a cell is a hole in a thermokarst feature
      karst_neighbor <- reclassify(focal(fill, weights, fun = sum, na.rm = TRUE), rcl = reclass_matrix_neighbor)
      # Again, fill in the identified holes
      fill <- reclassify(overlay(fill, karst_neighbor, fun = function(x,y){x + y}), rcl = reclass_matrix_thermokarst)
    }
  }
  return(fill)
}

# neighbor cells to include when looking for thermokarst
weights_8_cell <- matrix(c(1,1,1, 1,0,1, 1,1,1), nrow = 3)
# matrix used to reclassify cells of 6 or greater to 1 (if there are at least 6 neighbor cells with thermokarst)
reclass_neighbor <- matrix(c(-Inf,5,0, 5,Inf,1), ncol = 3, byrow = TRUE)

# Fill the various thermokarst models
karst15_fill <- map(karst15, ~ fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst15_5_fill <- map(karst15_5, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst25_fill <- map(karst25, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst35_fill <- map(karst35, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst_combined_1_fill <- map(karst_combined_1, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst_combined_2_fill <- map(karst_combined_2, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst_combined_3_fill <- map(karst_combined_3, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
karst_combined_4_fill <- map(karst_combined_4, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
########################################################################################################################

### Save Data to Server ################################################################################################
# writeRaster(karst15_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_15m.tif')
# writeRaster(karst15_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_15m.tif')
# writeRaster(karst15_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_15m.tif')
# writeRaster(karst15_5_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_15m_5cm.tif')
# writeRaster(karst15_5_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_15m_5cm.tif')
# writeRaster(karst15_5_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_15m_5cm.tif')
# writeRaster(karst25_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_25m.tif')
# writeRaster(karst25_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_25m.tif')
# writeRaster(karst25_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_25m.tif')
# writeRaster(karst35_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_35m.tif')
# writeRaster(karst35_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_35m.tif')
# writeRaster(karst35_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_35m.tif')
# writeRaster(karst_combined_1_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_15_25_35.tif')
# writeRaster(karst_combined_1_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_15_25_35.tif')
# writeRaster(karst_combined_1_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_15_25_35.tif')
# writeRaster(karst_combined_2_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_15_5_25_35.tif')
# writeRaster(karst_combined_2_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_15_5_25_35.tif')
# writeRaster(karst_combined_2_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_15_5_25_35.tif')
# writeRaster(karst_combined_3_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_15_25.tif')
# writeRaster(karst_combined_3_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_15_25.tif')
# writeRaster(karst_combined_3_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_15_25.tif')
# writeRaster(karst_combined_4_fill[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2017_25_35.tif')
# writeRaster(karst_combined_4_fill[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2018_25_35.tif')
# writeRaster(karst_combined_4_fill[[3]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/NEON/thermokarst_2019_25_35.tif')
########################################################################################################################
