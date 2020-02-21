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
elev18 <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2018.tif")
crs(elev18)
########################################################################################################################

### Calculate Moving Window Median Elevation and Terrain Roughness #####################################################
## using the median elevation should not be too influenced by the relatively small portion of thermokarst within any moving window
## 31 m diameter circle
weights_31m <- focalWeight(elev18, 15, type = 'circle')*709
## 51 m diameter circle
weights_51m <- focalWeight(elev18, 25, type = 'circle')*1961
## 71 m diameter circle
weights_71m <- focalWeight(elev18, 35, type = 'circle')*3853

### calculate median elevation
## 31 m
# median18_31m <- focal(elev18_utm6_12b, w = weights_31m, fun = median)
# ## 51 m
# median18_51m <- focal(elev18_utm6_12b, w = weights_51m, fun = median)
# ## 71 m
# median18_71m <- focal(elev18_utm6_12b, w = weights_71m, fun = median)

### Calculate Roughness Metrics
# roughness18 <- terrain(elev18, opt = 'roughness')
# plot(roughness18)
# tri18 <- terrain(elev18, opt = 'TRI')
# plot(tri18)
# tpi18 <- terrain(elev18, opt = 'TPI')
# plot(tpi18)

# 5 meter focal window
weights_5m <- focalWeight(elev18, 2, type = 'circle')*13
# 5 m mean roughness
# roughness18_5 <- focal(roughness18, w = weights_5m, fun = mean)
# tri18_5 <- focal(tri, w = weights_5m, fun = mean)
# tpi18_5 <- focal(tpi, w = weights_5m, fun = mean)
########################################################################################################################

### Calculate Microtopography (deviance from median elevation) #########################################################
### positive values are higher than the surrounding area, negative values are lower and could be thermokarst
## 31 m
# microtopo18_31m <- elev18_utm6_12b - median18_31m
# ## 51 m
# microtopo18_51m <- elev18_utm6_12b - median18_51m
# ## 71 m
# microtopo18_71m <- elev18_utm6_12b - median18_71m

# writeRaster(microtopo18_31m, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography/neon_microtopography_18_31.tif')
# writeRaster(microtopo18_51m, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography/neon_microtopography_18_51.tif')
# writeRaster(microtopo18_71m, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography/neon_microtopography_18_71.tif')

# microtopo18_31m <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography/neon_microtopography_18_31.tif")
# microtopo18_51m <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography/neon_microtopography_18_51.tif")
# microtopo18_71m <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography/neon_microtopography_18_71.tif")

### plot
# plot(microtopo18_31m)
# plot(microtopo18_51m)
# plot(microtopo18_71m)
########################################################################################################################

### Reclassify Microtopography as Thermokarst ##########################################################################
## create matrices to use for cutoff values
## I'm not sure what cut-off makes sense. Vertical accuracy is ~15-22.5 cm. However, the average error over the moving window should be 0.
reclass_matrix_0cm <- matrix(c(-Inf,0,1, 0,Inf,0), ncol = 3, byrow = TRUE)
reclass_matrix_5cm <- matrix(c(-Inf,-0.05,1, -0.05,Inf,0), ncol = 3, byrow = TRUE)

## 31 m
# thermokarst18_31m_0cm <- reclassify(microtopo18_31m, rcl = reclass_matrix_0cm)
# thermokarst18_31m_5cm <- reclassify(microtopo18_31m, rcl = reclass_matrix_5cm)

## 51 m
# thermokarst18_51m_0cm <- reclassify(microtopo18_51m, rcl = reclass_matrix_0cm)

## 71 m
# thermokarst18_71m_0cm <- reclassify(microtopo18_71m, rcl = reclass_matrix_0cm)


### plot the thermokarst features
# 31 m
# plot(thermokarst18_31m_0cm)
# plot(thermokarst18_31m_5cm)

## 51 m
# plot(thermokarst18_51m_0cm)

## 71 m
# plot(thermokarst18_71m_0cm)
########################################################################################################################

### Test Various Combinations of Thermokarst Classification for Completion and Accuracy ################################
# This one gets all cells with at least one of the layers being thermokarst
# karst_combined_18_1 <- overlay(thermokarst18_31m_0cm,
#                                thermokarst18_51m_0cm,
#                                thermokarst18_71m_0cm,
#                                fun = function(x,y,z){x+y+z})

# include 31 m < -0.05 and all 51 m and 71 m
# karst_combined_18_2 <- overlay(thermokarst18_31m_5cm,
#                                thermokarst18_51m_0cm,
#                                thermokarst18_71m_0cm,
#                                fun = function(x,y,z){x + y + z})

# 31 m and 51 m
# karst_combined_18_3 <- overlay(thermokarst18_31m_5cm,
#                                thermokarst18_51m_0cm,
#                                fun = function(x,y){x + y})

# 51 m and 71 m
# karst_combined_18_4 <- overlay(thermokarst18_51m_0cm,
#                                thermokarst18_71m_0cm,
#                                fun = function(x,y){x + y})
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
# karst_18_31_fill <- fill(thermokarst18_31m_0cm, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_18_31_5_fill <- fill(thermokarst18_31m_5cm, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_18_51_fill <- fill(thermokarst18_51m_0cm, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_18_71_fill <- fill(thermokarst18_71m_0cm, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_combined_18_1_fill <- fill(karst_combined_18_1, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_combined_18_2_fill <- fill(karst_combined_18_2, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_combined_18_3_fill <- fill(karst_combined_18_3, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
# karst_combined_18_4_fill <- fill(karst_combined_18_4, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3)
########################################################################################################################
