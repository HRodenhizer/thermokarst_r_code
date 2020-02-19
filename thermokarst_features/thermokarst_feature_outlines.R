########################################################################################################################
###                                Determine Thermokarst Feature Outlines                                            ###
###                                         Code by HGR 2/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(ggthemes)
library(tidyverse)
########################################################################################################################

### Load Data ##########################################################################################################
elev14 <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/GLiht/AK_20140807/AK_20140807_l1s565_DTM.tif")
crs(elev14)

# this take a long time!
# rgb <- merge(brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_388000_7084000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_388000_7085000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_388000_7086000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_389000_7084000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_389000_7085000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_389000_7086000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_390000_7084000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_390000_7085000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_390000_7086000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_391000_7084000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_391000_7085000_image.tif"),
#              brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_391000_7086000_image.tif"))

test <- brick("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/L3/Camera/Mosaic/V01/2017_HEAL_1_385000_7077000_image.tif")
crs(test)
elev14_utm6 <- projectRaster(elev14, crs = crs(test))
egm96 <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoids/egm96-15.tif")
geoid12b <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/GPS/Geoid Troubleshooting 2018/Geoids/navd88-12b.tif")
crs(geoid12b) <- CRS('+init=EPSG:6318')
neon_elev <- raster("C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_2017.tif")
########################################################################################################################

### Calculate Geoid Correction Raster ##################################################################################
# orthorectify the GLiHT data relative to the NEON RGB data
# using ArcMap, I determined that there is a horizontal offset of about 6 m in the N-S direction and 1 m in the E-W direction
# GLiHT needs to go ~6 m North and ~1 m East
xmin(elev14_utm6) <- xmin(elev14_utm6) - 1
xmax(elev14_utm6) <- xmax(elev14_utm6) - 1
ymin(elev14_utm6) <- ymin(elev14_utm6) + 5
ymax(elev14_utm6) <- ymax(elev14_utm6) + 5

# there shouldn't be an ellipsoid correction (correction between wgs84 and grs80 is VERY small and typically ignored)
# that means there is only a correction for the geoid
# this converts the GLiHT data to use geoid 12b, which is the same as NEON and our GPS points
egm96_crop <- projectRaster(egm96, elev14_utm6)
geoid12b_rotate <- rotate(geoid12b) # this had coordinates from 0-360, but it needs to be -180 - 180, rotate fixes
geoid12b_crop <- projectRaster(geoid12b_rotate, elev14_utm6)
correction <- egm96_crop - geoid12b_crop
plot(correction)

# correct the GLiHT to use geoid 12b
elev14_utm6_12b <- elev14_utm6 + correction

# reproject NEON LiDAR to GLiHT
neon_elev_utm6 <- projectRaster(neon_elev, elev14_utm6_12b)

# find difference in elevation between the two LiDAR datasets
diff <- elev14_utm6_12b - neon_elev_utm6
plot(diff)
breaks <- c(-1.5, -0.5, 0.5, 1.5, 2.5)
colors <- c('red', 'white', 'yellow', 'green')
plot(diff, breaks = breaks, col = colors)

# # the offset after geoid correction and orthorectification
# ### WARNING! This is comparing rasters separated by 3 years, so this will not give actual elevations ###
# offset <- median(diff, na.rm = TRUE)
# 
# # remove the median offset
# elev14_corrected <- elev14_utm6_12b - offset
# 
# # now plot differences again
# diff <- elev14_corrected - neon_elev_utm6
# plot(diff)
########################################################################################################################

### Calculate Moving Window Median Elevation ###########################################################################
## using the median elevation should not be too influenced by the relatively small portion of thermokarst within any moving window
### create a matrix of weights to calculate the median
## 15 m diameter circle
# weights14_15m <- focalWeight(elev14_utm6_12b, 7, type = 'circle')*149 # the focalWeight function returns values that add up to 1, but we want each included cell to have a value of 1
## 31 m diameter circle
weights14_31m <- focalWeight(elev14_utm6_12b, 15, type = 'circle')*717
## 51 m diameter circle
weights14_51m <- focalWeight(elev14_utm6_12b, 25, type = 'circle')*1993
## 71 m diameter circle
weights14_71m <- focalWeight(elev14_utm6_12b, 35, type = 'circle')*3893


### calculate median elevation
## 15 m
# median14_15m <- focal(elev14_utm6_12b, w = weights14_15m, fun = median)
## 31 m
median14_31m <- focal(elev14_utm6_12b, w = weights14_31m, fun = median)
## 51 m
median14_51m <- focal(elev14_utm6_12b, w = weights14_51m, fun = median)
## 71 m
median14_71m <- focal(elev14_utm6_12b, w = weights14_71m, fun = median)
########################################################################################################################

### Calculate Microtopography (deviance from median elevation) #########################################################
### this requires ~2 minutes to run each line on my computer
### positive values are higher than the surrounding area, negative values are lower and could be thermokarst
## 15 m
# microtopo14_15m <- elev14_utm6_12b - median14_15m
## 31 m
microtopo14_31m <- elev14_utm6_12b - median14_31m
## 51 m
microtopo14_51m <- elev14_utm6_12b - median14_51m
## 71 m
microtopo14_71m <- elev14_utm6_12b - median14_71m

# writeRaster(microtopo14_31m, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography_14_31.tif')
# writeRaster(microtopo14_51m, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography_14_51.tif')
# writeRaster(microtopo14_71m, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/microtopography_14_71.tif')

### plot
# plot(microtopo14_15m)
plot(microtopo14_31m)
plot(microtopo14_51m)
plot(microtopo14_71m)
########################################################################################################################

### Reclassify Microtopography as Thermokarst ##########################################################################
## create matrices to use for cutoff values
## I'm not sure what cut-off makes sense. Vertical accuracy is ~15-22.5 cm. However, the average error over the moving window should be 0.
reclass_matrix_0cm <- matrix(c(-Inf,0,1, 0,Inf,0), ncol = 3, byrow = TRUE)
reclass_matrix_5cm <- matrix(c(-Inf,-0.05,1, -0.05,Inf,0), ncol = 3, byrow = TRUE)
reclass_matrix_10cm <- matrix(c(-Inf,-0.1,1, -0.1,Inf,0), ncol = 3, byrow = TRUE)
reclass_matrix_20cm <- matrix(c(-Inf,-0.2,1, -0.2,Inf,0), ncol = 3, byrow = TRUE)

## 15 m
# thermokarst14_15m_0cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_0cm)
# thermokarst14_15m_15cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_15cm)
# thermokarst14_15m_20cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_20cm)

## 31 m
thermokarst14_31m_0cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_0cm)
thermokarst14_31m_5cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_5cm)
thermokarst14_31m_10cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_10cm)
thermokarst14_31m_20cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_20cm)

## 51 m
thermokarst14_51m_0cm <- reclassify(microtopo14_51m, rcl = reclass_matrix_0cm)
thermokarst14_51m_10cm <- reclassify(microtopo14_51m, rcl = reclass_matrix_10cm)
thermokarst14_51m_20cm <- reclassify(microtopo14_51m, rcl = reclass_matrix_20cm)

## 71 m
thermokarst14_71m_0cm <- reclassify(microtopo14_71m, rcl = reclass_matrix_0cm)
thermokarst14_71m_10cm <- reclassify(microtopo14_71m, rcl = reclass_matrix_10cm)
thermokarst14_71m_20cm <- reclassify(microtopo14_71m, rcl = reclass_matrix_20cm)


### plot the thermokarst features with the same focal window and different cut-off values on top of each other
# set plot viewer to have two rows and one column
par(mfrow=c(2, 1))

##15 m
# plot(thermokarst14_15m_0cm)
# plot(thermokarst14_15m_15cm)
# plot(thermokarst14_15m_20cm)

# 31 m
plot(thermokarst14_31m_0cm)
plot(thermokarst14_31m_10cm)
# plot(thermokarst14_31m_20cm)

## 51 m
plot(thermokarst14_51m_0cm)
plot(thermokarst14_51m_10cm)
# plot(thermokarst14_51m_20cm)

## 71 m
plot(thermokarst14_71m_0cm)
plot(thermokarst14_71m_10cm)
# plot(thermokarst14_71m_20cm)

dev.off()
########################################################################################################################

### Test Various Combinations of Thermokarst Classification for Completion and Accuracy ################################
# value of 1 means that it is thermokarst using the first input only, 2 means the cell is thermokarst in both inputs
# thermo15m_31m <- overlay(thermokarst14_15m_0cm, thermokarst14_31m_0cm, fun = function(x, y){x*(x+y)})
# thermo15m_51m <- overlay(thermokarst14_15m_0cm, thermokarst14_51m_0cm, fun = function(x, y){x*(x+y)})
thermo31m_51m <- overlay(thermokarst14_31m_0cm, thermokarst14_51m_0cm, fun = function(x, y){x*(x+y)})
# thermo31m_15m <- overlay(thermokarst14_31m_0cm, thermokarst14_15m_0cm, fun = function(x, y){x*(x+y)})
# thermo51m_15m <- overlay(thermokarst14_51m_0cm, thermokarst14_15m_0cm, fun = function(x, y){x*(x+y)})
thermo51m_31m <- overlay(thermokarst14_51m_0cm, thermokarst14_31m_0cm, fun = function(x, y){x*(x+y)})
# This one gets all cells with at least one of the layers being thermokarst
karst_combined_1 <- overlay(thermokarst14_31m_0cm, thermokarst14_51m_0cm, fun = function(x,y){x+y})


# looking at these, there appears to be a pattern in all of the thermokarst by the time you hit 31 m
# 15 m is picking up random cells that aren't actually thermokarst, because the area the median is coming from isn't big enough to smooth out local variations?
# 31 m picks up some cells that may not be thermokarst on ridgelines, in particular
# try to include 31 m microtopography cells that are < -0.05 to include some of the features missed by 51 m, but that appear to actually be thermokarst
# 51 m does seem to fill in the spottiness of the 31 m window, so the thermokarst features are more continuous, but it might be missing smaller features
# need to check the last point
# 71 m fills in big features better, and eliminates smaller features - need to check if it is eliminating actual featurs that are just too small

# include 31 m < -0.05 and all 51 m and 71 m
karst_combined_2 <- overlay(thermokarst14_31m_5cm,
                            thermokarst14_51m_0cm,
                            thermokarst14_71m_0cm,
                            fun = function(x,y,z){x + y + z})


# plot(thermo15m_31m)
# plot(thermo15m_51m)
plot(thermo31m_51m)
# plot(thermo31m_15m)
# plot(thermo51m_15m)
plot(thermo51m_31m)
########################################################################################################################

### Fill in Holes in the Various Thermokarst Classification Rasters ####################################################
# this uses a filter where any cell that is not thermokarst, but has at least 6 cells surrounding it which are thermokarst,
# becomes thermokarst. This has to be run multiple times to make sure to catch all the cells in oddly shaped holes.
# I think 3 times should be enough, but need to check.

# neighbor cells to include when looking for thermokarst
weights_8_cell <- matrix(c(1,1,1, 1,0,1, 1,1,1), nrow = 3)
# matrix used to reclassify cells of 6 or greater to 1 (if there are at least 6 neighbor cells with thermokarst)
reclass_neighbor <- matrix(c(-Inf,5,0, 5,Inf,1), ncol = 3, byrow = TRUE)

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

# Fill the various thermokarst models
karst_31_fill <- fill(thermokarst14_31m_0cm, weights_8_cell, reclass_neighbor, reclass_matrix_0, 3)
karst_31_5_fill <- fill(thermokarst14_31m_5cm, weights_8_cell, reclass_neighbor, reclass_matrix_0, 3)
karst_51_fill <- fill(thermokarst14_51m_0cm, weights_8_cell, reclass_neighbor, reclass_matrix_0, 3)
karst_71_fill <- fill(thermokarst14_71m_0cm, weights_8_cell, reclass_neighbor, reclass_matrix_0, 3)
karst_combined_1_fill <- fill(karst_combined_1, weights_8_cell, reclass_neighbor, reclass_matrix_0, 3)
karst_combined_2_fill <- fill(karst_combined_2, weights_8_cell, reclass_neighbor, reclass_matrix_0, 3)

plot(karst_combined_2_fill1)
plot(karst_combined_2_fill)
########################################################################################################################

### Sample Cells for Ground Truthing ###################################################################################
samples <- sampleStratified(karst_combined_2_fill, size = 50, xy = TRUE)
samples2 <- sampleRandom(karst_combined_2_fill, size = 100)
########################################################################################################################

### convert rasters to simple features #################################################################################
# reclassify the rasters to replace 0 with NA for viewing in ArcMap
# this will need to be updated to include all of the rasters included in testing
reclass_matrix_0 <- c(-Inf,0,0, 1,Inf,1)
karst_31 <- reclassify(karst_31_fill, rcl = reclass_matrix_0)
karst_31_5 <- reclassify(karst_31_5_fill, rcl = reclass_matrix_0)
karst_51 <- reclassify(karst_51_fill, rcl = reclass_matrix_0)
karst_71 <- reclassify(karst_71_fill, rcl = reclass_matrix_0)
karst_combined_1 <- reclassify(karst_combined_1_fill, rcl = reclass_matrix_0)
karst_combined_2 <- reclassify(karst_combined_2_fill, rcl = reclass_matrix_0)

# convert to simple features
karst_31_sf <- st_as_sf(rasterToPolygons(karst_31, dissolve = TRUE))
karst_31_5_sf <- st_as_sf(rasterToPolygons(karst_31_5, dissolve = TRUE))
karst_51_sf <- st_as_sf(rasterToPolygons(karst_51, dissolve = TRUE))
karst_71_sf <- st_as_sf(rasterToPolygons(karst_71, dissolve = TRUE))
karst_combined_1_sf <- st_as_sf(rasterToPolygons(karst_combined_1, dissolve = TRUE))
karst_combined_2_sf <- st_as_sf(rasterToPolygons(karst_combined_2, dissolve = TRUE))


# st_write(karst_31_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_31.shp')
# st_write(karst_31_5_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_31_5.shp')
# st_write(karst_51_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_51.shp')
# st_write(karst_71_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_71.shp')
# st_write(karst_combined_1_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_31_51_combined.shp')
# st_write(karst_combined_2_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_31_5_51_71_combined.shp')
# st_write(karst_combined_2_fill_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/thermokarst_classification/karst_31_5_51_71_fill_combined.shp')
########################################################################################################################

### TO-DO:
##### 1
# D # it looks like GLiHT and NEON products are a few meters offset - need to fix somehow
# O # how well do the various NEON products line up? Can I align the GLiHT dataset to the NEON LiDAR?
# N # 
# E # fixed with a simple offset, although there might be a bit of rotation between the two datasets

##### 2
# D # Find a way to remove erroneous classification of thermokarst on ridgelines
# O # These are mostly single cells to a handful of cells
# N # Double checking in ArcMap, it looks like at least a solid handful of these are actually thermokarst features - maybe they don't need to be removed?
# E # 
##### Not sure if this is final, but I used a 5 cm cut-off on the 31 m microtopography raster to eliminate some of the superflous cells,
##### but keep as many of the actual thermokarst features as possible

##### 3
# D # Remove holes from thermokarst features (clump() does not do this, but it does give each clump a number, 
# O # which could be useful for counting features)
# N # Fill all cells that have thermokarst features on at least 6 sides? Run this a few times to fill in holes of multiple cells.
# E # 
##### I removed holes which were 1 cell wide, even if they turned, but left holes that were 4 m^2, using the thermokarst on 6 sides rule

### 4
# create a points shapefile with randomly selected locations (both thermokarst and not) to use to 'ground truth' with high res imagery
# create one shapefile, extract thermokarst classification from all of the different versions being tested, and add column for visual verification
# calculate overall, users, and producers accuracies for the various thermokarst classifications

### future ideas
# compare to cipehr plot locations with known thermokarst (only for 2018 data, because there's not a whole lot by 2014)
# plot thermokarst over neon high-res imagery - I do this in ArcMap for quick panning/zooming capability
# try merging 31 m and 51 m thermokarst if neither performs well by itself?
# count up # of thermokarst features using clump() once ground truthing has determined a 'best' thermokarst model



# plot over rgb
plotRGB(rgb)
plot(karst_sf, add = TRUE)

plotRGB(rgb)
plot(karst_31_sf, add = TRUE)

