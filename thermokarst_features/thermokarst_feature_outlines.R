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

### plot
# plot(microtopo14_15m)
plot(microtopo14_31m)
plot(microtopo14_51m)
plot(microtopo14_71m)
########################################################################################################################

### Reclassify Microtopography as Thermokarst ##########################################################################
## create matrices to use for cutoff values
## I'm not sure what cut-off makes sense. Vertical accuracy is ~15-22.5 cm. However, the average error over the moving window should be 0.
reclass_matrix_0cm <- c(-Inf,0,1, 0,Inf,0)
reclass_matrix_10cm <- c(-Inf,-0.1,1, -0.1,Inf,0)
reclass_matrix_20cm <- c(-Inf,-0.2,1, -0.2,Inf,0)

## 15 m
# thermokarst14_15m_0cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_0cm)
# thermokarst14_15m_15cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_15cm)
# thermokarst14_15m_20cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_20cm)

## 31 m
thermokarst14_31m_0cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_0cm)
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

### Check whether there are points in the 15 m moving window that get missed in the larger moving windows and vice versa
# value of 1 means that it is thermokarst using the first input only, 2 means the cell is thermokarst in both inputs
# thermo15m_31m <- overlay(thermokarst14_15m_0cm, thermokarst14_31m_0cm, fun = function(x, y){x*(x+y)})
# thermo15m_51m <- overlay(thermokarst14_15m_0cm, thermokarst14_51m_0cm, fun = function(x, y){x*(x+y)})
thermo31m_51m <- overlay(thermokarst14_31m_0cm, thermokarst14_51m_0cm, fun = function(x, y){x*(x+y)})
# thermo31m_15m <- overlay(thermokarst14_31m_0cm, thermokarst14_15m_0cm, fun = function(x, y){x*(x+y)})
# thermo51m_15m <- overlay(thermokarst14_51m_0cm, thermokarst14_15m_0cm, fun = function(x, y){x*(x+y)})
thermo51m_31m <- overlay(thermokarst14_51m_0cm, thermokarst14_31m_0cm, fun = function(x, y){x*(x+y)})
# This one gets all cells with at least one of the layers being thermokarst
karst_combined <- overlay(thermokarst14_31m_0cm, thermokarst14_51m_0cm, fun = function(x,y){x+y})


# looking at these, there appears to be a pattern in all of the thermokarst by the time you hit 31 m
# 15 m might be picking up random cells that aren't actually thermokarst, because the area the median is coming from isn't big enough to smooth out local variations?
# 51 m does seem to fill in the spottiness of the 31 m window, though, so the thermokarst features are more continuous, but it might be missing smaller features
# need to check the last point
# 71 m fills in big features better, and eliminates smaller features - need to check if it is eliminating actual featurs that are just too small
# 31 m is getting too many features on the ridgelines that just look like lichen patches, but not thermokarst
# plot(thermo15m_31m)
# plot(thermo15m_51m)
plot(thermo31m_51m)
# plot(thermo31m_15m)
# plot(thermo51m_15m)
plot(thermo51m_31m)

# reclassify the combined 31 m and 51 m raster
reclass_matrix <- c(-Inf,0,NA, 1,Inf,1)
karst_combined <- reclassify(karst_combined, rcl = reclass_matrix)
karst_sf <- st_as_sf(rasterToPolygons(karst_combined, dissolve = TRUE))

# reclassify the 31 m raster
karst_31 <- reclassify(thermokarst14_31m_0cm, rcl = reclass_matrix)
karst_31_sf <- st_as_sf(rasterToPolygons(karst_31, dissolve = TRUE))

# reclassify the 51 m raster
karst_51 <- reclassify(thermokarst14_51m_0cm, rcl = reclass_matrix)
karst_51_sf <- st_as_sf(rasterToPolygons(karst_51, dissolve = TRUE))

# reclassify the 71 m raster
karst_71 <- reclassify(thermokarst14_71m_0cm, rcl = reclass_matrix)
karst_71_sf <- st_as_sf(rasterToPolygons(karst_71, dissolve = TRUE))

# st_write(karst_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/karst_31_51_combined.shp')
# st_write(karst_31_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/karst_31.shp')
# st_write(karst_51_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/karst_51.shp')
# st_write(karst_71_sf, 'C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/karst_71.shp')

### TO-DO:
# D ### 1
# O # it looks like GLiHT and NEON products are a few meters offset - need to fix somehow
# N # how well do the various NEON products line up? Can I align the GLiHT dataset to the NEON LiDAR?
# E # fixed with a simple offset, although there might be a bit of rotation between the two datasets

###2
# Find a way to remove erroneous classification of thermokarst on ridgelines
# These are mostly single cells to a handful of cells
# Double checking in ArcMap, it looks like at least a solid handful of these are actually thermokarst features - maybe they don't need to be removed?
# Remove holes from thermokarst features (clump() does not do this, but it does give each clump a number, which could be useful for counting features)

### 3
# create a points shapefile with randomly selected locations (both thermokarst and not) to use to 'ground truth' with high res imagery
# create one shapefile, extract thermokarst classification from all of the different versions being tested, and add column for visual verification
# calculate overall, users, and producers accuracies for the various thermokarst classifications

### future ideas
# compare to cipehr plot locations with known thermokarst (only for 2018 data, because there's not a whole lot by 2014)
# plot thermokarst over neon high-res imagery - I do this in ArcMap for quick panning/zooming capability
# try merging 31 m and 51 m thermokarst if neither performs well by itself?



# plot over rgb
plotRGB(rgb)
plot(karst_sf, add = TRUE)

plotRGB(rgb)
plot(karst_31_sf, add = TRUE)

