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
########################################################################################################################

### Calculate Moving Window Median Elevation ###########################################################################
## using the median elevation should not be too influenced by the relatively small portion of thermokarst within any moving window
f### create a matrix of weights to calculate the median
## 15 m diameter circle
weights14_15m <- focalWeight(elev14, 7, type = 'circle')*149 # the focalWeight function returns values that add up to 1, but we want each included cell to have a value of 1
## 31 m diameter circle
weights14_31m <- focalWeight(elev14, 15, type = 'circle')*709
## 51 m diameter circle
weights14_51m <- focalWeight(elev14, 25, type = 'circle')*1961


### calculate median elevation
## 15 m
median14_15m <- focal(elev14, w = weights14_15m, fun = median)
## 31 m
median14_31m <- focal(elev14, w = weights14_31m, fun = median)
## 51 m
median14_51m <- focal(elev14, w = weights14_51m, fun = median)
########################################################################################################################

### Calculate Microtopography (deviance from median elevation) #########################################################
### this requires ~2 minutes to run each line on my computer
### positive values are higher than the surrounding area, negative values are lower and could be thermokarst
## 15 m
microtopo14_15m <- elev14 - median14_15m
## 31 m
microtopo14_31m <- elev14 - median14_31m
## 51 m
microtopo14_51m <- elev14 - median14_51m

### plot
plot(microtopo14_15m)
plot(microtopo14_31m)
plot(microtopo14_51m)
########################################################################################################################

### Reclassify Microtopography as Thermokarst ##########################################################################
## create matrices to use for cutoff values
## I'm not sure what cut-off makes sense. Vertical accuracy is ~15-22.5 cm. However, the average error over the moving window should be 0.
reclass_matrix_0cm <- c(-1,0,1, 0,1,0)
reclass_matrix_15cm <- c(-1,-0.15,1, -0.15,1,0)
reclass_matrix_20cm <- c(-1,-0.2,1, -0.2,1,0)

## 15 m
thermokarst14_15m_0cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_0cm)
thermokarst14_15m_15cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_15cm)
thermokarst14_15m_20cm <- reclassify(microtopo14_15m, rcl = reclass_matrix_20cm)

## 31 m
thermokarst14_31m_0cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_0cm)
thermokarst14_31m_15cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_15cm)
thermokarst14_31m_20cm <- reclassify(microtopo14_31m, rcl = reclass_matrix_20cm)

## 51 m
thermokarst14_51m_0cm <- reclassify(microtopo14_51m, rcl = reclass_matrix_0cm)
thermokarst14_51m_15cm <- reclassify(microtopo14_51m, rcl = reclass_matrix_15cm)
thermokarst14_51m_20cm <- reclassify(microtopo14_51m, rcl = reclass_matrix_20cm)


### plot the thermokarst features with the same focal window and different cut-off values on top of each other
# set plot viewer to have two rows and one column
par(mfrow=c(3, 1))

##15 m
plot(thermokarst14_15m_0cm)
plot(thermokarst14_15m_15cm)
plot(thermokarst14_15m_20cm)

# 31 m
plot(thermokarst14_31m_0cm)
plot(thermokarst14_31m_15cm)
plot(thermokarst14_31m_20cm)

## 51 m
plot(thermokarst14_51m_0cm)
plot(thermokarst14_51m_15cm)
plot(thermokarst14_51m_20cm)

par(mfrow=c(1,1))
########################################################################################################################

### Check whether there are points in the 15 m moving window that get missed in the larger moving windows and vice versa
# value of 1 means that it is thermokarst using the first input only, 2 means the cell is thermokarst in both inputs
thermo15m_31m <- overlay(thermokarst14_15m_0cm, thermokarst14_31m_0cm, fun = function(x, y){x*(x+y)})
thermo15m_51m <- overlay(thermokarst14_15m_0cm, thermokarst14_51m_0cm, fun = function(x, y){x*(x+y)})
thermo31m_51m <- overlay(thermokarst14_31m_0cm, thermokarst14_51m_0cm, fun = function(x, y){x*(x+y)})

# looking at these, I think the 31 m window probably does the best job (there appears to be a pattern in all of the thermokarst by the time you hit 31 m)
# 15 m might be picking up random cells that aren't actually thermokarst, because the are the median is coming from isn't big enough to smooth out local variations?
plot(thermo15m_31m)
plot(thermo15m_51m)
plot(thermo31m_51m)

### future ideas
# compare to cipehr plot locations with known thermokarst
# plot thermokarst over neon high-res imagery
# use mapview to plot over google imagery