########################################################################################################################
###                                  Calculate Subsidence from NEON Data                                             ###
###                                         Code by HGR 3/2020                                                       ###
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
elev <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
rm(filenames)
########################################################################################################################

### Calculate Subsidence ###############################################################################################
# calculate subsidence layers separately and then join into a brick
sub18 <- elev[[2]] - elev[[1]]
sub18[which(is.nan(sub18@data@values))] <- NA
sub19 <- elev[[3]] - elev[[1]]
sub19[which(is.nan(sub19@data@values))] <- NA
sub <- brick(sub18, sub19)
for (i in 1:nlayers(sub)) {
  sub[[i]][which(is.nan(sub[[i]]@data@values))] <- NA
}
rm(sub18, sub19)

# look at values
# summary(sub[[1]])
# cellStats(sub[[1]], stat = mean)
# boxplot(sub[[1]])
# summary(sub[[2]])
# cellStats(sub[[2]], stat = mean)
# boxplot(sub[[2]])
# plot(sub[[1]])
# plot(sub[[2]])

# writeRaster(sub[[1]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2018.tif')
# writeRaster(sub[[2]], 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2019.tif')
########################################################################################################################
