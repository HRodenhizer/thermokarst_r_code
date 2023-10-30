###########################################################################################################
###                               Pan-Sharpen Digital Globe Imagery                                     ###
###                                        Code by HGR 3/20                                             ###
###########################################################################################################

### Load Packages #########################################################################################
library(raster)
library(RStoolbox)
library(tidyverse)
###########################################################################################################

### Session Settings ###################################################################################################
rasterOptions()
rasterOptions(maxmemory = 1e+12)
###########################################################################################################

### Load Data #############################################################################################
filenames.mul <- list.files('/scratch/hgr7/wv2/multiband',
                            pattern = '.+(R02C2|R02C3|R03C2|R03C3).+TIF$',
                            full.names = TRUE)
mul <- merge(brick(filenames.mul[[1]]),
             brick(filenames.mul[[2]]),
             brick(filenames.mul[[3]]),
             brick(filenames.mul[[4]]))
NAvalue(mul) <- -9999
# plotRGB(mul, 5, 3, 2)
filenames.pan <- list.files('/scratch/hgr7/wv2/panchromatic',
                            pattern = '.+(R02C2|R02C3|R03C2|R03C3).+TIF$',
                            full.names = TRUE)
pan <- merge(brick(filenames.pan[[1]]),
             brick(filenames.pan[[2]]),
             brick(filenames.pan[[3]]),
             brick(filenames.pan[[4]]))
NAvalue(pan) <- -9999
# plot(pan)
###########################################################################################################

### Pan-Sharpen Image #####################################################################################
start <- Sys.time()
pan.sharp <- panSharpen(mul, pan, r = 5, g = 3, b = 2, method = 'ihs')
end <- Sys.time()
difftime(start, end)

writeRaster(pan.sharp, '/scratch/hgr7/wv2/18OCT01221048-sharp.tif')

plotRGB(pan.sharp)
###########################################################################################################

### Crop to NEON Cropped Extent ###########################################################################
crs(pan.sharp)
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
pan.crop <- crop(pan.sharp, crop_extent)

writeRaster(pan.crop, '/scratch/hgr7/wv2/18OCT01221048-sharp_crop.tif')
###########################################################################################################