########################################################################################################################
###                       Determine Thermokarst Feature Outlines from NEON Data                                      ###
###                                         Code by HGR 2/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(ggthemes)
library(doParallel)
library(tidyverse)
########################################################################################################################

### Session Settings ###################################################################################################
# rasterOptions()
# rasterOptions(maxmemory = 1e+12)
########################################################################################################################

### Load Data ##########################################################################################################
filenames <- list.files('/scratch/hgr7/DTM_all',
                        full.names = TRUE,
                        pattern = '.tif$')
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
#Define how many cores you want to use
UseCores <- 3

#Register CoreCluster
# cl <- makeCluster(UseCores)
# registerDoParallel(cl)

# 15 m radius mean will take about 4 hours on monsoon
#Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
#   
#   median <- focal(elev[[i]], weights[[1]], fun = median)
#   
#   
#   outname <- paste('/scratch/hgr7/int_output/median15_',
#                    i,
#                    '.tif',
#                    sep = '')
#   
#   writeRaster(median, 
#               filename  = outname,
#               overwrite = T)
#   
# }
# end <- Sys.time()
# difftime(end, start)

# 25 m radius will take about 6 hours on monsoon
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
#   
#   median <- focal(elev[[i]], weights[[2]], fun = median)
#   
#   
#   outname <- paste('/scratch/hgr7/int_output/median25_',
#                    i,
#                    '.tif',
#                    sep = '')
#   
#   writeRaster(median, 
#               filename  = outname,
#               overwrite = T)
#   
# }
# end <- Sys.time()
# difftime(end, start)

# 35 m radius will take about 12 hours on monsoon
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
#   
#   median <- focal(elev[[i]], weights[[3]], fun = median)
#   
#   
#   outname <- paste('/scratch/hgr7/int_output/median35_',
#                    i,
#                    '.tif',
#                    sep = '')
#   
#   writeRaster(median, 
#               filename  = outname,
#               overwrite = T)
#   
# }
# end <- Sys.time()
# difftime(end, start)
# 
# #end cluster
# stopCluster(cl)

# load median rasters
filenames <- list.files('/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '.tif$')

median15 <- list(raster(filenames[which(str_detect(filenames, 'median15_1'))]), # 2017
                 raster(filenames[which(str_detect(filenames, 'median15_2'))]), # 2018
                 raster(filenames[which(str_detect(filenames, 'median15_3'))])) # 2019

median25 <- list(raster(filenames[which(str_detect(filenames, 'median25_1'))]), # 2017
                 raster(filenames[which(str_detect(filenames, 'median25_2'))]), # 2018
                 raster(filenames[which(str_detect(filenames, 'median25_3'))])) # 2019

median35 <- list(raster(filenames[which(str_detect(filenames, 'median35_1'))]), # 2017
                 raster(filenames[which(str_detect(filenames, 'median35_2'))]), # 2018
                 raster(filenames[which(str_detect(filenames, 'median35_3'))])) # 2019

# plot(median15[[1]])
# plot(median15[[2]])
# plot(median15[[3]])
# plot(median25[[1]])
# plot(median25[[2]])
# plot(median25[[3]])
# plot(median35[[1]])
# plot(median35[[2]])
# plot(median35[[3]])


# print(paste('start median15 time: ', Sys.time(), sep = ''))
# median15_1 <- focal(elev[[1]], weights[[1]], fun = median)
# median15 <- map(elev, ~ focal(.x, weights[[1]], fun = median))
# print(paste('end median15 time: ', Sys.time(), sep = ''))
# writeRaster(median15[[1]], '/scratch/hgr7/median/median15_1.tif')
# writeRaster(median15[[2]], '/scratch/hgr7/median/median15_2.tif')
# writeRaster(median15[[3]], '/scratch/hgr7/median/median15_3.tif')
# 
# print(paste('start median25 time: ', Sys.time(), sep = ''))
# median25 <- map(elev, ~ focal(.x, weights[[2]], fun = median, progress = "text"))
# print(paste('end median25 time: ', Sys.time(), sep = ''))
# writeRaster(median25[[1]], '/scratch/hgr7/median/median25_1.tif')
# writeRaster(median25[[2]], '/scratch/hgr7/median/median25_2.tif')
# writeRaster(median25[[3]], '/scratch/hgr7/median/median25_3.tif')
# 
# print(paste('start median35 time: ', Sys.time(), sep = ''))
# median35 <- map(elev, ~ focal(.x, weights[[3]], fun = median, progress = "text"))
# print(paste('end median35 time: ', Sys.time(), sep = ''))
# writeRaster(median35[[1]], '/scratch/hgr7/median/median35_1.tif')
# writeRaster(median35[[2]], '/scratch/hgr7/median/median35_2.tif')
# writeRaster(median35[[3]], '/scratch/hgr7/median/median35_3.tif')

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
# #Register CoreCluster
# cl <- makeCluster(UseCores)
# registerDoParallel(cl)
# 
# # Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   mtopo <- elev[[i]] - median15[[i]]
# 
#   outname <- paste('/scratch/hgr7/int_output/mtopo15_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(mtopo,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# #Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   mtopo <- elev[[i]] - median25[[i]]
# 
#   outname <- paste('/scratch/hgr7/int_output/mtopo25_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(mtopo,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# #Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   mtopo <- elev[[i]] - median35[[i]]
# 
#   outname <- paste('/scratch/hgr7/int_output/mtopo35_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(mtopo,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# #end cluster
# stopCluster(cl)

# load microtopography rasters
filenames <- list.files('/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '.tif$')

mtopo15 <- list(raster(filenames[which(str_detect(filenames, 'mtopo15_1'))]), # 2017
                 raster(filenames[which(str_detect(filenames, 'mtopo15_2'))]), # 2018
                 raster(filenames[which(str_detect(filenames, 'mtopo15_3'))])) # 2019

mtopo25 <- list(raster(filenames[which(str_detect(filenames, 'mtopo25_1'))]), # 2017
                 raster(filenames[which(str_detect(filenames, 'mtopo25_2'))]), # 2018
                 raster(filenames[which(str_detect(filenames, 'mtopo25_3'))])) # 2019

mtopo35 <- list(raster(filenames[which(str_detect(filenames, 'mtopo35_1'))]), # 2017
                 raster(filenames[which(str_detect(filenames, 'mtopo35_2'))]), # 2018
                 raster(filenames[which(str_detect(filenames, 'mtopo35_3'))])) # 2019

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
# #Register CoreCluster
# cl <- makeCluster(UseCores)
# registerDoParallel(cl)
# 
# # Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   karst <- reclassify(mtopo15[[i]], reclass_matrix_0cm)
# 
#   outname <- paste('/scratch/hgr7/int_output/karst15_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# # Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   karst <- reclassify(mtopo25[[i]], reclass_matrix_0cm)
# 
#   outname <- paste('/scratch/hgr7/int_output/karst25_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# # Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   karst <- reclassify(mtopo35[[i]], reclass_matrix_0cm)
# 
#   outname <- paste('/scratch/hgr7/int_output/karst35_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# # reclassify values < -5 as thermokarst (15 m radius only)
# # Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(elev)) %dopar% {
#   library(raster)
# 
#   karst <- reclassify(mtopo15[[i]], reclass_matrix_5cm)
# 
#   outname <- paste('/scratch/hgr7/int_output/karst15_5_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# # 
# 
# #end cluster
# stopCluster(cl)

# load thermokarst rasters
filenames <- list.files('/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '.tif$')

karst15 <- list(raster(filenames[which(str_detect(filenames, 'karst15_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst15_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst15_3'))])) # 2019

karst25 <- list(raster(filenames[which(str_detect(filenames, 'karst25_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst25_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst25_3'))])) # 2019

karst35 <- list(raster(filenames[which(str_detect(filenames, 'karst35_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst35_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst35_3'))])) # 2019

karst15_5 <- list(raster(filenames[which(str_detect(filenames, 'karst15_5_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst15_5_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst15_5_3'))])) # 2019

### plot the thermokarst features
map(karst15, ~ plot(.x))
map(karst25, ~ plot(.x))
map(karst35, ~ plot(.x))
map(karst15_5, ~ plot(.x))
########################################################################################################################

### Fill in Holes in the Various Thermokarst Classification Rasters ####################################################
# this uses a filter where any cell that is not thermokarst, but has at least 6 cells surrounding it which are thermokarst,
# becomes thermokarst. This has to be run multiple times to make sure to catch all the cells in oddly shaped holes.
# I think 3 times should be enough, but need to check.

# # Create a function to iteratively fill in all cells with at least 6 thermokarst cells in the 8 immediately surrounding cells
# fill <- function(raster, weights, reclass_matrix_neighbor, reclass_matrix_thermokarst, n) {
#   for (i in 1:n) {
#     if (i == 1) {
#       # Determine whether a cell is a hole in a thermokarst feature
#       karst_neighbor <- reclassify(focal(raster, weights, fun = sum, na.rm = TRUE), rcl = reclass_matrix_neighbor)
#       # Fill in the identified holes
#       fill <- reclassify(overlay(raster, karst_neighbor, fun = function(x,y){x + y}), rcl = reclass_matrix_thermokarst)
#     } else {
#       # Again, determine whether a cell is a hole in a thermokarst feature
#       karst_neighbor <- reclassify(focal(fill, weights, fun = sum, na.rm = TRUE), rcl = reclass_matrix_neighbor)
#       # Again, fill in the identified holes
#       fill <- reclassify(overlay(fill, karst_neighbor, fun = function(x,y){x + y}), rcl = reclass_matrix_thermokarst)
#     }
#   }
#   return(fill)
# }
# 
# # neighbor cells to include when looking for thermokarst
# weights_8_cell <- matrix(c(1,1,1, 1,0,1, 1,1,1), nrow = 3)
# # matrix used to reclassify cells of 6 or greater to 1 (if there are at least 6 neighbor cells with thermokarst)
# reclass_neighbor <- matrix(c(-Inf,5,0, 5,Inf,1), ncol = 3, byrow = TRUE)
# 
# # Fill the various thermokarst models
# karst15_fill <- map(karst15, ~ fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
# karst15_5_fill <- map(karst15_5, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
# karst25_fill <- map(karst25, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))
# karst35_fill <- map(karst35, fill(.x, weights_8_cell, reclass_neighbor, reclass_matrix_0cm, 3))

# this function fills in holes by dilating (classifying cells next to thermokarst as thermokarst)
# and then eroding (removing one layer of thermokarst cells from the outside of each thermokarst feature)
# fill_gaps <- function(raster,
#                       dilate_n = 2,
#                       erode_n = 2,
#                       dilate_kernel = matrix(c(0,1,0, 1,1,1, 0,1,0), nrow = 3),
#                       erode_kernel = matrix(c(0,1,0, 1,1,1, 0,1,0), nrow = 3)) {
#   
#   raster_array <- as.array(
#     matrix(
#       raster[,],
#       nrow = raster@nrows,
#       ncol = raster@ncols
#     )
#   )
#   
#   for (i in 1:dilate_n) {
#     raster_array <- mmand::dilate(
#       raster_array,
#       dilate_kernel)
#   }
#   
#   for (i in 1:erode_n) {
#     raster_array <- mmand::erode(
#       raster_array,
#       erode_kernel)
#   }
#   
#   filled_vector <- as.vector(raster_array)
#   filled_raster <- raster
#   filled_raster[,] <- filled_vector
#   
#   return(filled_raster)
# }
# 
# test <- fill_gaps(karst15[[1]])
# 
# # # Fill the various thermokarst models
# start <- Sys.time()
# karst15_fill <- map(karst15, ~ fill_gaps(.x))
# end <- Sys.time()
# difftime(end, start)
# writeRaster(karst15_fill[[1]], '/scratch/hgr7/int_output/karst15_fill_1.tiff')
# writeRaster(karst15_fill[[2]], '/scratch/hgr7/int_output/karst15_fill_2.tiff')
# writeRaster(karst15_fill[[3]], '/scratch/hgr7/int_output/karst15_fill_3.tiff')
# 
# start <- Sys.time()
# karst15_5_fill <- map(karst15_5, ~ fill_gaps(.x))
# end <- Sys.time()
# difftime(end, start)
# writeRaster(karst15_5_fill[[1]], '/scratch/hgr7/int_output/karst15_5_fill_1.tiff')
# writeRaster(karst15_5_fill[[2]], '/scratch/hgr7/int_output/karst15_5_fill_2.tiff')
# writeRaster(karst15_5_fill[[3]], '/scratch/hgr7/int_output/karst15_5_fill_3.tiff')
# 
# start <- Sys.time()
# karst25_fill <- map(karst25, ~ fill_gaps(.x))
# end <- Sys.time()
# difftime(end, start)
# writeRaster(karst25_fill[[1]], '/scratch/hgr7/int_output/karst25_fill_1.tiff')
# writeRaster(karst25_fill[[2]], '/scratch/hgr7/int_output/karst25_fill_2.tiff')
# writeRaster(karst25_fill[[3]], '/scratch/hgr7/int_output/karst25_fill_3.tiff')
# 
# start <- Sys.time()
# karst35_fill <- map(karst35, ~ fill_gaps(.x))
# end <- Sys.time()
# difftime(end, start)
# writeRaster(karst35_fill[[1]], '/scratch/hgr7/int_output/karst35_fill_1.tiff')
# writeRaster(karst35_fill[[2]], '/scratch/hgr7/int_output/karst35_fill_2.tiff')
# writeRaster(karst35_fill[[3]], '/scratch/hgr7/int_output/karst35_fill_3.tiff')

# load thermokarst rasters
filenames <- list.files('/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '.tif$')

karst15_fill <- list(raster(filenames[which(str_detect(filenames, 'karst15_fill_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst15_fill_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst15_fill_3'))])) # 2019

karst25_fill <- list(raster(filenames[which(str_detect(filenames, 'karst25_fill_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst25_fill_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst25_fill_3'))])) # 2019

karst35_fill <- list(raster(filenames[which(str_detect(filenames, 'karst35_fill_1'))]), # 2017
                raster(filenames[which(str_detect(filenames, 'karst35_fill_2'))]), # 2018
                raster(filenames[which(str_detect(filenames, 'karst35_fill_3'))])) # 2019

karst15_5_fill <- list(raster(filenames[which(str_detect(filenames, 'karst15_5_fill_1'))]), # 2017
                  raster(filenames[which(str_detect(filenames, 'karst15_5_fill_2'))]), # 2018
                  raster(filenames[which(str_detect(filenames, 'karst15_5_fill_3'))])) # 2019
########################################################################################################################

### Remove Landscape Features Not Due to Thermokarst ###################################################################
### I don't think I will actually use this, because it removes only portions of streams, and it doesn't remove those completely
# flow_accum <- list(raster('/scratch/hgr7/hydrologic_flow/flow_accum_17.tif'),
#                    raster('/scratch/hgr7/hydrologic_flow/flow_accum_18.tif'),
#                    raster('/scratch/hgr7/hydrologic_flow/flow_accum_19.tif'))

# # load in flow accumulation that was created in ArcMap
# flow_accum <- raster('/scratch/hgr7/hydrologic_flow/flow_accum_17.tif')

# # plot to find likely cut-off value that differentiates between thermokarst and canyons
# colors <- c('#000000', '#FFFFFF')
# breaks <- c(0, 20000000, 4e+07)
# 
# plot(flow_accum[[1]], breaks = breaks, col = colors)

# # reclassify the flow accumulation raster into a binary stream raster using a cut-off of 20,000,000
# streams <- reclassify(flow_accum,
#                       rcl = matrix(c(-Inf,20000000,NA, 20000000,Inf,1),
#                                    ncol = 3,
#                                    byrow = TRUE))
# writeRaster(streams, '/scratch/hgr7/hydrologic_flow/streams17_20000000.tif', overwrite = TRUE)

# # load in stream data
# streams <- raster('/scratch/hgr7/hydrologic_flow/streams17_20000000.tif')
# 
# # buffer streams 15 m on either side
# stream_buffer_15 <- buffer(streams, width = 15, dissolve = TRUE)
# stream_buffer_15[which(is.na(stream_buffer_15[]))] <- 0
# writeRaster(stream_buffer_15, '/scratch/hgr7/hydrologic_flow/streams17_20000000_buffer_15.tif', overwrite = TRUE)
# plot(stream_buffer_15)
# 
# stream_buffer_15 <- raster('/scratch/hgr7/hydrologic_flow/streams17_20000000_buffer_15.tif')
# 
# #Register CoreCluster
# cl <- makeCluster(UseCores)
# registerDoParallel(cl)
# 
# # Use foreach loop and %dopar% command
# start <- Sys.time()
# foreach(i=1:length(karst15_fill)) %dopar% {
#   library(raster)
# 
#   rm_streams <- reclassify(karst15_fill[[i]] - stream_buffer_15, rcl = matrix(c(-Inf,0,0, 0,Inf,1), ncol = 3, byrow = TRUE))
# 
#   outname <- paste('/scratch/hgr7/int_output/karst15_fill_filter_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(rm_streams,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# start <- Sys.time()
# foreach(i=1:length(karst25_fill)) %dopar% {
#   library(raster)
# 
#   rm_streams <- reclassify(karst25_fill[[i]] - stream_buffer_15, rcl = matrix(c(-Inf,0,0, 0,Inf,1), ncol = 3, byrow = TRUE))
# 
#   outname <- paste('/scratch/hgr7/int_output/karst25_fill_filter_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(rm_streams,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# start <- Sys.time()
# foreach(i=1:length(karst35_fill)) %dopar% {
#   library(raster)
# 
#   rm_streams <- reclassify(karst35_fill[[i]] - stream_buffer_15, rcl = matrix(c(-Inf,0,0, 0,Inf,1), ncol = 3, byrow = TRUE))
# 
#   outname <- paste('/scratch/hgr7/int_output/karst35_fill_filter_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(rm_streams,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# start <- Sys.time()
# foreach(i=1:length(karst15_5_fill)) %dopar% {
#   library(raster)
# 
#   rm_streams <- reclassify(karst15_5_fill[[i]] - stream_buffer_15, rcl = matrix(c(-Inf,0,0, 0,Inf,1), ncol = 3, byrow = TRUE))
# 
#   outname <- paste('/scratch/hgr7/int_output/karst15_5_fill_filter_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(rm_streams,
#               filename  = outname,
#               overwrite = T)
# 
# }
# end <- Sys.time()
# difftime(end, start)
# 
# # end cluster
# stopCluster(cl)
# 
# # load thermokarst rasters
# filenames <- list.files('/scratch/hgr7/int_output',
#                         full.names = TRUE,
#                         pattern = '.tif$')
# 
# karst15_fill_filter <- list(raster(filenames[which(str_detect(filenames, 'karst15_fill_filter_1'))]), # 2017
#                 raster(filenames[which(str_detect(filenames, 'karst15_fill_filter_2'))]), # 2018
#                 raster(filenames[which(str_detect(filenames, 'karst15_fill_filter_3'))])) # 2019
# 
# karst25_fill_filter <- list(raster(filenames[which(str_detect(filenames, 'karst25_fill_filter_1'))]), # 2017
#                 raster(filenames[which(str_detect(filenames, 'karst25_fill_filter_2'))]), # 2018
#                 raster(filenames[which(str_detect(filenames, 'karst25_fill_filter_3'))])) # 2019
# 
# karst35_fill_filter <- list(raster(filenames[which(str_detect(filenames, 'karst35_fill_filter_1'))]), # 2017
#                 raster(filenames[which(str_detect(filenames, 'karst35_fill_filter_2'))]), # 2018
#                 raster(filenames[which(str_detect(filenames, 'karst35_fill_filter_3'))])) # 2019
# 
# karst15_5_fill_filter <- list(raster(filenames[which(str_detect(filenames, 'karst15_5_fill_filter_1'))]), # 2017
#                   raster(filenames[which(str_detect(filenames, 'karst15_5_fill_filter_2'))]), # 2018
#                   raster(filenames[which(str_detect(filenames, 'karst15_5_fill_filter_3'))])) # 2019
# 
# ### plot the thermokarst features
# map(karst15_fill_filter, ~ plot(.x))
# map(karst25_fill_filter, ~ plot(.x))
# map(karst35_fill_filter, ~ plot(.x))
# map(karst15_5_fill_filter, ~ plot(.x))
########################################################################################################################

### Test Various Combinations of Thermokarst Classification for Completion and Accuracy ################################
# matrix to reclassify after 
# Register CoreCluster
# cl <- makeCluster(UseCores)
# registerDoParallel(cl)
# 
# # This one gets all cells with at least one of the layers being thermokarst
# karst_combined_1 <- list()
# foreach(i=1:length(karst15_fill)) %dopar% {
#   library(raster)
# 
#   karst_combined <- overlay(karst15_fill[[i]],
#                                    karst25_fill[[i]],
#                                    karst35_fill[[i]],
#                                    fun = function(x,y,z){ifelse(x > 0 | y > 0 | z > 0, 1, 0)})
# 
#   outname <- paste('/scratch/hgr7/int_output/karst_combined_1_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst_combined,
#               filename  = outname,
#               overwrite = T)
# 
# }
# 
# # include 31 m < -0.05 and all 51 m and 71 m
# karst_combined_2 <- list()
# foreach(i=1:length(karst15_fill)) %dopar% {
#   library(raster)
# 
#   karst_combined <- overlay(karst15_5_fill[[i]],
#                                    karst25_fill[[i]],
#                                    karst35_fill[[i]],
#                                    fun = function(x,y,z){ifelse(x > 0 | y > 0 | z > 0, 1, 0)})
# 
#   outname <- paste('/scratch/hgr7/int_output/karst_combined_2_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst_combined,
#               filename  = outname,
#               overwrite = T)
# 
# }
# 
# # 31 m and 51 m
# karst_combined_3 <- list()
# foreach(i=1:length(karst15_fill)) %dopar% {
#   library(raster)
# 
#   karst_combined <- overlay(karst15_fill[[i]],
#                                    karst25_fill[[i]],
#                                    fun = function(x,y){ifelse(x > 0 | y > 0, 1, 0)})
# 
#   outname <- paste('/scratch/hgr7/int_output/karst_combined_3_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst_combined,
#               filename  = outname,
#               overwrite = T)
# 
# }
# 
# # 51 m and 71 m
# karst_combined_4 <- list()
# foreach(i=1:length(karst15_fill)) %dopar% {
#   library(raster)
# 
#   karst_combined <- overlay(karst25_fill[[i]],
#                                    karst35_fill[[i]],
#                                    fun = function(x,y){ifelse(x > 0 | y > 0, 1, 0)})
# 
#   outname <- paste('/scratch/hgr7/int_output/karst_combined_4_',
#                    i,
#                    '.tif',
#                    sep = '')
# 
#   writeRaster(karst_combined,
#               filename  = outname,
#               overwrite = T)
# 
# }
# 
# #end cluster
# stopCluster(cl)

# load thermokarst rasters
filenames <- list.files('Y:/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '.tif$')

karst_combined_1 <- list(raster(filenames[which(str_detect(filenames, 'karst_combined_1_1'))]), # 2017
                         raster(filenames[which(str_detect(filenames, 'karst_combined_1_2'))]), # 2018
                         raster(filenames[which(str_detect(filenames, 'karst_combined_1_3'))])) # 2019

karst_combined_2 <- list(raster(filenames[which(str_detect(filenames, 'karst_combined_2_1'))]), # 2017
                         raster(filenames[which(str_detect(filenames, 'karst_combined_2_2'))]), # 2018
                         raster(filenames[which(str_detect(filenames, 'karst_combined_2_3'))])) # 2019

karst_combined_3 <- list(raster(filenames[which(str_detect(filenames, 'karst_combined_3_1'))]), # 2017
                         raster(filenames[which(str_detect(filenames, 'karst_combined_3_2'))]), # 2018
                         raster(filenames[which(str_detect(filenames, 'karst_combined_3_3'))])) # 2019

karst_combined_4 <- list(raster(filenames[which(str_detect(filenames, 'karst_combined_4_1'))]), # 2017
                         raster(filenames[which(str_detect(filenames, 'karst_combined_4_2'))]), # 2018
                         raster(filenames[which(str_detect(filenames, 'karst_combined_4_3'))])) # 2019

map(karst_combined_1, ~plot(.x))
map(karst_combined_2, ~plot(.x))
map(karst_combined_3, ~plot(.x))
map(karst_combined_4, ~plot(.x))
########################################################################################################################

### Sample Cells for Validation ########################################################################################
# crop_extent <- extent(matrix(c(387000, 394000, 7080500, 7089500), nrow = 2, byrow = TRUE))
# sample_raster <- crop(karst_combined_1[[1]], crop_extent)
# set.seed(33)
# samples <- st_as_sf(sampleStratified(sample_raster, size = 100, xy = TRUE, sp = TRUE)) %>%
#   select(-4)
# ggplot(samples, aes(x = x, y = y)) +
#   geom_point() +
#   coord_fixed()
# st_write(samples, '/scratch/hgr7/int_output/samples_stratified_100.shp', delete_layer = TRUE)
samples <- st_read('Y:/scratch/hgr7/int_output/samples_stratified_100.shp')

# make a brick of the various thermokarst classifications
extract_brick <- brick(karst15_fill[[2]],
                       karst15_5_fill[[2]],
                       karst25_fill[[2]],
                       karst35_fill[[2]],
                       karst_combined_1[[2]],
                       karst_combined_2[[2]],
                       karst_combined_3[[2]],
                       karst_combined_4[[2]])

# adjust the x-y location of the brick to line up with wv2 imagery
xmin(extract_brick) <- xmin(extract_brick) + 2.5
xmax(extract_brick) <- xmax(extract_brick) + 2.5
ymin(extract_brick) <- ymin(extract_brick) + 0
ymax(extract_brick) <- ymax(extract_brick) + 0

writeRaster(extract_brick, 'Y:/scratch/hgr7/output/thermokarst_brick_18_wv2_aligned_2.5.tif')

# extract values from each 2018 classification
karst_extract_18 <- st_as_sf(raster::extract(extract_brick, as(samples, 'Spatial'), layer = 1, nl = 8, sp = TRUE)) %>%
  rename(tk.15 = 4,
         tk.15.5 = 5,
         tk.25 = 6,
         tk.35 = 7,
         tk.comb.1 = 8,
         tk.comb.2 = 9,
         tk.comb.3 = 10,
         tk.comb.4 = 11) %>%
  mutate(validation = '')

st_write(karst_extract_18, 'Y:/scratch/hgr7/output/thermokarst_extract_18.shp', delete_layer = TRUE)
########################################################################################################################