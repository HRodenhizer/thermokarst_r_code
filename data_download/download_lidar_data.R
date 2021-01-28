###########################################################################################################
###                                       NEON LiDAR Download                                           ###
###                                        Code by HGR 10/19                                            ###
###########################################################################################################

### Load packages #########################################################################################
library(raster)
library(tidyverse)
library(neonUtilities)
###########################################################################################################

### User Input ############################################################################################
# choose your data type - currently: lidar level 3 data
data_type <- 'DP3.30024.001'
# choose your site - currently: Healy
site <- 'HEAL'
# choose year - currently: 2019
year <- '2019'
# choose a folder in which to download the new files
# this is currently set up to use the same folder organization as I have used previously
# make sure the folder exists on your computer before trying to download to this location
download_filepath <- paste('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_', year, '/DTM_All/', sep = '')
# this is the filepath to the individual DTM files once they have been downloaded
dtm_filepath <- paste(download_filepath, 'DP3.30024.001/', year, '/FullSite/D', as.character(as.numeric(year)-2000), '/', year, '_HEAL_', as.character(as.numeric(year)-2016),'/L3/DiscreteLidar/DTMGtif/', sep = '')
# this is the filepath to the individual DSM files once they have been downloaded
dsm_filepath <- paste(download_filepath, 'DP3.30024.001/', year, '/FullSite/D', as.character(as.numeric(year)-2000), '/', year, '_HEAL_', as.character(as.numeric(year)-2016),'/L3/DiscreteLidar/DSMGtif/', sep = '')
# choose a folder in which to save the merged raster file
# this will automatically add the year to the filename and put it in the same folder as I have previously used
save_filepath_local <- paste('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All/NEON_DTM_', year, '.tif', sep = '')
# choose server file to save output to
dtm_save_filepath_server <- paste('Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Remote Sensing/NEON/DTM/NEON_DTM_', year, '.tif', sep = '')
dsm_save_filepath_server <- paste('Z:/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Remote Sensing/NEON/DSM/NEON_DSM_', year, '.tif', sep = '')
###########################################################################################################

### Download LiDAR DTM, DSM, and metadata #################################################################
# this will download all of the DTM files into one folder
# this takes about 5-10 minutes on my computer on NAU wifi (when operating normally)
byFileAOP(dpID = data_type,
          site = site,
          year = year,
          savepath = download_filepath)
###########################################################################################################

### Create mosaic and save ################################################################################
### DTM
# list all the filenames
filenames <- list.files(dtm_filepath,
                        full.names = TRUE)

# import all of the rasters
raster_files <- map(filenames, ~ raster(.x))

# merge the many raster files into one - this takes a few minutes
NEON_DTM <- do.call(merge, raster_files)

# plot to test that the whole thing is there and looks good
plot(NEON_DTM)

# writeRaster(NEON_DTM, save_filepath_local)
# writeRaster(NEON_DTM, dtm_save_filepath_server)

### DSM
# list all the filenames
filenames <- list.files(dsm_filepath,
                        full.names = TRUE)

# import all of the rasters
raster_files <- map(filenames, ~ raster(.x))

# merge the many raster files into one - this takes a few minutes
NEON_DSM <- do.call(merge, raster_files)

# plot to test that the whole thing is there and looks good
plot(NEON_DSM)

# writeRaster(NEON_DSM, dsm_save_filepath_server)
###########################################################################################################


