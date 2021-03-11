#########################################################################################
###                          Run FFP on EML Tower 2017-2019                        ###
###                                Code by HGR 2/2021                                 ###
#########################################################################################

### Libraries ###########################################################################
library(sf)
library(raster)
library(data.table)
library(lubridate)
library(viridis)
library(doParallel)
library(tidyverse)
#########################################################################################

### Load data ###########################################################################
# # Ameriflux doesn't have gapfilled wind data, so maybe this is not the right choice.
# # Use Ameriflux data for most data
# ameriflux <- fread('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/Ameriflux/AMF_US-EML_BASE_HH_3-5.csv')

# Get most wind data from processed files
load('~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/AK17_CO2&CH4_30Apr2019.Rdata')
load('~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_CO2&CH4_30Apr2019.Rdata')
load('~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_CO2&CH4.Rdata')

flux <- rbind(Tower17.18, Tower18.19, Tower19.20)

# Get sigma_v and L (Obukhov Length) from unprocessed eddypro output
filenames <- c(list.files(path='~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/EC', full.names = TRUE),
               list.files(path='~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/EC', full.names = TRUE),
               list.files(path='~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/EC', full.names = TRUE)) 
eddy <- map_dfr(filenames,
                ~ fread(.x, head=T, na.strings=c("NAN", "-9999"))[, .(date, time, v_var, L)])
eddy[, date := mdy(date)]
eddy[, ts := ymd_hm(paste(date, time))]
eddy[, sigma_v := sqrt(v_var)]
eddy <- eddy[, .(ts, L, sigma_v)]
eddy <- eddy[ts >= as_date('2017-05-01') & ts < as_date('2020-05-01')]
eddy[is.na(L), L := -999]
eddy[is.na(sigma_v), sigma_v := -999]


#########################################################################################

### Format for FFP ######################################################################
# Select desired timeframe and variables
# Get all of the needed variables from the ameriflux file except V_SIGMA, which we
# haven't been uploading to Ameriflux
subset <-flux[ts >= as_date('2017-05-01') & ts < as_date('2020-05-01'),
                   .(ts, wind_speed_filter, `u*`, wind_dir)]
setnames(subset,
         old = c('ts', 'wind_speed_filter', 'u*', 'wind_dir'),
         new = c('ts', 'u_mean', 'u_star', 'wind_dir'))
subset[, ts := ymd_hms(ts)]
subset[is.na(u_mean) | u_mean == -9999, u_mean := -999]
subset[is.na(u_star) | u_star == -9999, u_star := -999]
subset[is.na(wind_dir) | wind_dir == -9999, wind_dir := -999]

flux.format <- merge(subset, eddy, by = 'ts', all = TRUE)
flux.format <- flux.format[, yyyy := year(ts)]
flux.format <- flux.format[, mm := month(ts)]
flux.format <- flux.format[, day := mday(ts)]
flux.format <- flux.format[, HH := hour(ts)]
flux.format <- flux.format[, MM := minute(ts)]
flux.format[is.na(u_mean), u_mean := -999]
flux.format[is.na(u_star), u_star := -999]
flux.format[is.na(wind_dir), wind_dir := -999]
flux.format[is.na(L), L := -999]
flux.format[is.na(sigma_v), sigma_v := -999]
flux.format[, z := 3.5]
flux.format[, d := 0.2]
flux.format[, zm := z - d]
flux.format[, z0 := 0.05]

### plot to make sure everything looks good
# wind speed over time
ggplot(flux.format[u_mean != -999], aes(x = ts, y = u_mean)) +
  geom_point()

# monin obukhov length over time
ggplot(flux.format[L != -999], aes(x = ts, y = L)) +
  geom_point()
ggplot(flux.format[abs(L) < 25000 & L != -999], aes(x = ts, y = L)) +
  geom_point()
# bad L values at end of 2020
flux.format[ts >= as_date('2020-02-03'), L := -999]

# sigma_v over time
ggplot(flux.format[sigma_v != -999], aes(x = ts, y = sigma_v)) +
  geom_point()
# bad sigma_v values at end of 2020
flux.format[ts >= as_date('2020-02-03'), sigma_v := -999]

# u* over time
ggplot(flux.format[u_star != -999], aes(x = ts, y = u_star)) +
  geom_point()
# bad u_star values at end of 2020
flux.format[ts >= as_date('2020-02-03'), u_star := -999]

ggplot(flux.format[wind_dir != -999], aes(x = ts, y = wind_dir)) +
  geom_point()

# wind speed by monin obukhov length
ggplot(flux.format[u_mean != -999 & L != -999], aes(x = u_mean, y = L, color = as.factor(yyyy))) +
  geom_point()
# wind speed by sigma_v
ggplot(flux.format[u_mean != -999 & sigma_v != -999], aes(x = u_mean, y = sigma_v, color = as.factor(yyyy))) +
  geom_point()
# wind speed by sigma_v
ggplot(flux.format[u_mean != -999 & u_star != -999], aes(x = u_mean, y = u_star, color = as.factor(yyyy))) +
  geom_point()
# wind speed by wind direction
ggplot(flux.format[u_mean != -999 & wind_dir != -999], aes(x = u_mean, y = wind_dir, color = as.factor(yyyy))) +
  geom_point()


### Final Formatting
final <- flux.format[,
                     .(yyyy, mm, day, HH, MM, zm, d, z0, u_mean, L, sigma_v,
                       u_star, wind_dir)]

final.17 <- final[yyyy == 2017 & mm >= 5 | yyyy == 2018 & mm < 5]
final.18 <- final[yyyy == 2018 & mm >= 5 | yyyy == 2019 & mm < 5]
final.19 <- final[yyyy == 2019 & mm >= 5 | yyyy == 2020 & mm < 5]

### Save output
# write.csv(final.17,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint/ffp_ready_2017.csv',
#           row.names = FALSE)
#########################################################################################

### Run FFP #############################################################################
### set up data needed for ffp formatting
# point for ec tower location UTM zone 6N
ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
ec_sf <- st_sf(geometry = ec, crs = 32606)
rm(ec)

# raster template
karst_1 <- brick(stack("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_raster_final_1.tif",
                       "/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_raster_final_2.tif",
                       "/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_raster_final_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')
mean.karst <- calc(karst_1, mean, na.rm = TRUE)

filenames <- list.files('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output',
                        full.names = TRUE,
                        pattern = '^mtopo15.+9km')

mtopo15 <- brick(stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_1\\.tif$'))],
                     filenames[which(str_detect(filenames, pattern = 'mtopo15.+_2\\.tif$'))],
                     filenames[which(str_detect(filenames, pattern = 'mtopo15.+_3\\.tif$'))]))
mean.mtopo <- calc(mtopo15, mean, na.rm = TRUE)
karst.mtopo.brick <- brick(mean.karst, mean.mtopo)

rm(filenames, karst_1, mena.karst, mtopo15, mean.mtopo)

# # This section was used to help build/test the calc.ffp.loop function, below
# # test.data <- final.17[1,]
# test.data <- final.17[4300,]
# test.data <- final.17[14985,]
# latitude <- 63.879933
# # longitude <- -149.252103
# # extract tower location information as numeric 
# tower.x.utm <- st_coordinates(ec_sf)[,1]
# tower.y.utm <- st_coordinates(ec_sf)[,2]
# 
# # create raster template onto which output should be projected
# crop.extent <- extent(tower.x.utm - 500,
#                       tower.x.utm + 500,
#                       tower.y.utm - 500,
#                       tower.y.utm + 500)
# raster.brick.crop <- crop(karst.mtopo.brick, crop.extent)
# 
# ffp <- calc_footprint_FFP(lat = latitude,
#                           zm = as.numeric(test.data[,"zm"]),
#                           z0 = as.numeric(test.data[,"z0"]),
#                           #h = 1000,
#                           umean = as.numeric(test.data[,"u_mean"]),
#                           ol = as.numeric(test.data[, "L"]),
#                           sigmav = as.numeric(test.data[, "sigma_v"]),
#                           ustar = as.numeric(test.data[, "u_star"]),
#                           wind_dir = as.numeric(test.data[, "wind_dir"]),
#                           r = seq(10, 90, 10))
# 
# # format as data frame (this allows easy alignment of points to ec tower)
# matrices <- c('x_2d', 'y_2d', 'f_2d')
# ffp.sf <- map_dfc(matrices,
#                    ~ ffp[[.x]] %>% # select x, y, or f matrix
#                      as.data.frame() %>%
#                      pivot_longer(cols = 1:1001, names_to = 'remove', values_to = .x) %>% # make it into a 'tidy' data frame (long format with single column for x, y, or f)
#                      select(-matches('remove'))) %>% # we don't need the names column which just contains original column information
#   mutate(x_geo = x_2d + 389389.25, # add ec tower latitude location to x distance values
#          y_geo = y_2d + 7085586.3, # add ec tower longitude location to y distance values
#          f_2d_scaled = f_2d/sum(f_2d, na.rm = TRUE)) %>% # scale the flux measurement for each cell to a percentage of total flux
#   st_as_sf(coords = c('x_geo', 'y_geo'), crs = 32606, remove = FALSE)
# 
# # ggplot(test.df, aes(color = f_2d_scaled)) +
# #   geom_sf() +
# #   geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'red')
# 
# # use rasterize to convert points to raster aligned with thermokarst model
# ffp.raster <- rasterize(as(ffp.sf, 'Spatial'),
#                          raster.brick.crop,
#                          field = 'f_2d_scaled',
#                          fun = 'sum',
#                          na.rm = TRUE)
# 
# # plot(test.raster)
# 
# # reformat contours as sf
# ffp.sa.polygon <- list()
# for (i in 1:length(ffp$xr)) {
#   xr <- ffp$xr[[i]] + 389389.25
#   yr <- ffp$yr[[i]] + 7085586.3
#   ffp.sa.polygon[[i]] <- st_polygon(list(matrix(c(xr, yr), ncol = 2, byrow = FALSE)))
# }
# ffp.sa.sfc <- st_as_sfc(ffp.sa.polygon, crs = 32606)
# ffp.sa.sf <- st_sf(geometry = ffp.sa.sfc) %>%
#   mutate(interval = seq(10, 90, 10))
# 
# # mask to 90% area
# ffp.raster.mask <- mask(ffp.raster, ffp.sa.sf)
# raster.brick.mask <- mask(raster.brick.crop, ffp.sa.sf)
# # plot(ffp.raster.mask)
# # check flux sum
# output <- vector()
# for (i in 1:nrow(ffp.sa.sf)) {
#   mask <- slice(ffp.sa.sf, i)
#   output[i] <- cellStats(mask(ffp.raster.mask, mask), sum, na.rm = TRUE)
# }
# 
# # plot
# ggplot(as.data.frame(ffp.raster.mask, xy = TRUE), aes(x = x, y = y, fill = sqrt(layer))) +
#   geom_tile() +
#   # coord_fixed() +
#   geom_sf(data = ffp.sa.sf, inherit.aes = FALSE, color = 'red', fill = 'transparent') +
#   scale_fill_viridis(na.value = 'transparent')
# 
# ggplot(test.df, aes(x = f)) +
#   geom_bar()


# To use this function, you have to run the function in calc_footprint_FFP.R
# takes about 20 seconds per iteration through raster mask
# which means about 100 hours to run for an entire year on my computer...
# can either try to mask before converting to raster and parallelize
# or run on monsoon (or both)

calc.ffp.loop <- function(df, tower.loc, raster.brick, contour.range) {
  
  # extract tower location information as numeric 
  tower.x.utm <- st_coordinates(tower.loc)[,1]
  tower.y.utm <- st_coordinates(tower.loc)[,2]
  latitude.wgs84 <- st_coordinates(st_transform(tower.loc, crs = 4326))[,1]
  
  # create raster template onto which output should be projected
  crop.extent <- extent(tower.x.utm - 500,
                        tower.x.utm + 500,
                        tower.y.utm - 500,
                        tower.y.utm + 500)
  raster.brick.crop <- crop(raster.brick, crop.extent)
  
  # create vector of ffp output names
  matrices <- c('x_2d', 'y_2d', 'f_2d')
  
  # create output objects
  karst.pc <- vector()
  sd.mtopo <- vector()
  
  # run ffp model on each row of data in the input (each row is a half hour period)
  for (i in 1:nrow(df)) { 
    
    # run ffp model
    ffp <- calc_footprint_FFP(lat = latitude.wgs84,
                              zm = as.numeric(df[i, "zm"]),
                              z0 = as.numeric(df[i, "z0"]),
                              umean = as.numeric(df[i, "u_mean"]),
                              ol = as.numeric(df[i, "L"]),
                              sigmav = as.numeric(df[i, "sigma_v"]),
                              ustar = as.numeric(df[i, "u_star"]),
                              wind_dir = as.numeric(df[i, "wind_dir"]),
                              r = contour.range)
    
    if (ffp$flag_err == 0) {
      
      # format as data frame (this allows easy alignment of points to ec tower)
      ffp.sf <- map_dfc(matrices,
                        ~ ffp[[.x]] %>% # select x, y, or f matrix
                          as.data.frame() %>%
                          pivot_longer(cols = 1:ncol(.),
                                       names_to = 'remove',
                                       values_to = .x) %>% # make it into a 'tidy' data frame (long format with single column for x, y, or f)
                          select(-matches('remove'))) %>% # we don't need the names column which just contains original column information
        mutate(x_geo = x_2d + tower.x.utm, # add ec tower latitude location to x distance values
               y_geo = y_2d + tower.y.utm, # add ec tower longitude location to y distance values
               f_2d_scaled = f_2d/sum(f_2d, na.rm = TRUE)) %>% # scale the flux measurement for each cell to a percentage of total flux
        st_as_sf(coords = c('x_geo', 'y_geo'), crs = 32606, remove = FALSE)
      
      # reformat contours as sf
      ffp.sa.polygon <- list()
      for (j in 1:length(ffp$xr)) {
        # extract vectors with contour location information
        xr <- ffp$xr[[j]] + tower.x.utm
        yr <- ffp$yr[[j]] + tower.y.utm
        # convert contour information to an sf polygon object
        ffp.sa.polygon[[j]] <- st_polygon(list(matrix(c(xr, yr), ncol = 2, byrow = FALSE)))
      }
      ffp.sa.sfc <- st_as_sfc(ffp.sa.polygon, crs = 32606)
      ffp.sa.sf <- st_sf(geometry = ffp.sa.sfc) %>%
        mutate(interval = contour.range)
      
      # use rasterize to convert points to a raster aligned with thermokarst model
      ffp.raster <- rasterize(as(ffp.sf, 'Spatial'),
                              raster.brick.crop,
                              field = 'f_2d_scaled',
                              fun = 'sum',
                              na.rm = TRUE)
      
      # mask to maximum requested contour polygon
      ffp.raster.mask <- mask(ffp.raster, ffp.sa.sf)
      raster.brick.mask <- mask(raster.brick.crop, ffp.sa.sf)
      # plot(ffp.raster.mask)
      
      # calculate thermokarst percent cover
      karst.pc[i] <- cellStats(raster.brick.mask[[1]]*ffp.raster.mask, sum, na.rm = TRUE)/cellStats(ffp.raster.mask, sum, na.rm = TRUE)
      
      # calculate mean microtopography
      sd.mtopo[i] <- sqrt(cellStats(ffp.raster.mask*(raster.brick.mask[[2]] - cellStats(raster.brick.mask[[2]], mean, na.rm = TRUE))^2, sum, na.rm = TRUE)/((length(ffp.raster.mask[!is.na(ffp.raster.mask)]) - 1)/length(ffp.raster.mask[!is.na(ffp.raster.mask)])*cellStats(ffp.raster.mask, sum, na.rm = TRUE)))
      
    } else { # if the model didn't run for the current time period
      
      # fill in karst.pc and sd.mtopo with NA
      karst.pc[i] <- NA
      sd.mtopo[i] <- NA
      
    }
    
  }
  
  # output[[2]] <- df %>%
  #   mutate(karst.pc = c(karst.pc, rep(NA, nrow(df) - length(karst.pc))),
  #          sd.mtopo = c(sd.mtopo, rep(NA, nrow(df) - length(sd.mtopo))))
  
  output <- df %>%
    mutate(karst.pc = karst.pc,
           sd.mtopo = sd.mtopo)
  
  
  return(output)
  
}

# # test the function on a small set of data
# start.time <- Sys.time()
# test <- calc.ffp.loop(final.17[14980:14990], ec_sf, karst.mtopo.brick, seq(10, 90, 10))
# end.time <- Sys.time()
# difftime(start.time, end.time)

# # run the function on 2017
# calculate ffp
ffp.2017 <- calc.ffp.loop(final.17.list[[i]], ec_sf, karst.mtopo.brick, seq(10, 90, 10))

write.csv(ffp.2017,
          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint/ffp_2017.csv',
          row.names = FALSE)
#########################################################################################