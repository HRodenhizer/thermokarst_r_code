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
Tower18.19[, `:=` (u_var = NULL,  v_var = NULL, w_var = NULL)]
load('~/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_CO2&CH4.Rdata')

flux <- rbind(Tower17.18[ts >= as_date('2017-05-01') & ts < as_date('2018-05-01')],
              Tower18.19[ts >= as_date('2018-05-01') & ts < as_date('2019-05-01')],
              Tower19.20[ts >= as_date('2019-05-01') & ts < as_date('2020-05-01')])
test <- flux[, .N, by = 'ts']
# View(test[N > 1])

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

# for some reason there are a bunch of duplicate timestamps, some with NAs, some
# the same data in both rows
test <- eddy[, .N, by = 'ts']
# View(test[N > 1])
duplicates <- eddy[eddy$ts %in% test[N > 1]$ts,]
# View(duplicates[order(ts)])
eddy <- eddy[, lapply(.SD, max, na.rm = TRUE) , by = 'ts']
eddy <- eddy[ts >= as_date('2017-05-01') & ts < as_date('2020-05-01')]
eddy[is.na(L), L := -999]
eddy[ L == -Inf, L := -999]
eddy[is.na(sigma_v), sigma_v := -999]
eddy[ sigma_v == -Inf, sigma_v := -999]
#########################################################################################

### Format for FFP ######################################################################
# Select desired timeframe and variables
# Get all of the needed variables from the ameriflux file except V_SIGMA, which we
# haven't been uploading to Ameriflux
subset <- flux[ts >= as_date('2017-05-01') & ts < as_date('2020-05-01'),
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

# test for duplicates again
test <- flux.format[, .N, by = ts]
# View(test[N > 1])

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
# write.csv(final.18,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint/ffp_ready_2018.csv',
#           row.names = FALSE)
# write.csv(final.19,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint/ffp_ready_2019.csv',
#           row.names = FALSE)
#########################################################################################

### FFP Iteration Function ##############################################################
### set up data needed for ffp formatting
final.17 <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint/ffp_ready_2017.csv',
                     na.strings = '-999')
final.17 <- data.table(final.17)
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

rm(filenames, karst_1, mean.karst, mtopo15, mean.mtopo)

# load ffp functions
source('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_r_code/flux_footprint_estimation/calc_footprint_FFP_EML.R')
source('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_r_code/flux_footprint_estimation/flux_footprint_iteration_function.R')

# # test the function on a small set of data
# start.time <- Sys.time()
# test <- calc.ffp.loop(final.17[14980:14990], ec_sf, karst.mtopo.brick, seq(10, 90, 10))
# end.time <- Sys.time()
# difftime(start.time, end.time)
#########################################################################################

### Test parallelization on my computer #################################################
# Define how many cores you want to use
UseCores <- 5

# split up data
chunk.length <- 2
final.17.list <- list()
for (i in 1:UseCores) {
  start.row <- (i - 1)*chunk.length
  if (i < UseCores) {
    end.row <- i*chunk.length
  } else {
    end.row <- UseCores*chunk.length
  }
  final.17.list[[i]] <- final.17[start.row:end.row]
}

cl <- makeCluster(UseCores, outfile = "/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/test/log.txt")
registerDoParallel(cl)
ffp.2017 <- foreach(i=1:UseCores, .combine = rbind) %dopar% {
  # load libraries
  library(sf)
  library(raster)
  library(tidyverse)
  
  # calculate ffp
  calc.ffp.loop(final.17.list[[i]], ec_sf, karst.mtopo.brick, seq(10, 90, 10))
  
}
stopCluster(cl)

# filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/test/',
#                         full.names = TRUE)
# ffp.2017 <- map_dfr(filenames,
#                     ~ read.csv(.x))

# write.csv(ffp.2017,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint/ffp_2017.csv',
#           row.names = FALSE)
#########################################################################################