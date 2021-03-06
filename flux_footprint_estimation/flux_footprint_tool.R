#########################################################################################
###                          Run FFP on EML Tower 2017-2019                        ###
###                                Code by HGR 2/2021                                 ###
#########################################################################################

### Libraries ###########################################################################
library(sf)
library(data.table)
library(lubridate)
library(viridis)
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
# To use this function, you have to run the function in calc_footprint_FFP.R
test.data <- final.17[4300,]
latitude <- 63.879933
longitude <- -149.252103
test <- calc_footprint_FFP(lat = latitude,
                           zm = as.numeric(test.data[,"zm"]),
                           z0 = as.numeric(test.data[,"z0"]),
                           #h = 1000,
                           umean = as.numeric(test.data[,"u_mean"]),
                           ol = as.numeric(test.data[, "L"]),
                           sigmav = as.numeric(test.data[, "sigma_v"]),
                           ustar = as.numeric(test.data[, "u_star"]),
                           wind_dir = as.numeric(test.data[, "wind_dir"]),
                           r = seq(10, 90, 10))

# format as data frame to plot with ggplot
x <- test$x_2d %>%
  as.data.frame() %>%
  pivot_longer(cols = 1:1001, names_to = 'remove', values_to = 'x') %>%
  select(x) %>%
  data.table()
y <- test$y_2d %>%
  as.data.frame() %>%
  pivot_longer(cols = 1:1001, names_to = 'remove', values_to = 'y') %>%
  select(y) %>%
  data.table()
f <- test$f_2d %>%
  as.data.frame() %>%
  pivot_longer(cols = 1:1001, names_to = 'remove', values_to = 'f') %>%
  select(f) %>%
  data.table()
test.df <- cbind(x, y, f)

# reformat contours
xr <- test$xr[[1]]
yr <- test$yr[[1]]
matrix.10 <- matrix(c(xr, yr), ncol = 2, byrow = FALSE)
sfc.10 <- st_sfc(st_polygon(list(matrix.10)))
sf.10 <- st_sf(geometry = sfc.10)

r.sfc <- vector()
for (i in 1:length(test$xr)) {
  xr <- test$xr[[i]]
  yr <- test$yr[[i]]
  r.polygon[i] <- st_polygon(list(matrix(c(xr, yr), ncol = 2, byrow = FALSE)))
}
r.sfc <- st_as_sfc(r.polygon)
r.sf <- st_sf(geometry = test.sfc)

# sum of all weights - why not 1?
# sum changes for each time period, so I think it just needs to be scaled?
sum(test.df$f)

# plot
ggplot(filter(test.df, x < 0 & x > -275 & y < 100 & y > -50), aes(x = x, y = y, color = sqrt(f))) +
  geom_point() +
  # coord_fixed() +
  geom_sf(data = r.sf, inherit.aes = FALSE, color = 'red', fill = 'transparent') +
  scale_color_viridis()

ggplot(test.df, aes(x = f)) +
  geom_bar()
#########################################################################################