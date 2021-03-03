#########################################################################################
###                         Format Flux Data for FFP Online Tool                      ###
###                                Code by HGR 2/2021                                 ###
#########################################################################################

### Libraries ###########################################################################
library(data.table)
library(lubridate)
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
flux.format[, zm := 3.5]
flux.format[, d := 0.2]
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
