################################################################################
###                   Test Thermokarst Detection Function                    ###
###                    Code by Heidi G Rodenhizer 4/2021                     ###
################################################################################

### To Do:
# actually figure out how to do tests for real

### Libraries
library(raster)
library(doParallel)

### Data
dtm <- crop(raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM/NEON_DTM_2017.tif'),
            y = extent(matrix(c(390600, 390850, 7085600, 7085850), ncol = 2, byrow = TRUE)))
dtm2 <- brick(crop(raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM/NEON_DTM_2017.tif'),
             y = extent(matrix(c(390600, 390850, 7085600, 7085850), ncol = 2, byrow = TRUE))),
             crop(raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM/NEON_DTM_2018.tif'),
                  y = extent(matrix(c(390600, 390850, 7085600, 7085850), ncol = 2, byrow = TRUE))))
plot(dtm)

### load function
source('thermokarst_features/thermokarst_detection_function.R')

### Tests
# 1 elevation layer, 1 core
test <- tk.detect(dtm, radii = 5, cutoff = 0, n.cores = 1)
test <- tk.detect(dtm, radii = c(5, 10), cutoff = 0, n.cores = 1)
# 2 elevation layers, 1 core
test <- tk.detect(dtm2, radii = c(5, 10), cutoff = 0, n.cores = 1)
# 1 elevation layer, >1 cores
test <- tk.detect(dtm, radii = c(5, 10), cutoff = 0, n.cores = 2)
# 2 elevation layers, >1 cores
test <- tk.detect(dtm2, radii = c(5, 10), cutoff = 0, n.cores = 2)
# 2 elevation layers, too many cores - should fail
test <- tk.detect(dtm2, radii = c(5, 10), cutoff = 0, n.cores = 13)
# wrong elevation data type - should fail
df <- data.frame(x = c(1, 2),
                 y = c(3, 4))
test <- tk.detect(df, radii = c(5, 10), cutoff = 0, n.cores = 1)
# wrong radius type - should fail
test <- tk.detect(dtm, radii = c('5', '10'), cutoff = 0, n.cores = 1)
# wrong cutoff type - should fail
test <- tk.detect(dtm, radii = c(5, 10), cutoff = '0', n.cores = 1)
# test defaults
test <- tk.detect(dtm)

# some plots
plot(test$elev.crop)
plot(test$med.elev$med.elev.5)
plot(test$med.elev$med.elev.10)
plot(test$microtopography$microtopography.5)
plot(test$microtopography$microtopography.10)
plot(test$thermokarst$thermokarst.5)
plot(test$thermokarst$thermokarst.10)
