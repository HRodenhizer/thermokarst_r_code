###########################################################################################################
###                               Pan-Sharpen Digital Globe Imagery                                     ###
###                                        Code by HGR 3/20                                             ###
###########################################################################################################

### Load Packages #########################################################################################
library(raster)
library(RStoolbox)
###########################################################################################################

### Load Data #############################################################################################
filenames.mul <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/DigitalGlobe/EightmileLake_012293551_10_0/012293551010_01_003/012293551010_01/012293551010_01_P001_MUL/',
                            pattern = '.+(R02C2|R02C3|R03C2|R03C3).+TIF$',
                            full.names = TRUE)
filenames.pan <- list.files('C:/Users/Heidi Rodenhizer/Documents/School/NAU/Schuur Lab/Remote Sensing/DigitalGlobe/EightmileLake_012293551_10_0/012293551010_01_003/012293551010_01/012293551010_01_P001_PAN/',
                            pattern = '.+(R02C2|R02C3|R03C2|R03C3).+TIF$',
                            full.names = TRUE)

