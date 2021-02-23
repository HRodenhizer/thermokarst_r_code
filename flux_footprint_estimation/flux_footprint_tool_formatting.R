#########################################################################################
###                         Format Flux Data for FFP Online Tool                      ###
###                                Code by HGR 2/2021                                 ###
#########################################################################################

### Libraries ###########################################################################
library(data.table)
#########################################################################################

### Load data ###########################################################################
# Use Ameriflux data
ameriflux <- fread('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/Ameriflux/AMF_US-EML_BASE_HH_3-5.csv')
#########################################################################################

### Format Ameriflux for FFP ############################################################
# Select desired timeframe and variables
# We apparently haven't been uploading a V_SIGMA variable to ameriflux, which is one of
# needed columns. Need to find it from the eddypro output, I guess.
subset <-ameriflux[TIMESTAMP_START >= 201705010000 & TIMESTAMP_START < 202005010000,
                   .(TIMESTAMP_START, WS, MO_LENGTH, USTAR, WD)]


