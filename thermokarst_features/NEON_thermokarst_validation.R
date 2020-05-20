########################################################################################################################
###                     Analyze Validation of Thermokarst Feature Outlines from NEON Data                            ###
###                                             Code by HGR 5/2020                                                   ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(sf)
library(tidyverse)
########################################################################################################################

### Load Data ##########################################################################################################
karst_extract <- st_read("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/thermokarst_extract_18.shp") %>%
  st_set_crs(32606)
validated_samples <- st_read("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/samples_stratified_100_scrambled.shp") %>%
  st_set_crs(32606)
########################################################################################################################

### Join Validated Points with Extracted Values ########################################################################
validation_df <- full_join(st_drop_geometry(validated_samples),
                           select(st_drop_geometry(karst_extract), -validation),
                           by = c('cell', 'x', 'y'))
########################################################################################################################

### Compare Performance of Different Thermokarst Classifications #######################################################
# check number of thermokarst vs. non thermokarst points in validation (should be close to 100 for each)
summary(validation_df$validation)
# check my confidence for each validation point
summary(validation_df$confidence)

# overall accuracy
# tk_comb_1 (15 + 25 + 35 m radii) and tk_comb_3 (25 + 35 m radii) tie
tk_15_accuracy <- mean(validation_df$validation == validation_df$tk_15)
tk_15_5_accuracy <- mean(validation_df$validation == validation_df$tk_15_5)
tk_25_accuracy <- mean(validation_df$validation == validation_df$tk_25)
tk_35_accuracy <- mean(validation_df$validation == validation_df$tk_35)
tk_comb_1_accuracy <- mean(validation_df$validation == validation_df$tk_comb_1)
tk_comb_2_accuracy <- mean(validation_df$validation == validation_df$tk_comb_2)
tk_comb_3_accuracy <- mean(validation_df$validation == validation_df$tk_comb_3)
tk_comb_4_accuracy <- mean(validation_df$validation == validation_df$tk_comb_4)

tk_15_accuracy
tk_15_5_accuracy
tk_25_accuracy
tk_35_accuracy
tk_comb_1_accuracy
tk_comb_2_accuracy
tk_comb_3_accuracy
tk_comb_4_accuracy

# create confusion matrices
# tk_comb_1 has slightly higher correct classification as thermokarst (is less conservative)
# tk_comb_3 has slightly higher correct classification as non-thermokarst (is more conservative)
tk_15_performance <- table(validation_df$validation, validation_df$tk_15)
tk_15_5_performance <- table(validation_df$validation, validation_df$tk_15_5)
tk_25_performance <- table(validation_df$validation, validation_df$tk_25)
tk_35_performance <- table(validation_df$validation, validation_df$tk_35)
tk_comb_1_performance <- table(validation_df$validation, validation_df$tk_comb_1)
tk_comb_2_performance <- table(validation_df$validation, validation_df$tk_comb_2)
tk_comb_3_performance <- table(validation_df$validation, validation_df$tk_comb_3)
tk_comb_4_performance <- table(validation_df$validation, validation_df$tk_comb_4)

tk_15_performance
tk_15_5_performance
tk_25_performance
tk_35_performance
tk_comb_1_performance
tk_comb_2_performance
tk_comb_3_performance
tk_comb_4_performance

# check out difference in number of false classifications by validation certainty
# basically, my confidence means very little
# there is lower accuracy at confidence level of 2, which I used for points that were at the edge of features
# which indicates that the edge of features are hard to identify (either by me or the model)
# slight orthorectification issues between wv2 imagery and neon lidar/neon imagery and neon lidar could contribute to this
accuracy_by_conf_1 <- mean(validation_df$validation[which(validation_df$confidence == 1)] == validation_df$tk_comb_3[which(validation_df$confidence == 1)])
accuracy_by_conf_2 <- mean(validation_df$validation[which(validation_df$confidence == 2)] == validation_df$tk_comb_3[which(validation_df$confidence == 2)])
accuracy_by_conf_3 <- mean(validation_df$validation[which(validation_df$confidence == 3)] == validation_df$tk_comb_3[which(validation_df$confidence == 3)])

accuracy_by_conf_1
accuracy_by_conf_2
accuracy_by_conf_3

performance_by_conf_1 <- table(validation_df$validation[which(validation_df$confidence == 1)], validation_df$tk_comb_3[which(validation_df$confidence == 1)])
performance_by_conf_2 <- table(validation_df$validation[which(validation_df$confidence == 2)], validation_df$tk_comb_3[which(validation_df$confidence == 2)])
performance_by_conf_3 <- table(validation_df$validation[which(validation_df$confidence == 3)], validation_df$tk_comb_3[which(validation_df$confidence == 3)])

performance_by_conf_1
performance_by_conf_2
performance_by_conf_3
########################################################################################################################

