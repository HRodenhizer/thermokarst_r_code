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
karst_extract <- st_read("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/thermokarst_extract_18.shp") %>%
  st_set_crs(32606)
validated_samples <- st_read("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/samples_stratified_100_scrambled.shp") %>%
  st_set_crs(32606) %>%
  mutate(validation = as.numeric(as.character(validation)),
         confidence = as.numeric(as.character(confidence)))
########################################################################################################################

### Join Validated Points with Extracted Values ########################################################################
validation_df <- full_join(st_drop_geometry(validated_samples),
                           select(st_drop_geometry(karst_extract), -validation),
                           by = c('cell', 'x', 'y'))

########################################################################################################################

### Compare Performance of Different Thermokarst Classifications #######################################################
# check number of thermokarst vs. non thermokarst points in validation (should be close to 100 for each = near 0.5 mean)
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
# validation class is rows, and classification class is columns
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

# tk_comb_1 metrics
# create a neat confusion matrix
tk_comb_1_performance <- table(validation_df$validation, validation_df$tk_comb_1)
tk_confusion_matrix <- as.data.frame(tk_comb_1_performance) %>%
  mutate(Observed = ifelse(Var1 == 0,
                           'Non-Thermokarst',
                           'Thermokarst'),
         Simulated = ifelse(Var2 == 0,
                            'Non-Thermokarst',
                            'Thermokarst')) %>%
  select(-c(Var1, Var2)) %>%
  pivot_wider(names_from = Observed,
              values_from = Freq) %>%
  mutate(Total = `Non-Thermokarst` + Thermokarst)
tk_confusion_matrix <- tk_confusion_matrix %>%  
  rbind.data.frame(data.frame(Simulated = 'Total',
                              `Non-Thermokarst` = sum(tk_confusion_matrix$`Non-Thermokarst`),
                              Thermokarst = sum(tk_confusion_matrix$Thermokarst),
                              Total = sum(tk_confusion_matrix$`Non-Thermokarst`, tk_confusion_matrix$Thermokarst)) %>%
                     rename(`Non-Thermokarst` = Non.Thermokarst))
# write.csv(tk_confusion_matrix,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/confusion_matrix.csv',
#           row.names = FALSE)
# % of simulated non-thermokarst which were correctly identified
user.acc.non.tk <- tk_confusion_matrix$`Non-Thermokarst`[1]/tk_confusion_matrix$Total[1]
# % of simulated thermokarst which were correctly identified
user.acc.tk <- tk_confusion_matrix$Thermokarst[2]/tk_confusion_matrix$Total[2]
# % of observed non-thermokarst which were correctly identified
prod.acc.non.tk <- tk_confusion_matrix$`Non-Thermokarst`[1]/tk_confusion_matrix$`Non-Thermokarst`[3]
# % of observed thermokarst which were correctly identified
prod.acc.tk <- tk_confusion_matrix$Thermokarst[2]/tk_confusion_matrix$Thermokarst[3]
# overall accurracy
overall.acc <- (tk_confusion_matrix$`Non-Thermokarst`[1] + tk_confusion_matrix$Thermokarst[2])/tk_confusion_matrix$Total[3] 

# check out difference in number of false classifications by validation certainty
# my confidence in the thermokarst validation from imagery does impact how accurate each class was
# essentially the more uncertain I was in the validation, the lower the accuracy
# so at least some of the low accuracy level could be due to my inability to determine if features are thermokarst
# from imagery
accuracy_by_conf_1 <- mean(validation_df$validation[which(validation_df$confidence == 1)] == validation_df$tk_comb_3[which(validation_df$confidence == 1)])
accuracy_by_conf_2 <- mean(validation_df$validation[which(validation_df$confidence == 2)] == validation_df$tk_comb_3[which(validation_df$confidence == 2)])
accuracy_by_conf_3 <- mean(validation_df$validation[which(validation_df$confidence == 3)] == validation_df$tk_comb_3[which(validation_df$confidence == 3)])

accuracy_by_conf_1
accuracy_by_conf_2
accuracy_by_conf_3

performance_by_conf_1 <- table(validation_df$validation[which(validation_df$confidence == 1)],
                               validation_df$tk_comb_3[which(validation_df$confidence == 1)])/
  sum(table(validation_df$validation[which(validation_df$confidence == 1)],
            validation_df$tk_comb_3[which(validation_df$confidence == 1)])[1:2,1:2])
performance_by_conf_2 <- table(validation_df$validation[which(validation_df$confidence == 2)],
                               validation_df$tk_comb_3[which(validation_df$confidence == 2)])/
  sum(table(validation_df$validation[which(validation_df$confidence == 2)],
            validation_df$tk_comb_3[which(validation_df$confidence == 2)])[1:2,1:2])
performance_by_conf_3 <- table(validation_df$validation[which(validation_df$confidence == 3)],
                               validation_df$tk_comb_3[which(validation_df$confidence == 3)])/
  sum(table(validation_df$validation[which(validation_df$confidence == 3)],
            validation_df$tk_comb_3[which(validation_df$confidence == 3)])[1:2,1:2])

performance_by_conf_1
performance_by_conf_2
performance_by_conf_3
########################################################################################################################

