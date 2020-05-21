########################################################################################################################
###                           Subsidence/Thermokarst Analysis from NEON Data                                         ###
###                                         Code by HGR 5/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(tidyverse)
library(sf)
########################################################################################################################

### Load Data ##########################################################################################################
sub <- brick(stack('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2018.tif',
                   'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2019.tif'))

karst <- brick(stack("Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_3_filter_1.tif",
                     "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_3_filter_2.tif",
                     "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/karst_combined_3_filter_3.tif"))
########################################################################################################################

### Calculate Thermokarst Coverage #####################################################################################
# calculate area (each cell is 1 m^2, so just getting the number of cells is the area in m^2)
karst_area <- karst %>%
  as.data.frame() %>%
  rename(class.2017 = 1, class.2018 = 2, class.2019 = 3) %>%
  gather(key = 'year', value = 'thermokarst', 1:3) %>%
  mutate(year = as.numeric(year, str_sub(7)),
         thermokarst = ifelse(is.na(thermokarst),
                              'undisturbed',
                              'thermokarst')) %>%
  group_by(year, thermokarst) %>%
  summarise(thermokarst = n())
karst_cover <- karst_area[2,1]/sum(karst_area[,1])
########################################################################################################################

### Mixed Effects Model of Subsidence by Thermokarst Class Over Time ###################################################
# take stratified random sample of cells
set.seed(333)
samples <- st_as_sf(sampleStratified(karst[[1]], size = 1000, xy = TRUE, sp = TRUE)) %>%
  select(-4)
ggplot(samples, aes(x = x, y = y)) +
  geom_point() +
  coord_fixed()

# extract values from subsidence brick
sub_extract <- st_as_sf(raster::extract(sub, as(samples, 'Spatial'), layer = 1, nl = 2, sp = TRUE), cellnumbers = TRUE) %>%
  rename(sub.2018 = 5,
         sub.2019 = 6) %>%
  mutate(sub.2017 = 0) %>%
  gather(key = year, value = sub, sub.2018:sub.2017) %>%
  mutate(year = as.numeric(str_sub(year, 5))) %>%
  arrange(year)

# extract values from thermokarst classification brick
karst_extract <- st_as_sf(raster::extract(karst, as(samples, 'Spatial'), layer = 1, nl = 2, sp = TRUE), cellnumbers = TRUE) %>%
  rename(tk.2017 = 5,
         tk.2018 = 6,
         tk.2019 = 7) %>%
  gather(key = year, value = thermokarst, tk.2017:tk.2019) %>%
  mutate(year = as.numeric(str_sub(year, 4)))

# join subsidence and thermokarst extract dataframes
# this won't work, because I also need to join by year, I think
# will a row bind work?
sub_karst <- sub_extract %>%
  st_join(karst_extract)


