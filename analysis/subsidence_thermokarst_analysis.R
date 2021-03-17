########################################################################################################################
###                           Subsidence/Thermokarst Analysis from NEON Data                                         ###
###                                         Code by HGR 5/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(lubridate)
library(raster)
library(sf)
library(rgdal)
library(readxl)
library(lme4)
# library(heavy)
library(MuMIn)
library(emmeans)
library(pbkrtest)
library(viridis)
library(lwgeom)
library(mgcv)
library(doParallel)
library(mapview)
library(ggthemes)
library(ggpubr)
library(gridExtra)
library(tidyverse)
########################################################################################################################

### Load Data ##########################################################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All',
                        full.names = TRUE,
                        pattern = '.tif$')
crop_extent <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
elev <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
             crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
             crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
sub <- raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/lidar_subsidence/gliht_sub_14_18.tif')
karst_1 <- brick(stack("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_raster_final_1.tif",
                       "/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_raster_final_2.tif",
                       "/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_raster_final_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')

eml_wtrshd <- st_read("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/eml_bnd/boundry_poly3.shp")

filenames <- list.files('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output',
                        full.names = TRUE,
                        pattern = 'shp$')

karst_1_poly <- map(filenames[which(str_detect(filenames, pattern = 'karst_combined_1_poly_fill_final_\\d'))],
                    ~ st_read(.x))
names(karst_1_poly) <- c(2017, 2018, 2019)

filenames <- list.files('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output',
                        full.names = TRUE,
                        pattern = 'mtopo.+9km')

mtopo <- list(stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_1\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo25.+_1\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo35.+_1\\.tif$'))]),
              stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_2\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo25.+_2\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo35.+_2\\.tif$'))]),
              stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_3\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo25.+_3\\.tif$'))],
                    filenames[which(str_detect(filenames, pattern = 'mtopo35.+_3\\.tif$'))]))
rm(filenames)
########################################################################################################################


### Image-Wide Analysis
### Calculate Thermokarst Coverage #####################################################################################
# # calculate area of entire image (each cell is 1 m^2, so just getting the number of cells is the area in m^2)
# karst_area <- karst_1 %>%
#   as.data.frame() %>%
#   rename(class.2017 = 1, class.2018 = 2, class.2019 = 3) %>%
#   gather(key = 'year', value = 'thermokarst', 1:3) %>%
#   mutate(year = as.numeric(str_sub(year, 7)),
#          thermokarst = ifelse(thermokarst == 0,
#                               'undisturbed',
#                               ifelse(thermokarst == 1,
#                                      'thermokarst',
#                                      NA))) %>%
#   group_by(year, thermokarst) %>%
#   summarise(n = n())
# 
# # summarize
# karst_cover <- karst_area %>%
#   filter(!is.na(thermokarst)) %>%
#   spread(key = 'thermokarst', value = 'n') %>%
#   mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))
# write.csv(karst_cover, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/thermokarst_area.csv')
########################################################################################################################

### Summary Statistics From Polygons ###################################################################################
# Depth of thermokarst features is calculated in polygon_summary_statistics.R
# Read in all of the files from polygon_summary_statistics.R and reformat
filenames <- list.files('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/polygon_summary_split/redo_fill',
                        full.names = TRUE)

radius <- c(15, 25, 35)
karst_1_depth <- list()
for (i in 1:3) {
  karst_1_depth[[i]] <- list()
  for (j in 1:3) {
    pattern <- paste('karst_1_depth_', i, '_', j, sep = '')
    karst_1_depth[[i]][[j]] <- map_df(filenames[which(str_detect(filenames, pattern = pattern))],
                                      ~ read.csv(.x) %>%
                                        rename(depth = 3) %>%
                                        mutate(year = i + 2016,
                                               radius = radius[j]))
  }
}

karst_1_depth_join <- map_df(karst_1_depth,
                          ~ map_df(.x,
                                   ~ .x))
# write.csv(karst_1_depth_join, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_depth_join.csv', row.names = FALSE)
# rm(karst_1_depth)
# 
# plot(karst_1_depth_join)
# karst_1_depth_join <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_depth_join.csv')


### Old Note ###
# # karst polygons used to extract stats were not dissolved properly
# # need to figure out the actual polygon ids for summarizing

# I am running reassign_polygon_ids.sh on monsoon directly.
# load file with correct polygon ids
# karst_1_depth_join <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/code/analysis/old/karst_1_depth_join_new_ids.csv')
###

# # filter out depth values that are outside 3 std devs, unless all values are outside of 3 std devs
# # then filter by minimum depth within one cell and year (across radii) to only select the cell value corresponding to the best radius
# # somehow there are repeats of certain cells, or cells that don't actually exist which need to be filtered
# # this can be done by filtering out na values in depth
karst_1_depth_filter <- karst_1_depth_join %>%
  filter(!is.na(depth)) %>% # not sure how these are showing up, but if they don't have a cell number then it's not real data
  mutate(overall.mean.depth = mean(depth, na.rm = TRUE),
         overall.sd.depth = sd(depth, na.rm = TRUE),
         sd.3.upr = overall.mean.depth + 3*overall.sd.depth,
         sd.3.lwr = overall.mean.depth - 3*overall.sd.depth) %>%
  group_by(cell) %>%
  mutate(depth.clean = ifelse(all(depth > sd.3.upr) |
                                all(depth < sd.3.lwr) |
                                depth <= sd.3.upr & depth >= sd.3.lwr,
                              depth,
                              NA)) %>%
  group_by(year, ID, cell) %>%
  summarize(depth = min(depth, na.rm = TRUE),
            depth.clean = ifelse(all(is.na(depth.clean)),
                                 first(depth.clean),
                                 min(depth.clean, na.rm = TRUE)),
            radius = ifelse(all(is.na(depth)),
                            NA,
                            radius[which(depth == min(depth, na.rm = TRUE))])) %>%
  arrange(year, ID, cell)
# write.csv(karst_1_depth_filter, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_depth_clean.csv', row.names = FALSE)
# karst_1_depth_filter <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_depth_clean.csv')

# summarize stats by polygon and filter out polygons where all microtopography values are above 1
# this will remove single celled thermokarst features with positive values that were introduced
# during the raster hole filling step
karst_1_stats <-  karst_1_depth_filter %>%
  group_by(year, ID) %>%
  summarise(size = n(),
            min.depth = min(depth, na.rm = TRUE),
            mean.depth = mean(depth, na.rm = TRUE),
            median.depth = median(depth, na.rm = TRUE),
            max.depth = max(depth, na.rm = TRUE),
            sd.depth = sd(depth, na.rm = TRUE),
            se.depth = sd(depth, na.rm = TRUE)/sqrt(size),min.depth = min(depth, na.rm = TRUE),
            min.depth.clean = min(depth.clean, na.rm = TRUE),
            mean.depth.clean = mean(depth.clean, na.rm = TRUE),
            median.depth.clean = median(depth.clean, na.rm = TRUE),
            max.depth.clean = max(depth.clean, na.rm = TRUE),
            sd.depth.clean = sd(depth.clean, na.rm = TRUE),
            se.depth.clean = sd(depth.clean, na.rm = TRUE)/sqrt(size)) %>%
  filter(min.depth < 0)
# write.csv(karst_1_stats, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_stats_by_polygon.csv', row.names = FALSE)
karst_1_stats <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_stats_by_polygon.csv')

# join stats with geometries
karst_1_geom <- data.frame()
for (i in 1:3) {
  temp <- karst_1_poly[[i]] %>%
    mutate(year = i + 2016,
           ID = FID + 1)
  karst_1_geom <- rbind.data.frame(karst_1_geom, temp)
}

karst_1_stats_sf <- karst_1_stats %>%
  left_join(karst_1_geom, by = c('ID', 'year')) %>%
  st_as_sf(crs = 32606) %>%
  rename(min.d = min.depth,
         mean.d = mean.depth,
         med.d = median.depth,
         max.d = max.depth,
         sd.d = sd.depth,
         se.d = se.depth,
         min.d.c = min.depth.clean,
         mean.d.c = mean.depth.clean,
         med.d.c = median.depth.clean,
         max.d.c = max.depth.clean,
         sd.d.c = sd.depth.clean,
         se.d.c = se.depth.clean)
# st_write(karst_1_stats_sf, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_stats.shp')
# st_write(filter(karst_1_stats_sf, year == 2017), '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_poly_fill_final_1.shp')
# st_write(filter(karst_1_stats_sf, year == 2018), '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_poly_fill_final_2.shp')
# st_write(filter(karst_1_stats_sf, year == 2019), '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_poly_fill_final_3.shp')
karst_1_stats_sf <- read_sf('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_stats.shp') %>%
  rename(min.depth = min_d,
         mean.depth = mean_d,
         median.depth = med_d,
         max.depth = max_d,
         sd.depth = sd_d,
         se.depth = se_d,
         min.depth.clean = min_d_c,
         mean.depth.clean = mean_d_c,
         median.depth.clean = med_d_c,
         max.depth.clean = max_d_c,
         sd.depth.clean = sd_d_c,
         se.depth.clean= se_d_c)

# convert filtered shapefile to raster and save
# using st_rasterize requires stars which cannot be installed on monsoon due to
# dependency on lwgeom, which I have worked (unsuccesfully) with ITS to try and install
# karst_1_raster_filter <- st_rasterize(karst_1_stats_sf)
# karst_1_filter_list <- list(filter(karst_1_stats_sf, year == 2017),
#                             filter(karst_1_stats_sf, year == 2018),
#                             filter(karst_1_stats_sf, year == 2019))
# st_write(karst_1_filter_list[[1]] %>%
#            rename(min.d = min.depth,
#                   mean.d = mean.depth,
#                   med.d = median.depth,
#                   max.d = max.depth,
#                   sd.d = sd.depth,
#                   se.d = se.depth,
#                   min.d.c = min.depth.clean,
#                   mean.d.c = mean.depth.clean,
#                   med.d.c = median.depth.clean,
#                   max.d.c = max.depth.clean,
#                   sd.d.c = sd.depth.clean,
#                   se.d.c = se.depth.clean),
#          '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_1_final_poly_1.shp')
# st_write(karst_1_filter_list[[2]] %>%
#            rename(min.d = min.depth,
#                   mean.d = mean.depth,
#                   med.d = median.depth,
#                   max.d = max.depth,
#                   sd.d = sd.depth,
#                   se.d = se.depth,
#                   min.d.c = min.depth.clean,
#                   mean.d.c = mean.depth.clean,
#                   med.d.c = median.depth.clean,
#                   max.d.c = max.depth.clean,
#                   sd.d.c = sd.depth.clean,
#                   se.d.c = se.depth.clean),
#          '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_1_final_poly_2.shp')
# st_write(karst_1_filter_list[[3]] %>%
#            rename(min.d = min.depth,
#                   mean.d = mean.depth,
#                   med.d = median.depth,
#                   max.d = max.depth,
#                   sd.d = sd.depth,
#                   se.d = se.depth,
#                   min.d.c = min.depth.clean,
#                   mean.d.c = mean.depth.clean,
#                   med.d.c = median.depth.clean,
#                   max.d.c = max.depth.clean,
#                   sd.d.c = sd.depth.clean,
#                   se.d.c = se.depth.clean),
#          '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_1_final_poly_3.shp')
# karst_1_filter_list <- list(st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_poly_fill_final_1.shp'),
#                             st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_poly_fill_final_2.shp'),
#                             st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_poly_fill_final_3.shp'))
# karst_1_raster_filter <- brick(map(karst_1_filter_list,
#                                    ~ rasterize(as(.x, 'Spatial'), karst_1[[1]])))
# karst_1_raster_reclass <- reclassify(karst_1_raster_filter,
#                                      rcl = matrix(c(-1,Inf,1),
#                                                   ncol = 3,
#                                                   byrow = TRUE))
# writeRaster(karst_1_raster_filter, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_final_polygon_id.tif')
# writeRaster(karst_1_raster_reclass, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_final.tif')
# karst_1_raster_filter <- raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_final.tif')

summary(karst_1_stats_sf)

### some plots
# distribution of thermokarst sizes
ggplot(karst_1_stats_sf, aes(x = size)) +
  geom_histogram(bins = 100, color = 'gray35', fill = 'gray35') +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10')

# mean depth by size
ggplot(karst_1_stats_sf,
       aes(x = size, y = mean.depth, color = year),
       alpha = 0.5) +
  geom_point() +
  geom_smooth(method = 'gam',
              formula = y ~ s(x, bs = "cs"))

# max depth by size
ggplot(karst_1_stats_sf,
       aes(x = size, y = max.depth, color = year),
       alpha = 0.5) +
  geom_point() +
  geom_smooth(method = 'gam',
              formula = y ~ s(x, bs = "cs"))

# min depth by size
ggplot(karst_1_stats_sf,
       aes(x = size, y = min.depth, color = year),
       alpha = 0.5) +
  geom_point() +
  geom_smooth(method = 'gam',
              formula = y ~ s(x, bs = "cs"))

# make sure that there aren't any 1 cell thermokarst features with depths above 0
ggplot(filter(karst_1_stats_sf, size == 1), aes(y = mean.depth)) +
  geom_boxplot()

# ggplot(filter(karst_1_stats_sf, year == 2017 & size == 1)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(filter(karst_1_stats_sf, year == 2017 & size == 1 & mean.depth >= 0)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(filter(karst_1_stats_sf, year == 2017 & ID == 575)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(st_crop(filter(karst_1_stats_sf, year == 2017), y = c(xmin = 394273, ymin = 7080514, xmax = 394284, ymax = 7080525))) +
#   geom_sf(aes(geometry = geometry, color = as.factor(ID), fill = as.factor(ID)))
# 
# ggplot(filter(karst_1_stats_sf, year == 2017 & ID == 576)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(st_crop(filter(karst_1_stats_sf, year == 2017), y = c(xmin = 394075, ymin = 7080514, xmax = 394086, ymax = 7080525))) +
#   geom_sf(aes(geometry = geometry, color = as.factor(ID), fill = as.factor(ID)))
# 
# ggplot(filter(karst_1_stats_sf, year == 2017 & ID == 612)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(st_crop(filter(karst_1_stats_sf, year == 2017), y = c(xmin = 394225, ymin = 7081012, xmax = 394236, ymax = 7081023))) +
#   geom_sf(aes(geometry = geometry, color = as.factor(ID), fill = as.factor(ID)))
# 
# ggplot(filter(karst_1_stats_sf, year == 2017 & ID == 614)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(st_crop(filter(karst_1_stats_sf, year == 2017), y = c(xmin = 392700, ymin = 7081014, xmax = 392711, ymax = 7081025))) +
#   geom_sf(aes(geometry = geometry, color = as.factor(ID), fill = as.factor(ID)))
# 
# ggplot(filter(karst_1_stats_sf, year == 2017 & ID == 627)) +
#   geom_sf(aes(geometry = geometry, color = ID, fill = ID))
# 
# ggplot(st_crop(filter(karst_1_stats_sf, year == 2017), y = c(xmin = 394502, ymin = 7081015, xmax = 394513, ymax = 7081026))) +
#   geom_sf(aes(geometry = geometry, color = as.factor(ID), fill = as.factor(ID)))

# check that there's nothing funky going on with size by ID
# depth by ID
ggplot(filter(karst_1_stats, year == 2017), aes(x = ID, y = mean.depth)) +
  geom_point()

# map of features colored by depth
ggplot(karst_1_stats_sf) +
  geom_sf(aes(geometry = geometry, color = mean.depth, fill = mean.depth))
mapview(karst_1_stats_sf, zcol = 'mean.depth', map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))

### Thermokarst stats by year
karst_1_summary_year <- karst_1_stats_sf %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarize(n = n(), # there shouldn't be any NA (except for sd and se) so long as using depth not depth.clean
            mean.size = mean(size),
            percent.cover = sum(size)/8.1e+07,
            mean.min.depth = mean(min.depth),
            min.depth = min(min.depth),
            mean.depth = mean(mean.depth),
            median.depth = median(median.depth),
            mean.max.depth = mean(max.depth),
            max.depth = max(max.depth),
            mean.sd.depth = mean(sd.depth, na.rm = TRUE),
            mean.se.depth = mean(se.depth, na.rm = TRUE))
# write.csv(karst_1_summary_year,
#           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/thermokarst_annual_summary.csv')

### Thermokarst stats overall
karst_1_summary <- karst_1_summary_year %>%
  rename(size = mean.size) %>%
  ungroup() %>%
  summarize(mean.size = mean(size),
            percent.cover = mean(percent.cover),
            mean.min.depth = mean(mean.min.depth),
            min.depth = min(min.depth),
            mean.depth = mean(mean.depth),
            median.depth = median(median.depth),
            mean.max.depth = mean(mean.max.depth),
            max.depth = max(max.depth),
            mean.sd.depth = mean(mean.sd.depth, na.rm = TRUE),
            mean.se.depth = mean(mean.se.depth, na.rm = TRUE))

# write.csv(karst_1_summary,
#           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/thermokarst_summary.csv')

# create a data frame to explore relationship between size classes and prevalence, percent cover,
# percent volume, and mean depth
karst_size <- karst_1_stats_sf %>%
  select(-ends_with('clean')) %>%
  mutate(size.cat.exp = ifelse(size <= 10^0,
                              0,
                              ifelse(size <= 10^1,
                                     1,
                                     ifelse(size <= 10^2,
                                            2,
                                            ifelse(size <= 10^3,
                                                   3,
                                                   ifelse(size <= 10^4,
                                                          4,
                                                          ifelse(size <= 10^5,
                                                                 5,
                                                                 ifelse(size <= 10^6,
                                                                        6,
                                                                        NA))))))),
         size.cat = 10^size.cat.exp,
         volume = size*mean.depth) %>%
  select(-c(ID, FID)) %>%
  st_drop_geometry() %>%
  group_by(size.cat, size.cat.exp) %>%
  summarise(extent = sum(size, na.rm = TRUE),
            mean.size = mean(size, na.rm = TRUE),
            total.volume = sum(volume, na.rm = TRUE),
            n = n(),
            across(starts_with('min'), min, na.rm = TRUE),
            across(starts_with('mean'), mean, na.rm = TRUE),
            across(starts_with('max'), max, na.rm = TRUE),
            across(starts_with('med'), median, na.rm = TRUE),
            sd.depth = mean(sd.depth, na.rm = TRUE),
            se.depth = mean(se.depth, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent.cover = extent/8.1e+07,
         percent.features = n/sum(n),
         percent.volume = total.volume/sum(total.volume))

### Make some plots to look for any interesting patterns
# prevalence of features by size
size_prevalence_plot <- ggplot(karst_size, aes(x = size.cat, y = percent.features)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = n),
            vjust = 0,
            nudge_y = 0.02,
            hjust = "left",
            size = 3) +
  scale_x_continuous(breaks = karst_size$size.cat,
                     trans = 'log10') +
  scale_y_continuous(name = 'Prevalence (%)',
                     limits = c(0, 0.55),
                     breaks = seq(0, 0.5, by = 0.1),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
size_prevalence_plot

# areal coverage of thermokarst of different sizes
size_cover_plot <- ggplot(karst_size, aes(x = size.cat, y = percent.cover)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = karst_size$size.cat,
                     trans = 'log10') +
  scale_y_continuous(name = 'Percent Cover',
                     limits = c(0, 0.15),
                     breaks = seq(0, 0.15, by = 0.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
size_cover_plot

# volume of thermokarst of different sizes
size_volume_plot <- ggplot(karst_size, aes(x = size.cat, y = percent.volume)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = karst_size$size.cat,
                     trans = 'log10') +
  scale_y_continuous(name = 'Percent Volume',
                     limits = c(0, 0.85),
                     breaks = seq(0, 0.8, by = 0.2),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
size_volume_plot

# depth of thermokarst of different sizes
size_depth_plot <- ggplot(karst_size, aes(x = size.cat, y = mean.depth*-1)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = karst_size$size.cat,
                     trans = 'log10',
                     name = expression("Thermokarst Size" ~ (m^{2}))) +
  scale_y_continuous(name = 'Mean Depth (m)',
                     limits = c(0, 0.205),
                     breaks = seq(0, 0.2, by = 0.05),
                     labels = scales::number_format(accuracy = 0.01)) +
  theme_bw()
size_depth_plot

# Combined plot
### ADD ANNOTATION TO SHOW WHAT SHAPE MEANS!
size_plot <- ggarrange(size_prevalence_plot,
                       size_cover_plot,
                       size_volume_plot,
                       size_depth_plot,
                       labels = c('a', 'b', 'c', 'd'),
                       ncol = 1,
                       nrow = 4)
size_plot
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/size.jpg',
#        size_plot,
#        height = 10,
#        width = 6)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/size.pdf',
#        size_plot,
#        height = 10,
#        width = 6)
########################################################################################################################

### Thermokarst Morphology (Polygon Compactness via Polsby-Popper Test) ################################################
# Load karst_1_stats_sf from previous section
karst_1_stats_sf <- read_sf('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_1_stats.shp') %>%
  rename(min.depth = min_d,
         mean.depth = mean_d,
         median.depth = med_d,
         max.depth = max_d,
         sd.depth = sd_d,
         se.depth = se_d,
         min.depth.clean = min_d_c,
         mean.depth.clean = mean_d_c,
         median.depth.clean = med_d_c,
         max.depth.clean = max_d_c,
         sd.depth.clean = sd_d_c,
         se.depth.clean= se_d_c)

# remove the cleaned values here, because the summarizing will take care of extreme values
karst_morph <- karst_1_stats_sf %>%
  select(-ends_with('clean')) %>%
  mutate(shape = as.numeric(4*pi*st_area(karst_1_stats_sf)/st_perimeter(karst_1_stats_sf)^2),
         shape.cat.10 = ceiling(shape*10),
         shape.cat.2 = as.factor(ifelse(shape <= 0.2,
                                           1,
                                           2)),
         volume = size*mean.depth)

# extract mean slope for all polygons
slope <- stack('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/slope_9km_1.tif',
               '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/slope_9km_2.tif',
               '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/int_output/slope_9km_3.tif')
slope <- calc(slope, mean, na.rm = TRUE)

slope_extract <- raster::extract(slope,
                                 as(karst_morph, 'Spatial'),
                                 fun = mean,
                                 na.rm = TRUE,
                                 df = TRUE) %>%
  as.data.frame()

karst_morph_slope <- cbind.data.frame(karst_morph, select(slope_extract, -ID)) %>%
  rename(mean.slope = layer)
st_write(karst_morph_slope,
         '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_morphology.shp')
karst_morph_slope <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_morphology.shp') %>%
  rename(min.depth = min_dpth,
         mean.depth = men_dpth,
         median.depth = mdn_dpt,
         max.depth = mx_dpth,
         sd.depth = sd_dpth,
         se.depth = se_dpth,
         shape.cat.10 = shp__10,
         shape.cat.2 = shp_c_2,
         mean.slope = men_slp)

### Make some plots to look for any interesting patterns
karst_morph_sum <- karst_morph_slope %>%
  select(-c(ID, FID)) %>%
  st_drop_geometry() %>%
  group_by(shape.cat.10) %>%
  summarise(extent = sum(size, na.rm = TRUE),
            mean.size = mean(size, na.rm = TRUE),
            total.volume = sum(volume, na.rm = TRUE),
            n = n(),
            across(starts_with('min'), min, na.rm = TRUE),
            across(starts_with('mean'), mean, na.rm = TRUE),
            across(starts_with('max'), max, na.rm = TRUE),
            across(starts_with('med'), median, na.rm = TRUE),
            sd.depth = mean(sd.depth, na.rm = TRUE),
            se.depth = mean(se.depth, na.rm = TRUE)) %>%
  mutate(percent.cover = extent/8.1e+07,
         percent.features = n/sum(n),
         percent.volume = total.volume/sum(total.volume),
         position = c(0, 0.3, 0.45, 0.6, 0.7, 1, 1, 1),
         mean.slope = mean.slope*180/pi)

# # slope by shape - not interesting, not much variation in slope
# shape_slope_plot <- ggplot(karst_morph_sum, aes(x = shape.cat.10, y = mean.slope)) +
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks = seq(1:8)) +
#   scale_y_continuous(#name = 'Prevalence (%)',
#     breaks = seq(0.08, 0.09, by = 0.025),
#     labels = scales::number_format(accuracy = 0.01)) +
#   theme_bw() +
#   theme(axis.title = element_blank(),
#         axis.text.x = element_blank())
# shape_slope_plot

# prevalence of features by shape
shape_prevalence_plot <- ggplot(karst_morph_sum, aes(x = shape.cat.10, y = percent.features)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = n,
                hjust = position),
            vjust = 0,
            nudge_y = 0.02,
            size = 3) +
  scale_x_continuous(breaks = seq(1:8)) +
  scale_y_continuous(#name = 'Prevalence (%)',
    limits = c(0, 0.55),
    breaks = seq(0, 0.5, by = 0.1),
    labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
shape_prevalence_plot

# areal coverage of thermokarst of different shapes
shape_cover_plot <- ggplot(karst_morph_sum, aes(x = shape.cat.10, y = percent.cover)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1:8)) +
  scale_y_continuous(#name = 'Percent Cover'
    limits = c(0, 0.15),
    breaks = seq(0, 0.15, by = 0.05),
    labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
shape_cover_plot

# volume of thermokarst of different shapes
shape_volume_plot <- ggplot(karst_morph_sum, aes(x = shape.cat.10, y = percent.volume)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1:8)) +
  scale_y_continuous(#name = 'Percent Volume',,
    limits = c(0, 0.85),
    breaks = seq(0, 0.8, by = 0.2),
    labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank())
shape_volume_plot

# depth of thermokarst of different shapes
shape_depth_plot <- ggplot(karst_morph_sum, aes(x = shape.cat.10, y = mean.depth*-1)) +
  geom_point() +
  geom_line() +
  geom_text(aes(x = 1, y = 0, label = 'Long'),
            inherit.aes = FALSE,
            vjust = "inward",
            hjust = "inward") +
  geom_text(aes(x = 8, y = 0, label = 'Round'),
            inherit.aes = FALSE,
            vjust = "inward",
            hjust = "inward") +
  geom_segment(aes(x = 2, y = 0.0005, xend = 6.8, yend = 0.0005),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  scale_x_continuous(breaks = seq(1:8),
                     labels = seq(0.1, 0.8, by = 0.1),
                     name = 'Thermokarst Shape') +
  scale_y_continuous(#name = 'Mean Depth',
    limits = c(0, 0.205),
    breaks = seq(0, 0.2, by = 0.05),
    labels = scales::number_format(accuracy = 0.01)) +
  theme_bw() +
  theme(axis.title.y = element_blank())
shape_depth_plot

# Combined plot
### ADD ANNOTATION TO SHOW WHAT SHAPE MEANS!
morphology_plot <- ggarrange(shape_prevalence_plot,
                             shape_cover_plot,
                             shape_volume_plot,
                             shape_depth_plot,
                             labels = c('a', 'b', 'c', 'd'),
                             ncol = 1,
                             nrow = 4)
morphology_plot

size_morph_plot <- ggarrange(size_prevalence_plot,
                             shape_prevalence_plot,
                             size_cover_plot,
                             shape_cover_plot,
                             size_volume_plot,
                             shape_volume_plot,
                             size_depth_plot,
                             shape_depth_plot,
                             ncol = 2,
                             nrow = 4)
size_morph_plot

# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/size_morphology.jpg',
#        size_morph_plot,
#        height = 8,
#        width = 7)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/size_morphology.pdf',
#        size_morph_plot,
#        height = 8,
#        width = 7)

# ggplot(karst_morph_sum, aes(x = shape.cat.10, y = min.depth)) +
#   geom_point() +
#   geom_line()
# ggplot(karst_morph_sum, aes(x = shape.cat.10, y = max.depth)) +
#   geom_point() +
#   geom_line()

# # variation in depth
# ggplot(karst_morph_sum, aes(x = shape.cat.10, y = sd.depth)) +
#   geom_point() +
#   geom_line()
# 
# ggplot(karst_morph_sum, aes(x = shape.cat.10, y = se.depth)) +
#   geom_point() +
#   geom_line()

# Try 0.1 as cut-off for long features (water tracks or human paths)
# or is there a way to break into groups? Could I use an unsupervised classification
# with polsby-popper, length, perimeter, area, or something like that?

map(levels(karst_morph_df$karst_pp_cat),
    ~ ggplot(filter(karst_morph_df, year == 2017 & karst_pp_cat == .x),
             aes(color = karst_pp)) +
      geom_sf() +
      ggtitle(.x))

ggplot(filter(karst_morph_df, year == 2017 & karst_pp <= 0.2), aes(color = karst_pp)) +
  geom_sf()

ggplot(filter(karst_morph_df, year == 2017 & karst_pp > 0.2), aes(color = karst_pp)) +
  geom_sf()

ggplot(filter(karst_morph_df, year == 2017), aes(color = karst_pp_cat_2)) +
  geom_sf() +
  ggtitle(year)

ggplot(filter(karst_morph_df, year == 2018), aes(color = karst_pp_cat_2)) +
  geom_sf() +
  ggtitle(year)

ggplot(filter(karst_morph_df, year == 2019), aes(color = karst_pp_cat_2)) +
  geom_sf() +
  ggtitle(year)


########################################################################################################################

### Identify Thermokarst Edges #########################################################################################
karst_na <- karst_1
for (i in 1:nlayers(karst_na)) {
  karst_na[[i]][karst_na[[i]] == 0] <- NA
}

edges <- brick(boundaries(karst_na[[1]]),
               boundaries(karst_na[[2]]),
               boundaries(karst_na[[3]]))

edges_0 <- edges
for (i in 1:nlayers(edges_0)) {
  edges_0[[i]][is.na(edges_0[[i]])] <- 0
}

karst_1_0 <- karst_1
for (i in 1:nlayers(karst_1_0)) {
  karst_1_0[[i]][is.na(karst_1_0)[[i]]] <- 0
}


# combine edges with thermokarst classification (1 = thermokarst, 2 = thermokarst edge)
# and then extract thermokarst values from that
karst_edges <- brick(karst_1_0[[1]] + edges_0[[1]],
                     karst_1_0[[2]] + edges_0[[2]],
                     karst_1_0[[3]] + edges_0[[3]])

plot(karst_edges[[1]])
plot(karst_edges[[2]])
plot(karst_edges[[3]])

# writeRaster(karst_edges[[1]], '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_edges_1.tif')
# writeRaster(karst_edges[[2]], '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_edges_2.tif')
# writeRaster(karst_edges[[3]], '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_edges_3.tif')
########################################################################################################################


### Subsidence Mixed-Effects Model
######################## DEFINE FUNCTIONS TO EXTRACT AND GRAPH CI #########################
#Extract the coefficients for the fixed effects from your model, make a dataframe with them called model
extract_ci <- function(x) {coefs<-fixef(x) 
modeldf<-as.data.frame(coefs)
#calculate confidence intervals; merge fixed effects and ci into one dataframe
ci <- confint(x,method="boot",boot.type="norm",level=0.95,nsim=1000)
modelci<-merge(ci,modeldf,by="row.names",all.x=F)
#rename colnames so that they make sense and remove symbols
colnames(modelci)<-c("term","min","max","coefs")
return (modelci)}

# graph CI
graph_ci <- function(ci,figtitle,model) {ggplot(ci,aes(x=names,y=coefs))+
    geom_errorbar(aes(ymin=min,ymax=max),width=0,size=1)+
    geom_point(aes(size=2))+
    labs (title = paste(figtitle, ", AIC:", round(AIC(model),2), sep =" ") , x = "Fixed effect", y = "Effect size and 95% CI") +
    guides(size=F,shape=F)+
    theme_bw()+
    theme(axis.text.x=element_text(size=18),
          axis.title.x=element_text(size=26),
          axis.title.y=element_text(size=26,vjust=1),
          axis.text.y=element_text(size=22),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank())+
    geom_hline(yintercept=0)+
    coord_flip() } 
########################################################################################################################

### Mixed Effects Model of Subsidence by Thermokarst Class #############################################################
karst_edges <- brick(stack('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_edges_1.tif',
                           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_edges_2.tif',
                           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/karst_edges_3.tif'))
# create mask of karst_edges
karst_edges_mask <- mask(crop(karst_edges, sub), sub)
  
# take stratified random sample of cells (currently set up for non-thermokarst, thermokarst center and thermokarst edges)
set.seed(333)
samples <- st_as_sf(sampleStratified(karst_edges_mask[[1]], size = 1000, xy = TRUE, sp = TRUE)) %>%
  select(-4) %>%
  mutate(ID = seq(1, 3000))
ggplot(samples, aes(x = x, y = y)) +
  geom_point() +
  coord_fixed()

# st_write(samples, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_rate_samples_500.shp')
# st_write(samples, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_rate_samples_1000.shp')
# st_write(samples, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_rate_samples_2000.shp')
samples <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_rate_samples_1000.shp')

# extract values from subsidence brick
# use a buffer to average out erroneous very high/low reads
# extract all cells from sub (do not average them) join with the karst extract with all cells in
# a 1.5 m buffer and (in later steps) then filter out non-matching thermokarst classes and then average
# sub_extract <- raster::extract(sub, as(samples, 'Spatial'), buffer = 1.5, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame() %>%
#   rename(cell = cells,
#          sub = 3) %>%
#   arrange(ID)
# 
# write.csv(sub_extract, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_extract_gliht_1000.csv')
sub_extract <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_extract_gliht_1000.csv')

# elev_extract <- raster::extract(elev, as(samples, 'Spatial'), buffer = 1.5, layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame() %>%
#   rename(cell = cells,
#          elev.2017 = 3,
#          elev.2018 = 4,
#          elev.2019 = 5) %>%
#   gather(key = year, value = elev, elev.2017:elev.2019) %>%
#   mutate(year = as.numeric(str_sub(year, 6))) %>%
#   arrange(year, ID)


# karst_extract_buffer <- raster::extract(karst_1_0, as(samples, 'Spatial'), buffer = 1.5, layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame() %>%
#   rename(cell = cells,
#          karst.2017 = 3,
#          karst.2018 = 4,
#          karst.2019 = 5) %>%
#   mutate(karst.2017 = ifelse(karst.2017 == 1 | karst.2018 == 1 | karst.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(karst.2017 == 2 | karst.2018 == 2  | karst.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0)),
#          karst.2018 = ifelse(karst.2017 == 1 | karst.2018 == 1 | karst.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(karst.2017 == 2 | karst.2018 == 2  | karst.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0)),
#          karst.2019 = ifelse(karst.2017 == 1 | karst.2018 == 1 | karst.2019 == 1, # if any year is thermokarst reassign as thermokarst
#                           1,
#                           ifelse(karst.2017 == 2 | karst.2018 == 2  | karst.2019 == 2, # if any year is thermokarst edge reassign as thermokarst edge (this will replace thermokarst with thermokarst edge)
#                                  2,
#                                  0))) %>%
#   gather(key = year, value = karst, karst.2017:karst.2019) %>%
#   mutate(year = as.numeric(str_sub(year, 7))) %>%
#   arrange(year, ID)
karst_extract_buffer <- raster::extract(karst_edges,
                                        as(samples, 'Spatial'),
                                        buffer = 1.5,
                                        layer = 1,
                                        nl = 3,
                                        cellnumbers = TRUE,
                                        df = TRUE) %>%
  as.data.frame() %>%
  rename(cell = cells,
         karst.2017 = 3,
         karst.2018 = 4,
         karst.2019 = 5) %>%
  gather(key = year, value = karst, karst.2017:karst.2019) %>%
  group_by(ID, cell) %>%
  summarise(karst = ifelse(any(karst == 2),
                           2,
                           ifelse(any(karst == 1),
                                  1,
                                  0))) %>%
  arrange(ID)

# extract values from thermokarst classification brick
# no buffer so that this dataset can be used to identify the cells which samples are from
# the buffers when joined with the buffered extracted sub and karst
karst_extract <- raster::extract(karst_edges, as(samples, 'Spatial'),
                                 layer = 1,
                                 nl = 3,
                                 cellnumbers = TRUE,
                                 df = TRUE) %>%
  rename(cell = cells,
         karst.2017 = 3,
         karst.2018 = 4,
         karst.2019 = 5) %>%
  gather(key = year, value = sample.cell, karst.2017:karst.2019) %>%
  group_by(ID, cell) %>%
  summarise(sample.cell = ifelse(any(sample.cell == 2),
                           2,
                           ifelse(any(sample.cell == 1),
                                  1,
                                  0))) %>%
  arrange(ID)

# This section will be useful if I end up using neon data and have subsidence values for multiple years
# # join subsidence and thermokarst extract dataframes
# sub_karst <- karst_extract_buffer %>%
#   full_join(sub_extract, by = c('ID', 'cell')) %>%
#   # full_join(elev_extract, by = c('ID', 'cell', 'year')) %>%
#   full_join(karst_extract, by = c('ID', 'cell', 'year')) %>%
#   group_by(year, ID) %>%
#   mutate(sample_cell = ifelse(mean(thermokarst, na.rm = TRUE) == 0,
#                                      0,
#                                      ifelse(mean(thermokarst, na.rm = TRUE) >= 1,
#                                             1)))
# sub_karst_summary <- sub_karst %>%
#   filter(karst == sample_cell) %>%
#   summarise(x = mean(x, na.rm = TRUE),
#             y = mean(y, na.rm = TRUE),
#             thermokarst = mean(thermokarst, na.rm = TRUE),
#             mean.elev = mean(elev),
#             se.elev = sd(elev)/sqrt(n()),
#             mean.sub = mean(sub),
#             se.sub = sd(sub)/sqrt(n())) %>%
#   mutate(time = year - 2017,
#          id.factor = as.factor(ID),
#          thermokarst.presence = as.factor(ifelse(thermokarst == 0,
#                                                  0,
#                                                  ifelse(thermokarst >= 1,
#                                                         1,
#                                                         NA))),
#          thermokarst.class = as.factor(thermokarst),
#          elev.transform = mean.elev^-2) %>%
#   ungroup()#%>%
#   # group_by(ID) %>%
#   # filter((max(se.elev) - min(se.elev)) < 0.1)
# # write.csv(sub_karst_summary, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_summary_1000.csv', row.names = FALSE)
# sub_karst_summary <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_summary_1000.csv')

# sub_karst <- karst_extract_buffer %>%
#   cbind.data.frame(select(sub_extract, sub)) %>%
#   # full_join(elev_extract, by = c('ID', 'cell', 'year')) %>%
#   full_join(karst_extract, by = c('ID', 'cell')) %>%
#   group_by(ID) %>%
#   mutate(sample.class = sample.cell[which(!is.na(sample.cell))])
# 
# sub_karst_summary <- sub_karst %>%
#   filter(karst == sample.class) %>%
#   group_by(ID) %>%
#   summarise(karst = mean(karst),
#             sub = mean(sub))
# write.csv(sub_karst_summary, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/sub_karst_summary_1000.csv', row.names = FALSE)


### Model subsidence by thermokarst class
sub_karst_summary <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_summary_500.csv') %>%
  mutate(karst = factor(karst, levels = c(0, 1, 2)))

# model <- lm(sub ~ karst, sub_karst_summary)
# saveRDS(model, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_anova_500.rds')
model <- readRDS('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_anova_500.rds')
summary(model)
model.contrast <- emmeans(model, specs= pairwise~karst) %>%
  summary(level=0.90)
model.contrast
# write.csv(model.contrast, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_anova_500_contrast.csv')

letters <- data.frame(karst = factor(c(0, 1, 2)),
                      sub = 0.25,
                      letters = c('a', 'b', 'ab'))

model.table <- model.contrast[[1]] %>%
  mutate(letters = c('a', 'b', 'ab'),
         karst = factor(ifelse(karst == 0,
                        'Undisturbed',
                        ifelse(karst == 1,
                               'Thermokarst Center',
                               'Thermokarst Edge')),
                        levels = c('Undisturbed', 'Thermokarst Edge', 'Thermokarst Center'))) %>%
  rename(Class = karst, Mean = emmean, `Lower CI` = lower.CL, `Upper CI` = upper.CL, Group = letters) %>%
  select(Class, Group, Mean, `Lower CI`, `Upper CI`, SE, df)
# write.csv(model.table,
#           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_model.csv',
#           row.names = FALSE)

# boxplot looks really messy
boxplot <- ggplot(sub_karst_summary, aes(x = karst, y = sub, group = karst)) +
  geom_boxplot() +
  geom_text(data = letters, aes(label = letters)) +
  scale_x_discrete(breaks = c(0, 1, 2),
                   labels = c('Undisturbed', 'Thermokarst Center', 'Thermokarst Edge')) +
  scale_y_continuous(name = expression(Delta ~ "Elevation")) +
  theme_bw() +
  theme(axis.title.x = element_blank())

# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_boxplot.jpg',
#        boxplot,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_boxplot.pdf',
#        boxplot,
#        height = 4,
#        width = 5)

# try mean points with se
points_plot <- ggplot(model.table, aes(x = Class, y = Mean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin =`Lower CI`, ymax = `Upper CI`),
                width = 0.1) +
  geom_text(aes(y = -0.08, label = Group)) +
  scale_y_continuous(name = expression(Delta ~ "Elevation"),
                     limits = c(-0.08, 0)) +
  theme_bw() +
  theme(axis.title.x = element_blank())
points_plot

# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_points_se.jpg',
#        points_plot,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_points_se.pdf',
#        points_plot,
#        height = 4,
#        width = 5)

### Add in GPS points as comparison
elev.cip <- list(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevStack_unfilled_unclipped.tif'),
                 brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevStack_unfilled_unclipped.tif'),
                 brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/GPS survey/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevStack_unfilled_unclipped.tif'))

sub.gps <- list()
for (i in 1:3) {
  sub.gps[[i]] <- elev.cip[[i]][[7]] - elev.cip[[i]][[5]]
}

sub.gps <- map_dfr(sub.gps,
                   ~ as.data.frame(.x, xy = TRUE)) %>%
  rename(sub = layer) %>%
  st_as_sf(coords = c('x', 'y'),
           crs = '+proj=tmerc +lat_0=54 +lon_0=-150 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +units=m
+no_defs ') %>%
  st_transform(crs = crs(karst_edges))

gps.sub.extract <- raster::extract(karst_edges, as(sub.gps, 'Spatial'),
                                        cellnumbers = TRUE,
                                        df = TRUE) %>%
  as.data.frame()

sub.gps.summary <- gps.sub.extract %>%
  pivot_longer(karst_edges_1:karst_edges_3, names_to = 'year', values_to = 'karst') %>%
  group_by(ID, cells) %>%
  summarise(karst = ifelse(any(karst == 2),
                           2,
                           ifelse(any(karst == 1),
                                      1,
                                      0))) %>%
  cbind.data.frame(sub.gps) %>%
  select(-c(ID, cells, geometry)) %>%
  group_by(karst) %>%
  summarise(mean.sub = mean(sub, na.rm = TRUE),
            se.sub = sd(sub, na.rm = TRUE)) %>%
  mutate(`Lower CI` = mean.sub - se.sub,
         `Upper CI` = mean.sub + se.sub,
         karst = factor(ifelse(karst == 0,
                               'Undisturbed',
                               ifelse(karst == 1,
                                      'Thermokarst Center',
                                      'Thermokarst Edge')),
                        levels = c('Undisturbed', 'Thermokarst Edge', 'Thermokarst Center')))

# plot mean lidar sub with mean gps sub
points_plot_2 <- ggplot(model.table, aes(x = Class, y = Mean)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(data = sub.gps.summary,
             aes(x = karst, y = mean.sub, color = 'GPS'),
             size = 2,
             inherit.aes = FALSE) +
  geom_errorbar(data = sub.gps.summary,
                aes(x = karst, ymin = `Lower CI`, ymax = `Upper CI`, color = 'GPS'),
                inherit.aes = FALSE,
                width = 0.1) +
  geom_point(size = 3, aes(color = 'LiDAR')) +
  geom_errorbar(aes(ymin =`Lower CI`, ymax = `Upper CI`, color = 'LiDAR'),
                width = 0.1) +
  geom_text(aes(y = -0.14, label = Group)) +
  scale_y_continuous(name = expression(Delta ~ "Elevation"),
                     limits = c(-0.15, 0.05)) +
  scale_color_manual(breaks = c('GPS', 'LiDAR'),
                     values = c('gray', 'black'),
                     guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
points_plot_2

# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_points_w_gps.jpg',
#        points_plot_2,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/sub_karst_points_w_gps.pdf',
#        points_plot_2,
#        height = 4,
#        width = 5)


# points <- list(st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2017_SPCSAK4.shp'),
#                st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2018_SPCSAK4.shp'),
#                st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/All_Points_2019_Aug_SPCSAK4.shp'))
# 
# gps <- map2_dfr(points,
#                 c(2017, 2018, 2019),
#                ~ .x %>%
#                  select(-starts_with('Type')) %>%
#                  mutate(year = .y))
# rm(points)
# 
# gps_grid <- gps %>%
#   mutate(Name = as.character(Name)) %>%
#   mutate(Name = as.numeric(Name)) %>%
#   filter(Name != is.na(Name) & Name < 13000) %>%
#   arrange(Name) %>%
#   mutate(block = as.factor(ifelse(Name >= 10000 & Name < 11000,
#                                   'a',
#                                   ifelse(Name >= 11000 & Name < 12000,
#                                          'b',
#                                          ifelse(Name >= 12000 & Name < 13000,
#                                                 'c',
#                                                 NA))))) %>%
#   filter(!is.na(block)) %>%
#   select(year, block, Name, Easting, Northing, Elevation) %>%
#   st_as_sf() %>%
#   st_transform(crs = 32606) %>%
#   st_zm()
# 
# # check that I got the right points
# ggplot(gps_grid, aes(color = year)) +
#   geom_sf(aes(geometry = geometry)) +
#   facet_wrap(~year)
# 
# karst_points_extract <- raster::extract(karst_edges, as(gps_grid, 'Spatial'),
#                                         cellnumbers = TRUE,
#                                         df = TRUE) %>%
#   as.data.frame()


# mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 0 & sub_karst_summary$year == 2018)])
# mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 1 & sub_karst_summary$year == 2018)])
# mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 0 & sub_karst_summary$year == 2019)])
# mean(sub_karst_summary$mean.sub[which(sub_karst_summary$thermokarst == 1 & sub_karst_summary$year == 2019)])
# 
# hist(sub_karst_summary$mean.elev)
# hist(sub_karst_summary$elev.transform)
# 
# I don't know if it makes sense to filter out mean subsidence values with high standard deviations
# if I'm trying to find a pattern despite noise in the data by getting enough points

# model.full <- lmer(mean.elev ~ time*thermokarst.presence +
#                 (1+time|id.factor), # random intercept and slope for each sample point
#               data = sub_karst_summary,
#               REML = FALSE,
#               control=lmerControl(check.conv.singular="warning"))
# summary(model.full)
# 
# model.intercept <- lmer(mean.elev ~ time + thermokarst.presence +
#                      (1+time|id.factor), # random intercept and slope for each sample point
#                    data = sub_karst_summary,
#                    REML = FALSE,
#                    control=lmerControl(check.conv.singular="warning"))
# summary(model.intercept)
# 
# model.simple <- lmer(mean.elev ~ time +
#                      (1+time|id.factor), # random intercept and slope for each sample point
#                    data = sub_karst_summary,
#                    REML = FALSE,
#                    control=lmerControl(check.conv.singular="warning"))
# summary(model.simple)
# 
# AIC(model.full, model.intercept, model.simple)
# 
# # look at residuals
# model.resid <- resid(model.full)
# model.fitted <- fitted(model.full)
# model.sqrt <- sqrt(abs(resid(model.full)))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model.fitted, model.resid, main='resid, model')
# plot(model.fitted, model.sqrt, main='sqrt resid, model')
# qqnorm(model.resid, main = 'model')
# qqline(model.resid)
# hist(model.resid)
# par(mfrow=c(1,1))
# 
# # The errors are REALLY not normal...
# shapiro.test(sample(model.resid, size = 5000))
# 
# # However, that should not impact the model itself, only the determination of statistical significance
# # of parameters via p-values. Can use bootstrapping instead (which is what we normally do anyway).
# # See: https://data.library.virginia.edu/normality-assumption/
# 
# 
# 
# # re-run with reml = TRUE
# model.full <- lmer(mean.elev ~ time+thermokarst.presence +
#                      (1+time|id.factor), # random intercept and slope for each sample point
#                    data = sub_karst_summary,
#                    control=lmerControl(check.conv.singular="warning"))
# saveRDS(model.full, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_model_1000.rds')
model.full <- readRDS('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/sub_karst_model_1000.rds')
summary(model.full)
r2 <- r.squaredGLMM(model.full)
# # this is crashing R - need fewer samples
# # This doesn't make sense for a model with only different intercepts
# LetterResults <- emmeans(model.full, ~ thermokarst.presence) 
# LetterResults2 <- LetterResults %>% cld(Letters=letters)
# LetterResults

# model_ci <- extract_ci(model.full)
# write.csv(model_ci, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/model_ci_1000.csv', row.names = FALSE)
model_ci <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/model_ci.csv')

# # make confidence interval data frame for graphing
# ConfData <- expand.grid(time = 0:2,
#                         thermokarst.presence = as.factor(0:1))
# 
# myStats <- function(model){
#   out <- predict( model, newdata=ConfData, re.form=~0 )
#   return(out)
# }
# 
# bootObj <- bootMer(model.full, FUN=myStats, nsim = 1000)
# ConfData <- cbind(ConfData, predict(model.full, newdata=ConfData, re.form=~0 )) %>%
#   cbind(confint( bootObj,  level=0.95 ))
# colnames(ConfData) <- c('time', 'thermokarst.presence', 'fit', 'lwr', 'upr')
# # write.csv(ConfData, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_thermokarst_fit.csv', row.names = FALSE)
ConfData <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_thermokarst_fit.csv')

subsidence_model_table <- data.frame(Response = c('Elevation', '', '', ''),
                                     `Full Model` = c('Year*Thermokarst', '', '', ''),
                                     `Final Variables` = c('Intercept (Non-Thermokarst)', 'Intercept (Thermokarst)', 'Year*Non-thermokarst', 'Year*Thermokarst'),
                                     Coeficient = c(model_ci$coefs[1], model_ci$coefs[1] + model_ci$coefs[2], model_ci$coefs[3], model_ci$coefs[3] + model_ci$coefs[4]),
                                     `Min CI` = c(model_ci$coefs[1], model_ci$coefs[1] + model_ci$coefs[2], model_ci$coefs[3], model_ci$coefs[3] + model_ci$coefs[4]),
                                     `Max CI` = c(model_ci$coefs[1], model_ci$coefs[1] + model_ci$coefs[2], model_ci$coefs[3], model_ci$coefs[3] + model_ci$coefs[4]),
                                     `R2 Marginal` = c(r2[1], rep('', 3)),
                                     `R2 Conditional` = c(r2[2], rep('', 3)),
                                     AIC = c(AIC(subsidence_model), rep('', 3)))
write.csv(subsidence_model_table, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/subsidence_model_table.csv', row.names = FALSE)

# ### Look at different slopes - not too different
# sub_karst_summary_2 <- sub_karst_summary %>%
#   mutate(yhat = predict(model.full, re.form = ~(1+time|id.factor)))
# fixed.effects <- fixef(model.full)
# random.effects <- ranef(model.full)[[1]]
# random.effects <- random.effects %>%
#   arrange(time) %>%
#   mutate(order = seq(1:3000),
#          slope = time + fixed.effects[[2]])
# ggplot(random.effects, aes(x = order, y = slope)) +
#   geom_point()

ggplot(sub_karst_summary_2, aes(x = year, y = yhat, color = thermokarst.presence, group = id.factor)) +
  geom_line()
# determine boxcox transformation for elevation
# lambda = -2
# lmodel <- lm(mean.elev ~ time*thermokarst.presence,
#              data = sub_karst_summary)
# MASS::boxcox(lmodel, lambda = seq(-4, 2, 1/10))
# check model residuals of model
# model <- heavyLme(elev.transform ~ time + thermokarst.presence,
#                   random = ~ time,
#                   groups = ~ id.factor,
#                   data = sub_karst_summary)
# 
# summary(model)
# 
# model.sqrt <- sqrt(abs(model$Resid$marginal))
# 
# # graph
# par(mfrow=c(2,2), mar = c(4,4,3,2))
# plot(model$Fitted$marginal, model$Resid$marginal, main='resid, model')
# plot(model$Fitted$marginal, model.sqrt, main='sqrt resid, model')
# qqnorm(model$Resid$marginal, main = 'model')
# qqline(model$Resid$marginal)
# par(mfrow=c(1,1))

# the qq plot shows that there are a lot more extreme values than are expected in a normal distribution
# I want to make sure that is not due to noise in the lidar data
# extract ids with very large or small subsidence values and see if it is just one cell or many
ids <- c(filter(sub_karst_summary, sub < -0.3 | sub > 0.3)$ID)
test_outliers <- sub_karst %>%
  filter(ID %in% ids & year > 2017) %>%
  group_by(ID, year) %>%
  summarise(max.sub = max(sub),
            min.sub = min(sub),
            mean.sub = mean(sub),
            sd = sd(sub))

hist(sub_karst$sub)

ggplot(filter(sub_karst_summary, thermokarst.presence == 0), aes(x = year, y = elev.transform, color = thermokarst.presence)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(group = ID), alpha = 0.25)

ggplot(filter(sub_karst_summary, thermokarst.presence == 1), aes(x = year, y = elev.transform, color = thermokarst.presence)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(group = ID), alpha = 0.25)

ggplot(sub_karst_summary, aes(x = year, y = elev.transform, color = thermokarst.presence)) +
  geom_point(alpha = 0.25) +
  geom_line(aes(group = ID), alpha = 0.25)
########################################################################################################################


### EML Analysis
### Calculate Thermokarst Coverage #####################################################################################
# calculate thermokarst for eml watershed
# karst_eml <- crop(karst_1, as(eml_wtrshd, 'Spatial'))
# writeRaster(karst_eml, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst.tif')
karst_eml <- brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst.tif')
# writeRaster(karst_eml[[1]], '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst_2017.tif')
# writeRaster(karst_eml[[2]], '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst_2018.tif')
# writeRaster(karst_eml[[3]], '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst_2019.tif')
# karst_eml <- brick(stack('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst_2017.tif',
#                          '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst_2018.tif',
#                          '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_karst_2019.tif'))
# karst_eml_mean <- calc(karst_eml, function(x)ifelse(any(x == 1, na.rm = TRUE), 1, 0))
# karst_eml_mean <- mask(karst_eml_mean, as(eml_wtrshd, 'Spatial'))
# writeRaster(karst_eml_mean, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_mean_karst.tif')
karst_eml_mean <- raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_wtshd_mean_karst.tif')
karst_eml_mean_na <- karst_eml_mean
karst_eml_mean_na[karst_eml_mean_na == 0] <- NA
# karst_eml_mean_sp <- rasterToPolygons(karst_eml_mean_na)
# writeOGR(karst_eml_mean_sp,
#          dsn = '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/',
#          layer = 'eml_wtshd_mean_karst.shp',
#          driver = 'ESRI Shapefile')
# 
karst_eml_df <- karst_eml_mean %>%
  as.data.frame(xy = TRUE) %>%
  rename(thermokarst = 3) %>%
  mutate(thermokarst = factor(thermokarst))

ggplot(karst_eml_df, aes(x = x, y = y, fill = thermokarst)) +
  geom_raster() +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  # scale_fill_manual(breaks = c(0, 1),
  #                   values = c('#FFFFFF', '#000000'),
  #                   labels = c('Undisturbed', 'Thermokarst'),
  #                   na.value = 'transparent') +
  scale_fill_viridis(labels = c('Undisturbed', 'Thermokarst'),
                     begin = 0,
                     end = 1,
                     discrete = TRUE,
                     direction = -1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.title = element_blank())
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/eml_thermokarst_map.jpg',
#        height = 10,
#        width = 8)
 
# karst_eml_cover <- karst_eml_df %>%
#   filter(!is.na(thermokarst)) %>%
#   mutate(thermokarst = ifelse(thermokarst == 0,
#                               'undisturbed',
#                               'thermokarst')) %>%
#   group_by(thermokarst) %>%
#   summarise(n.cells = n()) %>%
#   pivot_wider(names_from = 'thermokarst', values_from = 'n.cells') %>%
#   mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))
# 
# write.csv(karst_eml_cover,
#           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_thermokarst_cover.csv',
#           row.names = FALSE)
karst_eml_cover <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/eml_thermokarst_cover.csv')

### The rest of this section is not currently in use
# # This section could be useful if I end up using neon data to get multiple years of subsidence
# karst_eml_df <- karst_eml %>%
#   as.data.frame(xy = TRUE) %>%
#   rename(class.2017 = 3, class.2018 = 4, class.2019 = 5)
# 
# karst_eml_area <- karst_eml_df %>%
#   filter(!is.na(class.2017)) %>%
#   gather(key = 'year', value = 'thermokarst', 3:5) %>%
#   mutate(year = as.numeric(str_sub(year, 7)),
#          thermokarst = ifelse(thermokarst == 0,
#                               'undisturbed',
#                               ifelse(thermokarst == 1,
#                                      'thermokarst',
#                                      NA))) %>%
#   group_by(year, thermokarst) %>%
#   summarise(n = n())
########################################################################################################################


### EC Tower Analysis
### Create ALT Points Dataset ##########################################################################################
### Load Data
ec_alt_2008 <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/footprint08.csv')
ec_alt_2017 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/ALT_Measurements_201708.xlsx')
ec_alt_2019 <- read_excel('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/ec_tower_alt_20190810.xlsx')
points_2017 <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/All_Points_2017_SPCSAK4.shp')
points_2019 <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/All_Points_2019_Aug_SPCSAK4.shp')

### Clean GPS points for joining
points_2008_clean <- ec_alt_2008 %>%
  filter(!is.na(Lat)) %>%
  st_as_sf(coords = c('Long', 'Lat'), crs = 32606) %>%
  mutate(year = 2008,
         td = (DAL1 + DAL2 + DAL3)/3,
         doy = yday(parse_date_time(`Date..T.`, orders = c('m!*/d!/y!')))) %>%
  select(ID = id)

points_2017_clean <- points_2017 %>%
  st_transform(crs = 32606) %>%
  st_zm() %>%
  select(Name, geometry) %>%
  filter(as.numeric(as.character(Name)) > 13000) %>%
  mutate(point = as.numeric(as.character(Name)) - 13000,
         year = 2017) %>%
  filter(point != 222) %>% # duplicate point
  select(ID = point, geometry)

points_2019_clean <- points_2019 %>%
  st_transform(crs = 32606) %>%
  st_zm() %>%
  select(Name, geometry) %>%
  filter(as.numeric(as.character(Name)) > 14000) %>%
  mutate(point = as.numeric(as.character(Name)) - 14000,
         year = 2019) %>%
  select(ID = point, geometry)

# Find the point IDs from 2019 that correspond to 2008 and 2017 point IDs
id_fix_2017 <- st_join(points_2017_clean,
                       points_2008_clean,
                       join = st_is_within_distance,
                       dist = 2) %>%
  rename(ID = ID.x, ID.2008 = ID.y) %>%
  st_drop_geometry()

id_fix_2019 <- st_join(points_2019_clean,
                points_2008_clean,
                join = st_is_within_distance,
                dist = 2) %>%
  rename(ID = ID.x, ID.2008 = ID.y) %>%
  st_drop_geometry()

# Replace IDs with 2017 IDs
points_2017_clean <- points_2017_clean %>%
  inner_join(id_fix_2017, by = c('ID')) %>%
  st_as_sf() %>%
  select(ID = ID.2008)

points_2019_clean <- points_2019_clean %>%
  inner_join(id_fix_2019, by = c('ID')) %>%
  st_as_sf() %>%
  select(ID = ID.2008)

### Create ALT points dataset
alt_points <- points_2017_clean

### Clean ALT Points for joining
ec_alt_2008_clean <- ec_alt_2008 %>%
  filter(!is.na(Lat)) %>%
  mutate(year = 2008,
         td = (DAL1 + DAL2 + DAL3)/3,
         doy = yday(parse_date_time(`Date..T.`, orders = c('m!*/d!/y!')))) %>%
  select(year, doy, ID = id, alt = td) 

ec_alt_2017_clean <- ec_alt_2017 %>%
  filter(Experiment == 'Flux Tower') %>%
  mutate(year = 2017,
         doy = 212) %>% # from the processed gps dbf file (date)
  select(year, doy, ID = `Grid Point`, alt = ALT)

ec_alt_2019_clean <- ec_alt_2019 %>%
  mutate(year = 2019,
         doy = 222) %>% # date in file name
  select(year, doy, ID = point, alt)

# plot all 3 years to make sure coordinate system info is correct
ggplot() +
  geom_sf(data = ec_alt_2008_clean,
          aes(geometry = geometry), color = 'black') +
  geom_sf(data = points_2017_clean,
          aes(geometry = geometry), color = 'red') +
  geom_sf(data = points_2019_clean,
          aes(geometry = geometry), color = 'blue') + 
  coord_sf(datum = st_crs(32606))

### Join All ALT and GPS Points Together
ec_alt_sf <- ec_alt_2008_clean %>%
  full_join(alt_points, by = c('ID')) %>%
  rbind.data.frame(ec_alt_2017_clean %>%
                     filter(ID != 222) %>%
                     full_join(id_fix_2017, by = 'ID') %>%
                     select(-ID) %>%
                     rename(ID = ID.2008) %>%
                     full_join(alt_points, by = c('ID'))) %>%
  rbind.data.frame(ec_alt_2019_clean %>%
                     full_join(id_fix_2019, by = 'ID') %>%
                     select(-ID) %>%
                     rename(ID = ID.2008) %>%
                     full_join(alt_points, by = c('ID'))) %>%
  st_as_sf() %>%
  st_transform(crs = 32606) %>%
  filter(!st_is_empty(geometry))

ggplot() +
  geom_sf(data = ec_alt_sf,
          aes(geometry = geometry,
              color = alt)) +
  facet_wrap(~ year, nrow = 2) +
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1) +
  scale_fill_viridis(direction = -1)

### Clean Up
rm(points_2008_clean, points_2017_clean, points_2019_clean, id_fix_2017, id_fix_2019,
   ec_alt_2008, ec_alt_2008_clean, ec_alt_2017, ec_alt_2017_clean, ec_alt_2019, ec_alt_2019_clean)
########################################################################################################################

### Create EC Tower Footprint Slices ###################################################################################
# # point for ec tower location
# ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
# ec_sf <- st_sf(geometry = ec, crs = 32606)
# 
# # create a circle around the ec tower with radius = 200 m
# circle <- st_buffer(ec, dist = 225)
# circle_sf <- st_sf(geometry = circle)
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = ec_sf, aes(geometry = geometry))
# 
# # create 360 lines at 1 degree angles around ec tower that reach to the circle
# # start by creating a single line the length of the diameter
# line <- st_sfc(st_linestring(matrix(c(389614.25, 389164.25, 7085586.3, 7085586.3), nrow = 2)), crs = 32606)
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = line, aes(geometry = geometry))
# 
# # then rotate the line by 1 degree 179 times
# rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# 
# line_sf <- st_sf(geometry = line, crs = 32606)
# for (i in 1:179) {
#   rad <- i*pi/180
#   line_rotate <- st_sf(geometry = (line - ec) * rot(rad) + ec, crs = 32606)
#   line_sf <- rbind(line_sf, line_rotate)
# }
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = line_sf, aes(geometry = geometry))
# 
# # Snap the lines to the circle
# line_sf <- st_snap(line_sf, circle_sf, tol = 0.1)
# 
# ggplot() +
#   geom_sf(data = circle_sf, aes(geometry = geometry)) +
#   geom_sf(data = split_lines, aes(geometry = geometry))
# 
# wedges <- st_as_sf(st_collection_extract(st_split(circle_sf$geometry,
#                                                   line_sf$geometry),
#                                          "POLYGON"))
# 
# wedges_sf <- wedges %>%
#   mutate(n = seq(1:360))
# 
# st_write(wedges_sf, "Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/wedges_poly_250.shp")
wedges_sf <- st_read("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/wedges_poly_250.shp")

ggplot() +
  geom_sf(data = wedges_sf, aes(color = n)) +
  geom_sf(data = alt_points) +
  coord_sf(datum = st_crs(32606))
########################################################################################################################

### Subsidence Distribution at EC Tower ################################################################################
# Currently only using subsidence on the image-wide analysis, because neither GLiHT or NEON
# appears to be accurate enough in any given cell, so I have to rely on averages of many
# cells to trust the results
# ### Crop and mask GLiHT subsidence dataset to speed up following code
# # This dataset does not cover the entire ec tower footprint
# ec_sub_buffer <- crop(sub,
#                       extend(extent(wedges_sf), c(3)))
# ec_sub <- mask(crop(ec_sub_buffer,
#                     as(wedges_sf, 'Spatial')),
#                as(wedges_sf, 'Spatial'))
# plot(ec_sub)
# ec_sub_mean <- mask(crop(focal(ec_sub_buffer,
#                                w = matrix(c(1,1,1, 1,1,1, 1,1,1),
#                                           nrow = 3),
#                                fun = mean),
#                          as(wedges_sf, 'Spatial')),
#                     as(wedges_sf, 'Spatial'))
# ggplot(as.data.frame(ec_sub_mean, xy = TRUE), aes(x = x, y = y)) +
#   geom_tile(aes(fill = layer)) +
#   geom_sf(data = alt_points, aes(geometry = geometry), inherit.aes = FALSE) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_fill_viridis()
# 
# # ### This section doesn't work very well (relationship between gliht and neon sub is bad at tower...!)
# # # I am going to try gap-filling the GLiHT subsidence with a relationship to the NEON elevation or subsidence
# # # first, calculate mean NEON elevation dataset and crop/mask to ec tower
# # gps_offset <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/lidar_subsidence/gliht_neon_offset.csv')
# # ec_neon_elev <- mask(crop(elev,
# #                           as(wedges_sf, 'Spatial')),
# #                      as(wedges_sf, 'Spatial')) + gps_offset[2:4, 4]
# # 
# # ec_neon_sub <- calc(brick(ec_neon_elev[[2]] - ec_neon_elev[[1]],
# #                           ec_neon_elev[[3]] - ec_neon_elev[[1]]),
# #                     mean,
# #                     na.rm = TRUE)
# # 
# # ec_neon_elev_mean <- calc(crop(ec_neon_elev, as(wedges_sf, 'Spatial')),
# #                           fun = mean,
# #                           na.rm = TRUE)
# # plot(ec_neon_elev)
# # 
# # ec_neon_slope <- mask(crop(terrain(calc(crop(elev,
# #                                              extend(extent(wedges_sf), c(3))),
# #                                         fun = mean,
# #                                         na.rm = TRUE),
# #                                    opt = 'slope'),
# #                            as(wedges_sf, 'Spatial')),
# #                       as(wedges_sf, 'Spatial'))
# # plot(ec_neon_slope)
# # 
# # ec_neon_aspect <- mask(crop(terrain(calc(crop(elev,
# #                                              extend(extent(wedges_sf), c(3))),
# #                                         fun = mean,
# #                                         na.rm = TRUE),
# #                                    opt = 'aspect'),
# #                            as(wedges_sf, 'Spatial')),
# #                       as(wedges_sf, 'Spatial'))
# # plot(ec_neon_aspect)
# # 
# # ec_mtopo15 <- mask(calc(crop(mtopo_brick, as(wedges_sf, 'Spatial')),
# #                         fun = mean,
# #                         na.rm = TRUE),
# #                    as(wedges_sf, 'Spatial'))
# # plot(ec_mtopo15)
# # 
# # ec_terrain_brick <- brick(ec_sub,
# #                           ec_neon_sub,
# #                           ec_neon_elev_mean,
# #                           ec_neon_slope,
# #                           ec_neon_aspect,
# #                           ec_mtopo15)
# # plot(ec_terrain_brick)
# # ec_terrain_df <- as.data.frame(ec_terrain_brick,
# #                                xy = TRUE) %>%
# #   as.data.frame() %>%
# #   rename(gliht_sub = 3,
# #          neon_sub = 4,
# #          neon_elev = 5,
# #          neon_slope = 6,
# #          neon_aspect = 7,
# #          neon_mtopo15 = 8) %>%
# #   mutate(mask = ifelse(is.nan(neon_slope),
# #                        1,
# #                        0),
# #          neon_slope = ifelse(is.nan(neon_slope),
# #                              NA,
# #                              neon_slope))
# # 
# # ec_terrain_df %>%
# #   select(3:8) %>%
# #   GGally::ggpairs(upper = list(continuous = 'points'), lower = list(continuous = 'cor'))
# # 
# # # ggplot(filter(ec_terrain_df, !is.na(gliht_sub)),
# # #        aes(x = neon_sub, y = gliht_sub)) +
# # #   geom_point(alpha = 0.2) +
# # #   geom_smooth(method = 'lm')
# # # 
# # # ggplot(filter(ec_terrain_df, !is.na(gliht_sub)),
# # #        aes(x = neon_elev, y = gliht_sub)) +
# # #   geom_point(alpha = 0.2) +
# # #   geom_smooth(method = 'lm')
# # # 
# # # ggplot(filter(ec_terrain_df, !is.na(gliht_sub)),
# # #        aes(x = neon_slope, y = gliht_sub)) +
# # #   geom_point(alpha = 0.2) +
# # #   geom_smooth(method = 'lm')
# # # 
# # # ggplot(filter(ec_terrain_df, !is.na(gliht_sub)),
# # #        aes(x = neon_aspect, y = gliht_sub)) +
# # #   geom_point(alpha = 0.2) +
# # #   geom_smooth(method = 'lm')
# # # 
# # # ggplot(filter(ec_terrain_df, !is.na(gliht_sub)),
# # #        aes(x = neon_mtopo15, y = gliht_sub)) +
# # #   geom_point(alpha = 0.2) +
# # #   geom_smooth(method = 'lm')
# # 
# # sub.model <- lm(gliht_sub ~ neon_sub*neon_slope*neon_aspect*neon_mtopo15,
# #                 data = filter(ec_terrain_df, mask == 0 & !is.na(gliht_sub)))
# # sub.model <- lm(gliht_sub ~ neon_sub*neon_elev +
# #                   neon_sub*neon_slope +
# #                   neon_sub*neon_aspect +
# #                   neon_sub*neon_mtopo15,
# #                 data = filter(ec_terrain_df, mask == 0 & !is.na(gliht_sub)))
# # summary(sub.model)
# # 
# # ec_terrain_df <- ec_terrain_df %>%
# #   mutate(sub_fill = ifelse(mask == 0,
# #                            predict(sub.model, newdata = .),
# #                            NA))
# # 
# # ggplot(filter(ec_terrain_df, !is.na(gliht_sub)),
# #        aes(x = gliht_sub, y = sub_fill)) +
# #   geom_point(alpha = 0.2) +
# #   geom_smooth(method = 'lm')
# # 
# # gapfill.fit <- lm(sub_fill ~ gliht_sub, data = ec_terrain_df)
# # summary(gapfill.fit)
# 
# ### Extract sub at alt points
# sub_points_extract <- raster::extract(ec_sub_mean, as(alt_points, 'Spatial'),
#                                       cellnumbers = TRUE,
#                                       df = TRUE) %>%
#   as.data.frame()
# 
# ### Extract sub in ec tower slices
# # sub_slice_extract <- raster::extract(sub, as(wedges_sf, 'Spatial'),
# #                                   cellnumbers = TRUE,
# #                                   df = TRUE) %>%
# #   as.data.frame()
# # 
# # sub_slice_sf <- sub_slice_extract %>%
# #   rename(n = ID, sub = 3) %>%
# #   group_by(n) %>%
# #   summarise(mean.sub = mean(sub)) %>%
# #   full_join(wedges_sf, by = c('n'))
# # st_write(sub_slice_sf, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_sub_extract_na.shp')
# sub_slice_sf <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_sub_extract_na.shp') %>%
#   rename(mean.sub = 2)
# 
# ggplot() + 
#   geom_sf(data = sub_slice_sf,
#           aes(geometry = geometry,
#               color = mean.sub,
#               fill = mean.sub)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis() +
#   scale_fill_viridis()
# 
# # ggplot() + 
# #   geom_sf(data = sub_ec_sf,
# #           aes(geometry = geometry,
# #               color = mean.sub.2018,
# #               fill = mean.sub.2018)) + 
# #   coord_sf(datum = st_crs(32606)) +
# #   scale_color_viridis(limits = c(-0.05, 0.15)) +
# #   scale_fill_viridis(limits = c(-0.05, 0.15))
# # 
# # ggplot() + 
# #   geom_sf(data = sub_ec_sf,
# #           aes(geometry = geometry,
# #               color = mean.sub.2019,
# #               fill = mean.sub.2019)) + 
# #   coord_sf(datum = st_crs(32606)) +
# #   scale_color_viridis(limits = c(-0.05, 0.15)) +
# #   scale_fill_viridis(limits = c(-0.05, 0.15))
########################################################################################################################

### Thermokarst Distribution at EC Tower ###############################################################################
### Extract Thermokarst values at ALT points
karst_points_extract_2017 <- raster::extract(karst_eml_mean,
                                             as(filter(ec_alt_sf, year == 2017),
                                                'Spatial'),
                                        cellnumbers = TRUE,
                                        df = TRUE) %>%
  as.data.frame() %>%
  mutate(year = 2017)

karst_points_extract_2019 <- raster::extract(karst_eml_mean,
                                             as(filter(ec_alt_sf, year == 2019),
                                                'Spatial'),
                                             cellnumbers = TRUE,
                                             df = TRUE) %>%
  as.data.frame() %>%
  mutate(year = 2019)

karst_points_extract <- rbind(karst_points_extract_2017, karst_points_extract_2019)
rm(karst_points_extract_2017, karst_points_extract_2019)

# karst_ec_extract <- raster::extract(karst_eml_mean, as(wedges_sf, 'Spatial'),
#                                     cellnumbers = TRUE,
#                                     df = TRUE) %>%
#   as.data.frame()
# write.csv(karst_ec_extract,
#           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/ec_karst_extract.csv',
#           row.names = FALSE)
# 
# karst_ec_percent <- karst_ec_extract %>%
#   group_by(ID) %>%
#   summarise(percent.thermokarst = sum(eml_wtshd_mean_karst/n()))
# write.csv(karst_ec_percent,
#           '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/ec_karst_extract_percent.csv',
#           row.names = FALSE)
# karst_ec_extract_sum <- read.csv('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/ec_karst_extract_percent.csv')
# 
# karst_ec_sf <- karst_ec_extract_sum %>%
#   rename(n = ID) %>%
#   full_join(wedges_sf, by = c('n')) %>%
#   st_as_sf()
# st_write(karst_ec_sf, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/ec_karst_extract_percent.shp')
karst_ec_sf <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/ec_karst_extract_percent.shp') %>%
  rename(n = 1, percent.thermokarst = 2, geometry = 3)

ggplot() +
  geom_sf(data = karst_ec_sf, aes(color = percent.thermokarst, fill = percent.thermokarst)) +
  coord_sf(datum = st_crs(32606)) +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  scale_color_viridis(name = 'Thermokarst (%)',
                      direction = -1) +
  scale_fill_viridis(name = 'Thermokarst (%)',
                     direction = -1) +
  theme_bw()

# could be useful if I end up using neon
# karst_ec_extract_neat <- karst_ec_extract %>%
#   rename(karst.2017 = 3,
#          karst.2018 = 4,
#          karst.2019 = 5) %>%
#   group_by(ID) %>%
#   summarise(karst.2017 = sum(karst.2017)/n(),
#             karst.2018 = sum(karst.2018)/n(),
#             karst.2019 = sum(karst.2019)/n()) %>%
#   pivot_longer(cols = karst.2017:karst.2019, names_to = 'year', values_to = 'karst.percent') %>%
#   mutate(year = as.numeric(str_sub(year, 7)))
# # write.csv(tower_extract_neat, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_karst_extract.csv')

# tower_karst_sf <- tower_extract %>%
#   rename(n = ID, karst.2017 = 3, karst.2018 = 4, karst.2019 = 5) %>%
#   group_by(n) %>%
#   summarise(karst.percent.2017 = sum(karst.2017)/n(),
#             karst.percent.2018 = sum(karst.2018)/n(),
#             karst.percent.2019 = sum(karst.2019)/n()) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(tower_karst_sf, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_karst_extract.shp')
# tower_karst_sf <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_karst_extract.shp') %>%
#   rename(karst.percent.2017 = 2, karst.percent.2018 = 3, karst.percent.2019 = 4)

# ggplot(tower_extract_neat, aes(x = year, y = karst.percent, color = ID, group = ID)) +
#   geom_point() +
#   geom_line()
# 
# 
# ggplot() + 
#   geom_sf(data = tower_karst_sf,
#           aes(geometry = geometry,
#               color = karst.percent.2017,
#               fill = karst.percent.2017)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis(direction = -1,
#                       limits = c(0, 0.6)) +
#   scale_fill_viridis(direction = -1,
#                      limits = c(0, 0.6))
# 
# ggplot() + 
#   geom_sf(data = tower_karst_sf,
#           aes(geometry = geometry,
#               color = karst.percent.2018,
#               fill = karst.percent.2018)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis(direction = -1,
#                       limits = c(0, 0.6)) +
#   scale_fill_viridis(direction = -1,
#                      limits = c(0, 0.6))
# 
# ggplot() + 
#   geom_sf(data = tower_karst_sf,
#           aes(geometry = geometry,
#               color = karst.percent.2019,
#               fill = karst.percent.2019)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis(direction = -1,
#                       limits = c(0, 0.6)) +
#   scale_fill_viridis(direction = -1,
#                      limits = c(0, 0.6))
########################################################################################################################

### Microtopography Distribution (Roughness) at EC Tower ###############################################################
mtopo_brick <- brick(mtopo[[1]][[1]],
                     mtopo[[2]][[1]],
                     mtopo[[3]][[1]])
mean_mtopo <- calc(mtopo_brick, mean, na.rm = FALSE)

# read in Fay's microtopography
mtopo_08 <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Fay/GIS footprint/GIS footprint/Microtopography/micro_clip/w001001.adf')
mtopo_08_df <- as.data.frame(mtopo_08, xy = TRUE) %>%
  rename(mtopo = 3)

transects <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Fay/GIS footprint/GIS footprint/360 degree transects/degree_trans.shp')

# re-calculate microtopography using mean rather than median to compare with Fay's
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All',
                        full.names = TRUE,
                        pattern = '.tif$')
ec_elev <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), wedges_sf),
                     crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), wedges_sf),
                     crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), wedges_sf))

ec_elev_buf <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), extend(extent(wedges_sf), c(25, 25, 25, 25))),
              crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), extend(extent(wedges_sf), c(25, 25, 25, 25))),
              crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), extend(extent(wedges_sf), c(25, 25, 25, 25))))

weights <- focalWeight(ec_elev[[1]], 15, type = 'circle')

ec_mean <- list()
ec_mtopo_mean <- list()
for (i in 1:3) {
  
  ec_mean[[i]] <- focal(ec_elev_buf[[i]], weights)
  ec_mean[[i]] <- mask(crop(ec_mean[[i]], wedges_sf), wedges_sf)
  ec_mtopo_mean[[i]] <- ec_elev[[i]] - ec_mean[[i]]
}

ec_mtopo_mean <- brick(ec_mtopo_mean[[1]],
                       ec_mtopo_mean[[2]],
                       ec_mtopo_mean[[3]])
mtopo_17_19 <- calc(ec_mtopo_mean, mean, na.rm = FALSE)
mtopo_17_19_df <- as.data.frame(mtopo_17_19, xy = TRUE) %>%
  rename(mtopo = 3)

# Change in mtopo (using mean elevation in 15 m )
mtopo_change <- mtopo_17_19 - mtopo_08
mtopo_change_df <- as.data.frame(mtopo_change, xy = TRUE) %>%
  rename(mtopo_change = 3)

change_plot <- ggplot(mtopo_change_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = mtopo_change)) +
  # geom_sf(data = transects, aes(geometry = geometry), inherit.aes = FALSE) +
  scale_fill_viridis(name = expression(Delta ~ 'Microtopography'),
                     na.value = 'transparent') +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())
change_plot

mtopo_08_plot <- ggplot(mtopo_08_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = mtopo)) +
  # geom_sf(data = transects, aes(geometry = geometry), inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Microtopography 2008',
                     limits = c(-1, 0.5),
                     na.value = 'transparent') +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())
mtopo_08_plot

mtopo_17_19_plot <- ggplot(mtopo_17_19_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = mtopo)) +
  # geom_sf(data = transects, aes(geometry = geometry), inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Avg. Microtopography 2017-2019',
                     limits = c(-1, 0.5),
                     na.value = 'transparent') +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())
mtopo_17_19_plot

mtopo_plot <- ggarrange(mtopo_08_plot,
                        mtopo_17_19_plot,
                        change_plot,
                        nrow = 1,
                        ncol = 3)
mtopo_plot

# Plot microtopography at ec tower
mtopo_ec <- mask(crop(mean_mtopo, as(wedges_sf, 'Spatial')), as(wedges_sf, 'Spatial'))
mtopo_ec_df <- mtopo_ec %>%
  as.data.frame(xy = TRUE) %>%
  rename(mtopo15 = 3)

ggplot(mtopo_ec_df, aes(x = x, y = y, fill = mtopo15)) +
  geom_raster() +
  scale_fill_viridis(name = 'Microtopography',
                     na.value = 'transparent') +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/microtopgraphy_map.jpg',
#        height = 4,
#        width = 6)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/microtopgraphy_map.pdf',
#        height = 4,
#        width = 6)

### Extract Thermokarst values at ALT points
mtopo_points_extract_2017 <- raster::extract(mean_mtopo,
                                             as(filter(ec_alt_sf, year == 2017),
                                                'Spatial'),
                                             cellnumbers = TRUE,
                                             df = TRUE) %>%
  as.data.frame() %>%
  mutate(year = 2017)

mtopo_points_extract_2019 <- raster::extract(mean_mtopo,
                                             as(filter(ec_alt_sf, year == 2019),
                                                'Spatial'),
                                             cellnumbers = TRUE,
                                             df = TRUE) %>%
  as.data.frame() %>%
  mutate(year = 2019)

mtopo_points_extract <- rbind(mtopo_points_extract_2017, mtopo_points_extract_2019) %>%
  rename(mtopo_15 = layer)
rm(mtopo_points_extract_2017, mtopo_points_extract_2019)
# mtopo_extract <- raster::extract(mtopo_brick, as(wedges_sf, 'Spatial'), layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame()
# mtopo_extract_neat <- mtopo_extract %>%
#   rename(mtopo15.2017 = 3,
#          mtopo15.2018 = 4,
#          mtopo15.2019 = 5) %>%
#   group_by(ID) %>%
#   summarise(mtopo15.2017 = sum(mtopo15.2017)/n(),
#             mtopo15.2018 = sum(mtopo15.2018)/n(),
#             mtopo15.2019 = sum(mtopo15.2019)/n()) %>%
#   pivot_longer(cols = mtopo15.2017:mtopo15.2019, names_to = 'year', values_to = 'mtopo.15.percent') %>%
#   mutate(year = as.numeric(str_sub(year, 7)))
# 
# mtopo_sf <- mtopo_extract %>%
#   rename(n = ID, mtopo15.2017 = 3, mtopo15.2018 = 4, mtopo15.2019 = 5) %>%
#   group_by(n) %>%
#   summarise(mtopo15.sd.2017 = sd(mtopo15.2017),
#             mtopo15.sd.2018 = sd(mtopo15.2018),
#             mtopo15.sd.2019 = sd(mtopo15.2019)) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(mtopo_sf, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_mtopo15_extract.shp')
mtopo_sf <- st_read('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/tower_mtopo15_extract.shp') %>%
  rename(mtopo15.sd.2017 = 2, mtopo15.sd.2018 = 3, mtopo15.sd.2019 = 4)

mtopo_ec_sf <- mtopo_sf %>%
  st_drop_geometry() %>%
  pivot_longer(mtopo15.sd.2017:mtopo15.sd.2019, names_to = 'year', values_to = 'mtopo15.sd') %>%
  mutate(year = as.numeric(str_sub(year, 12, 15))) %>%
  group_by(n) %>%
  summarise(mtopo15.sd = mean(mtopo15.sd)) %>%
  full_join(wedges_sf, by = c('n')) %>%
  st_as_sf()
rm(mtopo_sf)

ggplot() +
  geom_sf(data = mtopo_ec_sf, aes(color = mtopo15.sd, fill = mtopo15.sd)) +
  coord_sf(datum = st_crs(32606)) +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  scale_color_viridis(name = 'Roughness (m)',
                      direction = -1) +
  scale_fill_viridis(name = 'Roughness (m)',
                     direction = -1) +
  theme_bw()

# ggplot() + 
#   geom_sf(data = mtopo_sf,
#           aes(geometry = geometry,
#               color = mtopo15.sd.2017,
#               fill = mtopo15.sd.2017)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis(direction = -1,
#                       limits = c(0, 0.25)) +
#   scale_fill_viridis(direction = -1,
#                      limits = c(0, 0.25))
# 
# ggplot() + 
#   geom_sf(data = mtopo_sf,
#           aes(geometry = geometry,
#               color = mtopo15.sd.2018,
#               fill = mtopo15.sd.2018)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis(direction = -1,
#                       limits = c(0, 0.25)) +
#   scale_fill_viridis(direction = -1,
#                      limits = c(0, 0.25))
# 
# ggplot() + 
#   geom_sf(data = mtopo_sf,
#           aes(geometry = geometry,
#               color = mtopo15.sd.2019,
#               fill = mtopo15.sd.2019)) + 
#   coord_sf(datum = st_crs(32606)) +
#   scale_color_viridis(direction = -1,
#                       limits = c(0, 0.25)) +
#   scale_fill_viridis(direction = -1,
#                      limits = c(0, 0.25))

# Plot relationship between microtopography and ALT
mtopo_alt <- raster::extract(mtopo_brick, filter(ec_alt_sf, year == 2017 | year == 2019), df = TRUE) %>%
  as.data.frame() %>%
  rename(mtopo15.2017 = 2,
         mtopo15.2018 = 3,
         mtopo15.2019 = 4) %>%
  full_join(filter(ec_alt_sf, year == 2017 | year == 2019), by = 'ID') %>%
  st_as_sf() %>%
  mutate(mtopo15 = ifelse(year == 2017,
                          mtopo15.2017,
                          ifelse(year == 2019,
                                 mtopo15.2019,
                                 NA))) %>%
  select(-c(mtopo15.2017:mtopo15.2019))

ggplot(mtopo_alt, aes(x = mtopo15, y = alt, color = factor(year), group = year)) +
  geom_point() +
  # geom_smooth(method = 'lm',
  #             formula = y ~ poly(x, 2)) +
  scale_colour_manual(name = 'Year',
                      breaks = c(2017, 2019),
                      values = c('gray30', 'gray70')) +
  scale_x_continuous(name = 'Microtopography (m)') +
  scale_y_continuous(name = 'Active Layer Thickness (cm)') +
  theme_bw()
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/mtopo_alt_plot_v2.jpg',
#        height = 4,
#        width = 6)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/mtopo_alt_plot_v2.pdf',
#        height = 4,
#        width = 6)
########################################################################################################################

### ALT Thermokarst Microtopography Analysis ###########################################################################
### Calculate Thaw Penetratration in each ec tower footprint cell
# Start by adjusting ALT to estimate end of August values
gradient_td <- read_excel("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/alt/ThawDepthGradient2019_SiteID.xlsx",
                          sheet = 1)

gradient_td_clean <- gradient_td %>%
  filter((year == 2008 | year == 2017 | year == 2019) & doy >= 214) %>%
  mutate(date = parse_date_time(paste(year, month, day, sep = '-'),
                                orders = c('Y!-m!*-d!')),
         td = td*-1) %>%
  group_by(year, site, date, doy) %>%
  summarise(td = mean(td, na.rm = TRUE)) %>%
  mutate(doy.norm = doy - 214,
         year.factor = factor(year))

ggplot(gradient_td_clean, aes(x = doy.norm, y = td, color = year, group = year)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(gradient_td_clean, aes(x = doy.norm, y = td, color = year, group = year)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_grid(.~site)

# ggplot(gradient_td_clean, aes(x = doy, y = td, color = year, group = year)) +
#   geom_point() +
#   scale_x_continuous(limits = c(214, 274),
#                      breaks = c(214, 245),
#                      labels = c('Aug', 'Sept')) +
#   geom_smooth(method = 'lm')
# 
# ggplot(gradient_td_clean, aes(x = doy, y = td*-1, color = year)) +
#   geom_point() +
#   scale_x_continuous(breaks = c(122, 153, 183, 214, 245),
#                      labels = c('May', 'Jun', 'Jul', 'Aug', 'Sept'))

td.model <- lm(td ~ doy.norm*year.factor, data = gradient_td_clean)
summary(td.model)
td.slopes <- data.frame(year = c(2008, 2017, 2019),
                        td.slope = c(td.model$coefficients[2],
                                  td.model$coefficients[2] + td.model$coefficients[5],
                                  td.model$coefficients[2] + td.model$coefficients[6]))

ec_alt_sf <- ec_alt_sf %>%
  filter(!is.na(year)) %>%
  left_join(td.slopes, by = c('year')) %>%
  mutate(days.to.eos = 243 - doy, # eos = end of season
         alt.eos = alt*-1 + days.to.eos*td.slope)

### Test modeling ALT with roughness and thermokarst in order to gap fill
# Join ALT, mtopo, and thermokarst points
ec_point_data <- ec_alt_sf %>%
  full_join(select(karst_points_extract, ID, year, eml_wtshd_mean_karst), by = c('ID', 'year')) %>%
  full_join(select(mtopo_points_extract, ID, year, mtopo_15), by = c('ID', 'year')) %>%
  rename(karst = eml_wtshd_mean_karst)

ggplot(filter(ec_point_data, !is.na(karst)), aes(x = mtopo_15, y = alt.eos, color = karst)) +
  geom_point() +
  facet_grid(.~karst) +
  geom_smooth(method = 'lm')
# Looks like gap filling ALT doesn't make any sense

### Join thermokarst and roughness to model carbon fluxes
ec_footprint_data <- st_as_sf(karst_ec_sf) %>%
  full_join(select(mtopo_ec_sf, -geometry), by = 'n')


# Join mtopo with tp
ec_all <- ec_tp_sf %>%
  full_join(ec_mtopo_extract, by = c('point', 'year')) %>%
  st_as_sf() %>%
  mutate(gliht.sub.14.18 = gliht_sub_14_18*100) %>%
  select(time, year, doy, days.to.eos, point, mtopo15 = mtopo, mtopo15.sd, alt, alt.corrected,
         gliht.sub.14.18, sub.rate, total.sub, tp)

# plot relationship between spatial variables and tp
ggplot(filter(ec_all, year > 2008), aes(x = mtopo15, y = tp, color = year)) +
  geom_point()

ggplot(filter(ec_all, year > 2008), aes(x = mtopo15.sd, y = tp, color = year)) +
  geom_point() +
  geom_smooth(method = 'lm')

ggplot(filter(ec_all, year > 2008), aes(x = total.sub, y = tp, color = year)) +
  geom_point()

ggplot(filter(ec_all, year > 2008), aes(x = total.sub, y = alt.corrected, color = year)) +
  geom_point()

ggplot(filter(ec_all, year > 2008), aes(x = alt.corrected, y = tp, color = year)) +
  geom_point()

ggplot(filter(ec_all, year > 2008), aes(x = mtopo, y = total.sub, color = year)) +
  geom_point()

tp.model <- lm(tp ~ total.sub, data = filter(ec_all, year > 2008))
summary(tp.model)
tp.model.int <- tp.model$coefficients[1]
tp.model.slope <- tp.model$coefficients[2]

### Model TP in all cells within ec tower using relationship with subsidence
sub_tp_ec <- sub_ec_df %>%
  mutate(sub.rate = gliht_sub_14_18*100/4,
         sub.2008 = 0,
         sub.2017 = sub.rate*9,
         sub.2019 = sub.rate*11) %>%
  pivot_longer(sub.2008:sub.2019, names_to = 'year', values_to = 'sub') %>%
  mutate(year = as.numeric(str_sub(year, 5, 8)),
         tp.fill = tp.model.int + tp.model.slope*sub) %>%
  st_as_sf(coords = c('x', 'y'))

# need to join measured tp with modeled by cell and year,
# but not sure how to do this


### microtopography by thermokarst
karst_mtopo_sf <- karst_ec_sf %>%
  st_drop_geometry() %>%
  full_join(mtopo_ec_sf, 
            by = c('n')) %>%
    st_as_sf() %>%
  mutate(percent.thermokarst.sqr = sqrt(percent.thermokarst),
         color.group.2 = as.factor(ifelse(n > 330 | n < 180,
                                        'SW-S-E',
                                        'W-N-NE')),
         color.group.10 = as.factor(ceiling(n/36)),
         direction = ifelse(n <= 270,
                            n + 90,
                            n - 270))

# used this to figure out direction from n (direction is 0 to 360 clockwise from N)
ggplot(karst_mtopo_sf) +
  geom_sf(aes(color = direction))

ec_karst_plot <- ggplot() +
  geom_sf(data = karst_mtopo_sf, aes(color = percent.thermokarst, fill = percent.thermokarst)) +
  coord_sf(datum = st_crs(32606)) +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  scale_color_viridis(name = 'Thermokarst (%)',
                      direction = -1) +
  scale_fill_viridis(name = 'Thermokarst (%)',
                     direction = -1) +
  theme_bw()
ec_karst_plot

ec_mtopo_plot <- ggplot() +
  geom_sf(data = karst_mtopo_sf, aes(color = mtopo15.sd, fill = mtopo15.sd)) +
  coord_sf(datum = st_crs(32606)) +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  scale_color_viridis(name = 'Roughness (m)',
                      direction = -1) +
  scale_fill_viridis(name = 'Roughness (m)',
                     direction = -1) +
  theme_bw()
ec_mtopo_plot

ec_karst_mtopo_class <- ggplot(karst_mtopo_sf, aes(x = percent.thermokarst, y = mtopo15.sd)) +
  geom_point(aes(color = color.group.2)) +
  theme_bw()
ec_karst_mtopo_class

ec_karst_mtopo_cont <- ggplot(karst_mtopo_sf, aes(x = percent.thermokarst, y = mtopo15.sd)) +
  geom_point(aes(color = direction)) +
  geom_smooth(method = 'gam',
              formula = y ~ s(x, bs = "cs"),
              color = 'black') +
  scale_x_continuous(name = 'Thermokarst (%)') +
  scale_y_continuous(name = 'Roughness (m)') +
  scale_color_gradient(name = "Direction",
                       low = '#CCCCCC',
                       high = '#000000') +
  theme_bw()
ec_karst_mtopo_cont

karst_mtopo <- grid.arrange(ec_karst_plot,
                            ec_mtopo_plot,
                            ec_karst_mtopo_cont,
                            layout_matrix = rbind(c(1, 2),
                                                  c(3, 3)))
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/thermokarst_roughness.jpg',
#        karst_mtopo,
#        height = 8,
#        width = 10)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/thermokarst_roughness.pdf',
#        karst_mtopo,
#        height = 8,
#        width = 10)

# try to figure out why the mtopo values always have 999 or 000 in decimal places 3-5
# # I guess the raw LiDAR elevation somehow is only actually significant to 2 decimal places. Why?
# values_df <- data.frame(elev = getValues(elev[[1]]), median = getValues(median15))
# values_df <- mutate(values_df, diff = elev - median)
########################################################################################################################

### CO2 Analysis #######################################################################################################
# ameriflux
co2 <- read.table('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/Ameriflux/AMF_US-EML_BASE_HH_3-5.csv',
                  sep = ',',
                  skip = 2,
                  header = TRUE,
                  na.strings = "-9999")

# for now, also read in 2018-2019 and 2019-2020 from separate files
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_CO2&CH4_30Apr2019.Rdata')
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_CO2&CH4.Rdata')

new <- rbind(Tower18.19, Tower19.20)

# Select correct times from Ameriflux
co2 <- co2 %>%
  mutate(ts = parse_date_time(TIMESTAMP_END, orders = c('Y!m!*d!H!M!'))) %>%
  filter(ts >= parse_date_time('2017-05-01 00:00', orders = c('Y!-m!*-d! H!:M!')) &
           ts < parse_date_time('2020-05-01 00:00', orders = c('Y!-m!*-d! H!:M!')))

# Select needed data for tower analysis
# could potentially include short term variables such as VPD, PAR, etc
co2.small <- co2 %>%
  select(ts, NEE_PI_F, WD, PPFD_IN_PI_F) %>%
  mutate(n = ceiling(WD),
         month = month(ts),
         season = ifelse(month >= 5 & month <= 9,
                         'gs',
                         'ngs'),
         day = ifelse(PPFD_IN_PI_F >= 10,
                      'day',
                      'night'),
         group = ifelse(season == 'gs' & day == 'day',
                        'GS Day',
                        ifelse(season == 'gs' & day == 'night',
                               'GS Night Respiration',
                               'NGS Respiration')),
         year = ifelse(month >= 5,
                       year(ts),
                       ifelse(month < 5,
                              year(ts) - 1,
                              NA))) %>%
  filter(!is.na(n))

new.small <- new %>%
  select(-46) %>%
  rename(NEE_PI_F = nee_gapfilled, WD = wind_dir, PPFD_IN_PI_F = PAR_filter) %>%
  select(ts, NEE_PI_F, WD, PPFD_IN_PI_F) %>%
  mutate(n = ceiling(WD),
         month = month(ts),
         season = ifelse(month >= 5 & month <= 9,
                         'gs',
                         'ngs'),
         day = ifelse(PPFD_IN_PI_F >= 10,
                      'day',
                      'night'),
         group = ifelse(season == 'gs' & day == 'day',
                        'GS Day',
                        ifelse(season == 'gs' & day == 'night',
                               'GS Night Respiration',
                               'NGS Respiration')),
         year = ifelse(month >= 5,
                       year(ts),
                       ifelse(month < 5,
                              year(ts) - 1,
                              NA))) %>%
  filter(!is.na(n))

co2.small <- co2.small %>%
  rbind.data.frame(new.small)

ggplot(co2.small, aes(x = n, group = group, fill = group)) +
  geom_bar(position = position_dodge())

# Don't actually need this part? Keep all raw data rather than averaging for plot
co2.mean <- co2.small %>%
  group_by(year, n, group) %>%
  summarise(NEE = mean(NEE_PI_F, na.rm = TRUE))


# combine karst and roughness data for the ec tower
karst_roughness <- karst_ec_sf %>%
  st_drop_geometry() %>%
  full_join(mtopo_ec_sf, by = 'n')

# Join co2 and karst/roughness data
co2.model.data <- co2.small %>%
  full_join(karst_roughness, by = 'n')

co2.model.data.mean <- co2.mean %>%
  full_join(karst_roughness, by = 'n')

# thermokarst
# this is very busy, use the one with summarized data instead
# ggplot(co2.model.data,
#        aes(x = percent.thermokarst,
#            y = NEE_PI_F,
#            color = group,
#            group = group,
#            shape = factor(year))) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = 'lm') +
#   scale_x_continuous(name = 'Thermokarst Cover (%)') +
#   scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
#   scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
#                      values = c('#339900', '#990000', '#000033')) +
#   theme_bw() +
#   theme(legend.title = element_blank())

co2.karst.plot <- ggplot(co2.model.data.mean,
       aes(x = percent.thermokarst,
           y = NEE,
           color = group,
           group = group,
           shape = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = 'lm' #,
              # formula = y ~ poly(x, 2) # not sure about using a polynomial, particularly for respiration
              ) +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
  scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
                     values = c('#339900', '#990000', '#000033')) +
  theme_bw() +
  theme(legend.position = 'none')
co2.karst.plot

# roughness
# this is very busy, use the one with summarized data instead
# ggplot(co2.model.data,
#        aes(x = mtopo15.sd,
#            y = NEE_PI_F,
#            color = group,
#            group = group,
#            shape = factor(year))) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = 'lm') +
#   scale_x_continuous(name = 'Roughness') +
#   scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
#   scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
#                      values = c('#339900', '#990000', '#000033')) +
#   theme_bw() +
#   theme(legend.title = element_blank())

co2.roughness.plot <- ggplot(co2.model.data.mean,
       aes(x = mtopo15.sd,
           y = NEE,
           color = group,
           group = group,
           shape = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = 'lm') +
  scale_x_continuous(name = 'Roughness') +
  # scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
  scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
                     values = c('#339900', '#990000', '#000033')) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank())
co2.roughness.plot

co2.plot <- ggarrange(co2.karst.plot,
                      co2.roughness.plot,
                      nrow = 1,
                      ncol = 2,
                      widths = c(0.68, 1))
co2.plot
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/co2_karst_roughness.jpg',
#        co2.plot,
#        height = 6,
#        width = 8)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/co2_karst_roughness.pdf',
#        co2.plot,
#        height = 6,
#        width = 8)
########################################################################################################################

### CH4 Analysis #######################################################################################################
# Fluxnet
ch4 <- read.table('/home/heidi/Documents/School/NAU/Schuur Lab/Eddy Covariance/methane/FLX_US-EML_FLUXNET-CH4_2015-2018_1-1/FLX_US-EML_FLUXNET-CH4_HH_2015-2018_1-1.csv',
                  sep = ',',
                  header = TRUE,
                  na.strings = "-9999")

# Select correct times from Fluxnet
ch4 <- ch4 %>%
  mutate(ts = parse_date_time(TIMESTAMP_END, orders = c('Y!m!*d!H!M!'))) %>%
  filter(ts >= parse_date_time('2017-05-01 00:00', orders = c('Y!-m!*-d! H!:M!')) &
           ts < parse_date_time('2020-05-01 00:00', orders = c('Y!-m!*-d! H!:M!')))

# Select needed data for tower analysis
# could potentially include short term variables such as VPD, PAR, WS, TA, TS, etc
ch4.small <- ch4 %>%
  select(ts, FCH4, PPFD_IN_F) %>%
  mutate(month = month(ts),
         season = ifelse(month >= 5 & month <= 9,
                         'gs',
                         'ngs'),
         day = ifelse(PPFD_IN_F >= 10,
                      'day',
                      'night'),
         year = ifelse(month >= 5,
                       year(ts),
                       ifelse(month < 5,
                              year(ts) - 1,
                              NA)))

ggplot(ch4.small, aes(x = season, group = season, fill = season)) +
  geom_bar(position = position_dodge())

ggplot(ch4.small, aes(x = day, group = day, fill = day)) +
  geom_bar(position = position_dodge())


# combine karst and roughness data for the ec tower

# Join ch4 and karst/roughness data

# thermokarst
# this is very busy, use the one with summarized data instead
# ggplot(co2.model.data,
#        aes(x = percent.thermokarst,
#            y = NEE_PI_F,
#            color = group,
#            group = group,
#            shape = factor(year))) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = 'lm') +
#   scale_x_continuous(name = 'Thermokarst Cover (%)') +
#   scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
#   scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
#                      values = c('#339900', '#990000', '#000033')) +
#   theme_bw() +
#   theme(legend.title = element_blank())

co2.karst.plot <- ggplot(co2.model.data.mean,
                         aes(x = percent.thermokarst,
                             y = NEE,
                             color = group,
                             group = group,
                             shape = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = 'lm' #,
              # formula = y ~ poly(x, 2) # not sure about using a polynomial, particularly for respiration
  ) +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
  scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
                     values = c('#339900', '#990000', '#000033')) +
  theme_bw() +
  theme(legend.position = 'none')
co2.karst.plot

# roughness
# this is very busy, use the one with summarized data instead
# ggplot(co2.model.data,
#        aes(x = mtopo15.sd,
#            y = NEE_PI_F,
#            color = group,
#            group = group,
#            shape = factor(year))) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = 'lm') +
#   scale_x_continuous(name = 'Roughness') +
#   scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
#   scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
#                      values = c('#339900', '#990000', '#000033')) +
#   theme_bw() +
#   theme(legend.title = element_blank())

co2.roughness.plot <- ggplot(co2.model.data.mean,
                             aes(x = mtopo15.sd,
                                 y = NEE,
                                 color = group,
                                 group = group,
                                 shape = factor(year))) +
  geom_point(alpha = 0.3, size = 2) +
  geom_smooth(method = 'lm') +
  scale_x_continuous(name = 'Roughness') +
  # scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ (mu ~ 'mol' ~ 's'^-2))) +
  scale_color_manual(breaks = c('GS Day', 'GS Night Respiration', 'NGS Respiration'),
                     values = c('#339900', '#990000', '#000033')) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank())
co2.roughness.plot

co2.plot <- ggarrange(co2.karst.plot,
                      co2.roughness.plot,
                      nrow = 1,
                      ncol = 2,
                      widths = c(0.68, 1))
co2.plot
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/co2_karst_roughness.jpg',
#        co2.plot,
#        height = 6,
#        width = 8)
# ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/figures/co2_karst_roughness.pdf',
#        co2.plot,
#        height = 6,
#        width = 8)
########################################################################################################################