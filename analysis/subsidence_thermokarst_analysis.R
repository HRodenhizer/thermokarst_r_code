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
library(lmerTest)
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
library(ggnewscale)
library(ggpubr)
library(gridExtra)
library(spatialEco)
library(zoo)
library(data.table)
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
sub <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/lidar_subsidence/gliht_sub_14_18.tif')
karst_1 <- brick(stack("/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_raster_final_1.tif",
                       "/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_raster_final_2.tif",
                       "/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_raster_final_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')

eml_wtrshd <- st_read("/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/eml_bnd/boundry_poly3.shp")

filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output',
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
# write.csv(karst_cover, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_area.csv')
########################################################################################################################

### Summary Statistics From Polygons ###################################################################################
# Depth of thermokarst features is calculated in polygon_summary_statistics.R
# Read in all of the files from polygon_summary_statistics.R and reformat
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/polygon_summary_split/redo_fill',
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
# write.csv(karst_1_depth_join, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_depth_join.csv', row.names = FALSE)
# rm(karst_1_depth)
# 
# plot(karst_1_depth_join)
# karst_1_depth_join <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_depth_join.csv')


### Old Note ###
# # karst polygons used to extract stats were not dissolved properly
# # need to figure out the actual polygon ids for summarizing

# I am running reassign_polygon_ids.sh on monsoon directly.
# load file with correct polygon ids
# karst_1_depth_join <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/code/analysis/old/karst_1_depth_join_new_ids.csv')
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
# write.csv(karst_1_depth_filter, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_depth_clean.csv', row.names = FALSE)
# karst_1_depth_filter <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_depth_clean.csv')

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
# write.csv(karst_1_stats, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_stats_by_polygon.csv', row.names = FALSE)
karst_1_stats <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_stats_by_polygon.csv')

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
# st_write(karst_1_stats_sf, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_stats.shp')
# st_write(filter(karst_1_stats_sf, year == 2017), '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_poly_fill_final_1.shp')
# st_write(filter(karst_1_stats_sf, year == 2018), '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_poly_fill_final_2.shp')
# st_write(filter(karst_1_stats_sf, year == 2019), '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_poly_fill_final_3.shp')
karst_1_stats_sf <- read_sf('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_stats.shp') %>%
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
#          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_1_final_poly_1.shp')
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
#          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_1_final_poly_2.shp')
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
#          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_1_final_poly_3.shp')
# karst_1_filter_list <- list(st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_poly_fill_final_1.shp'),
#                             st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_poly_fill_final_2.shp'),
#                             st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_poly_fill_final_3.shp'))
# karst_1_raster_filter <- brick(map(karst_1_filter_list,
#                                    ~ rasterize(as(.x, 'Spatial'), karst_1[[1]])))
# karst_1_raster_reclass <- reclassify(karst_1_raster_filter,
#                                      rcl = matrix(c(-1,Inf,1),
#                                                   ncol = 3,
#                                                   byrow = TRUE))
# writeRaster(karst_1_raster_filter, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_final_polygon_id.tif')
# writeRaster(karst_1_raster_reclass, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_final.tif')
# karst_1_raster_filter <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_final.tif')

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
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_annual_summary.csv')

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
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_summary.csv')

########################################################################################################################

### Thermokarst Morphology (Polygon Compactness via Polsby-Popper Test) ################################################
# Load karst_1_stats_sf from previous section
karst_1_stats_sf <- read_sf('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_stats.shp') %>%
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
  summarise(extent = sum(size, na.rm = TRUE)/3, # divide by 3 to get average of the 3 years
            mean.size = mean(size, na.rm = TRUE),
            total.volume = sum(volume, na.rm = TRUE)/3,
            n = round(n()/3),
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
                     limits = c(0, 0.05),
                     breaks = seq(0, 0.05, by = 0.01),
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/size.jpg',
#        size_plot,
#        height = 10,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/size.pdf',
#        size_plot,
#        height = 10,
#        width = 6)


# remove the cleaned values column here, because the summarizing will take care of extreme values
karst_morph <- karst_1_stats_sf %>%
  select(-ends_with('clean')) %>%
  mutate(shape = as.numeric(4*pi*st_area(karst_1_stats_sf)/st_perimeter(karst_1_stats_sf)^2),
         shape.cat.10 = ceiling(shape*10),
         shape.cat.2 = as.factor(ifelse(shape <= 0.2,
                                           1,
                                           2)),
         volume = size*mean.depth)

# extract mean slope for all polygons
slope <- stack('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/int_output/slope_9km_1.tif',
               '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/int_output/slope_9km_2.tif',
               '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/int_output/slope_9km_3.tif')
slope <- calc(slope, mean, na.rm = TRUE)

slope_extract <- raster::extract(slope,
                                 as(karst_morph, 'Spatial'),
                                 fun = mean,
                                 na.rm = TRUE,
                                 df = TRUE) %>%
  as.data.frame()

karst_morph_slope <- cbind.data.frame(karst_morph, select(slope_extract, -ID)) %>%
  rename(mean.slope = layer)
# st_write(karst_morph_slope,
#          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_morphology.shp')
karst_morph_slope <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_morphology.shp') %>%
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
  summarise(extent = sum(size, na.rm = TRUE)/3, # divide by three to get average of 3 years
            mean.size = mean(size, na.rm = TRUE),
            total.volume = sum(volume, na.rm = TRUE)/3,
            n = round(n()/3),
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
    limits = c(0, 0.05),
    breaks = seq(0, 0.05, by = 0.01),
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
  geom_segment(aes(x = 2.3, y = 0.0005, xend = 6.5, yend = 0.0005),
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/morphology.jpg',
#        morphology_plot,
#        height = 10,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/morphology.pdf',
#        morphology_plot,
#        height = 10,
#        width = 6)


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

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/size_morphology.jpg',
#        size_morph_plot,
#        height = 7.5,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/size_morphology.pdf',
#        size_morph_plot,
#        height = 7.5,
#        width = 6.5)

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

# writeRaster(karst_edges[[1]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_1.tif')
# writeRaster(karst_edges[[2]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_2.tif')
# writeRaster(karst_edges[[3]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_3.tif')
########################################################################################################################

### Landform Classification ############################################################################################
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/DTM_All',
                        full.names = TRUE,
                        pattern = '.tif$')
crop_extent <- extent(matrix(c(385600, 396800, 7080000, 7091000), nrow = 2, byrow = TRUE))
elev <- brick(crop(raster(filenames[which(str_detect(filenames, '2017.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2018.tif$'))]), crop_extent),
              crop(raster(filenames[which(str_detect(filenames, '2019.tif$'))]), crop_extent))
mean.elev <- calc(elev, fun = mean, na.rm = TRUE)
plot(mean.elev)

source('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_r_code/analysis/landfClass_eml.R')

# test with coarse resolution
test.elev <- aggregate(mean.elev, fact = 5)
test.elev <- aggregate(mean.elev, fact = 10)

plot(test.elev)

# takes ~2 minutes to run with 10 m data and scale = 125
start <- Sys.time()
landforms <- landfClass(test.elev, scale = 125)
end <- Sys.time()
difftime(end, start)

# ~2 min*100 = 3-4 hours 
landforms <- landfClass(mean.elev,
                        scale = 1251)
writeRaster(landforms[[1]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tpi.tif')
writeRaster(landforms[[2]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/landforms.tif')

# convert landform classes to vector
landclasses <- rasterToPolygons(landforms[[2]])
st_write(landclasses, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/landforms.shp')
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

extract_ci_lm <- function(x) {
  coefs<-coef(x) 
  modeldf<-as.data.frame(coefs)
  #calculate confidence intervals; merge fixed effects and ci into one dataframe
  ci <- confint(x,method="boot",boot.type="norm",level=0.95,nsim=1000)
  modelci<-merge(ci,modeldf,by="row.names",all.x=F)
  #rename colnames so that they make sense and remove symbols
  colnames(modelci)<-c("term","min","max","coefs")
  return (modelci)
}

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
karst_edges <- brick(stack('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_1.tif',
                           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_2.tif',
                           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_edges_3.tif'))
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

# st_write(samples, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/subsidence_rate_samples_500.shp')
# st_write(samples, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/subsidence_rate_samples_1000.shp')
# st_write(samples, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/subsidence_rate_samples_2000.shp')
samples <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/subsidence_rate_samples_1000.shp')

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
# write.csv(sub_extract, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/sub_extract_gliht_1000.csv')
sub_extract <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/sub_extract_gliht_1000.csv')

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


### Model subsidence by thermokarst class
sub_karst_summary <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/sub_karst_summary_500.csv') %>%
  mutate(karst = factor(karst, levels = c(0, 1, 2)))

# model <- lm(sub ~ karst, sub_karst_summary)
# saveRDS(model, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/sub_karst_anova_500.rds')
model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/sub_karst_anova_500.rds')
summary(model)
model.contrast <- emmeans(model, specs= pairwise~karst) %>%
  summary(level=0.90)
model.contrast
# write.csv(model.contrast, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/sub_karst_anova_500_contrast.csv')

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
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_model.csv',
#           row.names = FALSE)
model.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_model.csv') %>%
  rename(`Lower CI` = Lower.CI, `Upper CI` = Upper.CI) %>%
  mutate(Class = factor(Class,
                        levels = c('Non-Thermokarst', 'Thermokarst Edge', 'Thermokarst Center')))

# boxplot looks really messy
boxplot <- ggplot(sub_karst_summary, aes(x = karst, y = sub, group = karst)) +
  geom_boxplot() +
  geom_text(data = letters, aes(label = letters)) +
  scale_x_discrete(breaks = c(0, 1, 2),
                   labels = c('Non-Thermokarst', 'Thermokarst Center', 'Thermokarst Edge')) +
  scale_y_continuous(name = expression(Delta ~ "Elevation")) +
  theme_bw() +
  theme(axis.title.x = element_blank())
boxplot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_boxplot.jpg',
#        boxplot,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_boxplot.pdf',
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_points_se.jpg',
#        points_plot,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_points_se.pdf',
#        points_plot,
#        height = 4,
#        width = 5)

### Add in GPS points as comparison
elev.cip <- list(brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/AElevStack_unfilled_unclipped.tif'),
                 brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/BElevStack_unfilled_unclipped.tif'),
                 brick('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Kriged_Surfaces/Elevation_Variance/ALT_Sub_Ratio_Corrected/Elevation_Stacks/CElevStack_unfilled_unclipped.tif'))

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
                               'Non-Thermokarst',
                               ifelse(karst == 1,
                                      'Thermokarst Center',
                                      'Thermokarst Edge')),
                        levels = c('Non-Thermokarst', 'Thermokarst Edge', 'Thermokarst Center')))

# plot mean lidar sub with mean gps sub
points_plot_2 <- ggplot(model.table, aes(x = Class, y = Mean)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(data = sub.gps.summary,
             aes(x = karst, y = mean.sub, color = 'GPS'),
             size = 2,
             inherit.aes = FALSE,
             position = position_nudge(x = 0.05)) +
  geom_errorbar(data = sub.gps.summary,
                aes(x = karst, ymin = `Lower CI`, ymax = `Upper CI`, color = 'GPS'),
                inherit.aes = FALSE,
                width = 0.1,
                position = position_nudge(x = 0.05)) +
  geom_point(size = 3, aes(color = 'LiDAR'),
             position = position_nudge(x = -0.05)) +
  geom_errorbar(aes(ymin =`Lower CI`, ymax = `Upper CI`, color = 'LiDAR'),
                width = 0.1,
                position = position_nudge(x = -0.05)) +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_points_w_gps.jpg',
#        points_plot_2,
#        height = 4,
#        width = 5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/sub_karst_points_w_gps.pdf',
#        points_plot_2,
#        height = 4,
#        width = 5)


########################################################################################################################


### EML Analysis
### Calculate Thermokarst Coverage #####################################################################################
# calculate thermokarst for eml watershed
# karst_eml <- crop(karst_1, as(eml_wtrshd, 'Spatial'))
# writeRaster(karst_eml, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst.tif')
karst_eml <- brick('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst.tif')
# writeRaster(karst_eml[[1]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst_2017.tif')
# writeRaster(karst_eml[[2]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst_2018.tif')
# writeRaster(karst_eml[[3]], '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst_2019.tif')
# karst_eml <- brick(stack('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst_2017.tif',
#                          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst_2018.tif',
#                          '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_karst_2019.tif'))
# karst_eml_mean <- calc(karst_eml, function(x)ifelse(any(x == 1, na.rm = TRUE), 1, 0))
# karst_eml_mean <- mask(karst_eml_mean, as(eml_wtrshd, 'Spatial'))
# writeRaster(karst_eml_mean, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_mean_karst.tif')
karst_eml_mean <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_wtshd_mean_karst.tif')
karst_eml_mean_na <- karst_eml_mean
karst_eml_mean_na[karst_eml_mean_na == 0] <- NA
# karst_eml_mean_sp <- rasterToPolygons(karst_eml_mean_na)
# writeOGR(karst_eml_mean_sp,
#          dsn = '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/',
#          layer = 'eml_wtshd_mean_karst.shp',
#          driver = 'ESRI Shapefile')
# 
karst_eml_df <- karst_eml_mean %>%
  as.data.frame(xy = TRUE) %>%
  rename(thermokarst = 3) %>%
  mutate(thermokarst = factor(thermokarst))

eml_karst_plot <- ggplot(karst_eml_df, aes(x = x, y = y, fill = thermokarst)) +
  geom_raster() +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  # scale_fill_manual(breaks = c(0, 1),
  #                   values = c('#FFFFFF', '#000000'),
  #                   labels = c('Undisturbed', 'Thermokarst'),
  #                   na.value = 'transparent') +
  scale_fill_viridis(labels = c('Non-Thermokarst', 'Thermokarst'),
                     begin = 0.15,
                     end = 0.95,
                     discrete = TRUE,
                     direction = -1,
                     na.translate = FALSE) +
  coord_fixed() +
  theme_bw() +
  theme(legend.title = element_blank())
eml_karst_plot
# retrieve thermokarst color: #463480FF
show_col(viridis_pal(begin = 0.15, end = 0.85)(2))
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/eml_thermokarst_map.jpg',
#        eml_karst_plot,
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
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_thermokarst_cover.csv',
#           row.names = FALSE)
karst_eml_cover <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_thermokarst_cover.csv')

# Test whether resolution changes the observed thermokarst percent cover
karst_eml_mean_3m <- aggregate(karst_eml_mean, fact = 3, fun = 'median')
karst_eml_mean_3m[karst_eml_mean_3m == 0.5] <- 1
# karst_eml_mean_3m[karst_eml_mean_3m < 0.5] <- 0
# karst_eml_mean_3m[karst_eml_mean_3m >= 0.5] <- 1
karst_eml_mean_3m[is.nan(karst_eml_mean_3m) | is.na(karst_eml_mean_3m)] <- NA

karst_eml_df_3m <- karst_eml_mean_3m %>%
  as.data.frame(xy = TRUE) %>%
  rename(thermokarst = 3) %>%
  mutate(thermokarst = factor(thermokarst))

karst_eml_cover_3m <- karst_eml_df_3m %>%
  filter(!is.na(thermokarst)) %>%
  mutate(thermokarst = ifelse(thermokarst == 0,
                              'undisturbed',
                              'thermokarst')) %>%
  group_by(thermokarst) %>%
  summarise(n.cells = n()) %>%
  pivot_wider(names_from = 'thermokarst', values_from = 'n.cells') %>%
  mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))

ggplot(karst_eml_df_3m, aes(x = x, y = y, fill = thermokarst)) +
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
########################################################################################################################


### EC Tower Analysis
### Create ALT Points Dataset ##########################################################################################
### Load Data
ec_alt_2008 <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/alt/footprint08.csv')
ec_alt_2017 <- read_excel('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/alt/ALT_Measurements_201708.xlsx')
ec_alt_2019 <- read_excel('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/alt/ec_tower_alt_20190810.xlsx')
points_2017 <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/alt/All_Points_2017_SPCSAK4.shp')
points_2019 <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/alt/All_Points_2019_Aug_SPCSAK4.shp')

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
# point for ec tower location
ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
ec_sf <- st_sf(geometry = ec, crs = 32606)

# create a circle around the ec tower with radius = 200 m
circle <- st_buffer(ec, dist = 225)
circle_sf <- st_sf(geometry = circle)

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
wedges_sf <- st_read("/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/wedges_poly_250.shp")
ggplot() +
  geom_sf(data = wedges_sf, aes(color = n)) +
  coord_sf(datum = st_crs(32606)) +
  geom_sf(data = alt_points)
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
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ec_karst_extract.csv',
#           row.names = FALSE)
# 
# karst_ec_percent <- karst_ec_extract %>%
#   group_by(ID) %>%
#   summarise(percent.thermokarst = sum(eml_wtshd_mean_karst/n()))
# write.csv(karst_ec_percent,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ec_karst_extract_percent.csv',
#           row.names = FALSE)
# karst_ec_extract_sum <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ec_karst_extract_percent.csv')
# 
# karst_ec_sf <- karst_ec_extract_sum %>%
#   rename(n = ID) %>%
#   full_join(wedges_sf, by = c('n')) %>%
#   st_as_sf()
# st_write(karst_ec_sf, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ec_karst_extract_percent.shp')
karst_ec_sf <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ec_karst_extract_percent.shp') %>%
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
# # write.csv(tower_extract_neat, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tower_karst_extract.csv')

# tower_karst_sf <- tower_extract %>%
#   rename(n = ID, karst.2017 = 3, karst.2018 = 4, karst.2019 = 5) %>%
#   group_by(n) %>%
#   summarise(karst.percent.2017 = sum(karst.2017)/n(),
#             karst.percent.2018 = sum(karst.2018)/n(),
#             karst.percent.2019 = sum(karst.2019)/n()) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(tower_karst_sf, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tower_karst_extract.shp')
# tower_karst_sf <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tower_karst_extract.shp') %>%
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
mtopo_brick <- brick(calc(mtopo[[1]], fun = min, na.rm = TRUE),
                     calc(mtopo[[2]], fun = min, na.rm = TRUE),
                     calc(mtopo[[3]], fun = min, na.rm = TRUE))
mean_mtopo <- calc(mtopo_brick, mean, na.rm = FALSE)

# read in Fay's microtopography
mtopo_08 <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Fay/GIS footprint/GIS footprint/Microtopography/micro_clip/w001001.adf')
mtopo_08_df <- as.data.frame(mtopo_08, xy = TRUE) %>%
  rename(mtopo = 3)

# # Fay started with ok_raster (1.45 m), subtracted min elevation to get scale_zero1 (1.45 m),
# # aggregated the scaled raster to sz_30 (30 m), resampled to sz_30_1_raster or sz_30_1 (1.45 m),
# # and subtracted sz_30_1_raster from scale_zero1 to get DE_raster (microtopography, 1.45 m)
# scale_zero1 <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Fay/GIS footprint/GIS footprint/Dev_ele/scale_zero1.img')
# sz_30_1_raster <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Fay/GIS footprint/GIS footprint/Dev_ele/sz_30_1_raster.img')
# DE_raster_remake <- scale_zero1 - sz_30_1_raster
# plot(DE_raster_remake)

# transects <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/Fay/GIS footprint/GIS footprint/360 degree transects/degree_trans.shp')

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

# calculate microtopography following Fay's methods
ec_mean <- list()
ec_mtopo_mean <- list()
for (i in 1:3) {
  ec_mean[[i]] <- aggregate(ec_elev_buf[[i]], fact = 30)
  ec_mean[[i]] <- resample(ec_mean[[i]], raster(extent(ec_mean[[i]]), resolution = 1.5, crs = crs(ec_mean[[i]])))
  ec_mtopo_mean[[i]] <- resample(ec_elev_buf[[i]], ec_mean[[i]]) - ec_mean[[i]]
}
plot(ec_elev_buf[[1]])
plot(ec_mean[[1]])
plot(ec_mtopo_mean[[1]])

ec_mtopo_mean <- brick(ec_mtopo_mean[[1]],
                       ec_mtopo_mean[[2]],
                       ec_mtopo_mean[[3]])
mtopo_17_19 <- calc(ec_mtopo_mean, mean, na.rm = FALSE)
mtopo_17_19_df <- as.data.frame(mtopo_17_19, xy = TRUE) %>%
  rename(mtopo = 3)
mtopo_plot_fays_method <- ggplot(mtopo_17_19_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = mtopo)) +
  # geom_sf(data = transects, aes(geometry = geometry), inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Microtopography',
                     na.value = 'transparent') +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())
mtopo_plot_fays_method

# Difference in methods
mtopo_method_diff <- mtopo_17_19 - resample(mean_mtopo, mtopo_17_19)
mtopo_method_df <- as.data.frame(mtopo_method_diff, xy = TRUE) %>%
  rename(mtopo.diff = 3)
mtopo_method_diff_plot <- ggplot(mtopo_method_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = mtopo.diff)) +
  # geom_sf(data = transects, aes(geometry = geometry), inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Microtopography Difference',
                     na.value = 'transparent') +
  theme_bw() +
  coord_fixed() +
  theme(axis.title = element_blank())
mtopo_method_diff_plot
t.test(mtopo_method_df$mtopo.diff)

# Change in mtopo (using mean elevation in 15 m )
mtopo_change <- mtopo_17_19 - resample(mtopo_08, mtopo_17_19)
# mtopo_change <- mask(crop(mtopo_change, as(wedges_sf, 'Spatial')), as(wedges_sf, 'Spatial'))

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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/microtopography_change_08_19.jpg',
#        change_plot,
#        height = 4,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/microtopography_change_08_19.pdf',
#        change_plot,
#        height = 4,
#        width = 6)

# change boxplot
mtopo.change.boxplot <- ggplot(mtopo_change_df, aes(y = mtopo_change)) +
  geom_boxplot() +
  scale_y_continuous(name = expression(Delta ~ 'Microtopography (m)')) +
  theme_bw() +
  theme(axis.text.x = element_blank())
mtopo.change.boxplot
mtopo.change.boxplot <- ggplotGrob(mtopo.change.boxplot)

# summarize change in microtopography
mtopo_change_summary <- mtopo_change_df %>%
  filter(!is.na(mtopo_change)) %>%
  summarize(mean_mtopo_change = mean(mtopo_change),
            se_mtopo_change = sd(mtopo_change)/sqrt(n())) %>%
  mutate(x = 0)

mtopo.change.point <- ggplot(mtopo_change_summary, aes(x = x)) +
  geom_point(aes(y = mean_mtopo_change)) +
  geom_errorbar(aes(ymin = mean_mtopo_change - se_mtopo_change,
                    ymax = mean_mtopo_change + se_mtopo_change),
                width = 0.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_y_continuous(name = expression(Delta ~ 'Microtopography (m)'),
                     limits = c(-0.01, 0.02)) +
  scale_x_continuous(limits = c(-1, 1)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank())
mtopo.change.point
mtopo.change.point <- ggplotGrob(mtopo.change.point)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/microtopography_change_08_19_boxplot.jpg',
#        mtopo.change.boxplot,
#        height = 4,
#        width = 2)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/microtopography_change_08_19_boxplot.pdf',
#        mtopo.change.boxplot,
#        height = 4,
#        width = 2)
summary(mtopo_change_df$mtopo_change)
change.test <- t.test(mtopo_change_df$mtopo_change)
change.test # they are different!(?) But probably only because of huge sample size.

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
mtopo_ec <- crop(mean_mtopo, extend(extent(as(circle_sf, 'Spatial')), 25))
mtopo_ec_df <- mtopo_ec %>%
  as.data.frame(xy = TRUE) %>%
  rename(mtopo15 = 3)

# Microtopography with change overlaid
mtopo.change.plot <- ggplot(mtopo_ec_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = mtopo15)) +
  scale_fill_viridis(name = 'Microtopography (m)',
                     na.value = 'transparent') +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, color = "black", fill = NA) +
  theme_bw() +
  coord_sf(clip = "off",
           datum = st_crs(circle_sf),
           expand = FALSE) +
  theme(axis.title = element_blank(),
        legend.justification = 'top') +
  annotation_custom(mtopo.change.boxplot,
                    xmin = 389660,
                    xmax = 389875,
                    ymin = 7085350,
                    ymax = 7085600)
mtopo.change.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/microtopgraphy_map.jpg',
#        mtopo.change.plot,
#        height = 4,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/microtopgraphy_map.pdf',
#        mtopo.change.plot,
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


# # Extract microtopography at ec tower slices (using my methods for microtopography)
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
# st_write(mtopo_sf, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tower_mtopo15_extract.shp')
mtopo_sf <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tower_mtopo15_extract.shp') %>%
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/mtopo_alt_plot_v2.jpg',
#        height = 4,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/mtopo_alt_plot_v2.pdf',
#        height = 4,
#        width = 6)
########################################################################################################################

### Thermokarst Microtopography Analysis ###############################################################################
### Join microtopography and thermokarst
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
  scale_x_continuous(name = 'Longitude (m)',
                     breaks = c(389200, 389400, 389600)) +
  scale_y_continuous(name = 'Latitude (m)',
                     breaks = c(7085400, 7085600, 7085800)) +
  scale_color_viridis(name = 'Thermokarst (%)',
                      direction = -1) +
  scale_fill_viridis(name = 'Thermokarst (%)',
                     direction = -1) +
  theme_bw() +
  theme(legend.position = 'bottom')
ec_karst_plot

ec_mtopo_plot <- ggplot() +
  geom_sf(data = karst_mtopo_sf, aes(color = mtopo15.sd, fill = mtopo15.sd)) +
  coord_sf(datum = st_crs(32606)) +
  scale_x_continuous(name = 'Longitude (m)',
                     breaks = c(389200, 389400, 389600)) +
  scale_y_continuous(name = 'Latitude (m)',
                     breaks = c(7085400, 7085600, 7085800)) +
  scale_color_viridis(name = 'Roughness (m)',
                      direction = -1) +
  scale_fill_viridis(name = 'Roughness (m)',
                     direction = -1) +
  theme_bw() +
  theme(legend.position = 'bottom')
ec_mtopo_plot

ec_karst_mtopo_class <- ggplot(karst_mtopo_sf, aes(x = percent.thermokarst, y = mtopo15.sd)) +
  geom_point(aes(color = color.group.2)) +
  theme_bw()
ec_karst_mtopo_class

tk_roughness_model <- gam(mtopo15.sd ~ percent.thermokarst,data = karst_mtopo_sf)
# saveRDS(tk_roughness_model, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tk_roughness.rds')
tk_roughness_model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/tk_roughness.rds')
summary(tk_roughness_model)

ec_karst_mtopo_cont <- ggplot(karst_mtopo_sf, aes(x = percent.thermokarst, y = mtopo15.sd)) +
  geom_point(aes(color = direction)) +
  geom_smooth(method = 'gam',
              formula = y ~ s(x, bs = "cs"),
              color = 'black') +
  scale_x_continuous(name = 'Thermokarst\n(%)') +
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
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_roughness.jpg',
#        karst_mtopo,
#        height = 6.5,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_roughness.pdf',
#        karst_mtopo,
#        height = 6.5,
#        width = 6.5)

# try to figure out why the mtopo values always have 999 or 000 in decimal places 3-5
# # I guess the raw LiDAR elevation somehow is only actually significant to 2 decimal places. Why?
# values_df <- data.frame(elev = getValues(elev[[1]]), median = getValues(median15))
# values_df <- mutate(values_df, diff = elev - median)
########################################################################################################################

### CO2 Analysis
### Prep Data ##########################################################################################################
# use data that we have processed and filtered as we want rather than as Ameriflux wants
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/AK17_CO2&CH4_30Apr2019.Rdata')
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_CO2&CH4_30Apr2019.Rdata')
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_CO2&CH4.Rdata')

load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/AK17_Carbon_new_30Apr2019.Rdata')
carbon.17 <- export
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_Carbon_new_30Apr2019.Rdata')
carbon.18 <- export
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_Carbon.Rdata')
carbon.19 <- export
rm(export)

new <- rbind(Tower17.18, Tower18.19[, `:=` (u_var = NULL, v_var = NULL, w_var = NULL)], Tower19.20)
new <- data.table(new)
new <- unique(new)
new[, 46 := NULL]
rm(Tower17.18, Tower18.19, Tower19.20)

carbon <- rbind(carbon.17, carbon.18[, `:=` (u_var = NULL, v_var = NULL, w_var = NULL)], carbon.19)
rm(carbon.17, carbon.18, carbon.19)
carbon <- data.table(carbon)
carbon <- unique(carbon)
carbon <- carbon[, .(ts, NEP, GEP, Reco)]

co2 <- merge(new, carbon, by = c('ts', 'NEP'))
rm(new, carbon)

# Filter to get correct times
co2 <- co2 %>%
  # mutate(ts = parse_date_time(TIMESTAMP_END, orders = c('Y!m!*d!H!M!'))) %>%
  filter(ts >= parse_date_time('2017-05-01 00:00', orders = c('Y!-m!*-d! H!:M!')) &
           ts < parse_date_time('2020-05-01 00:00', orders = c('Y!-m!*-d! H!:M!')))

# Select subset of columns that might be useful and format
co2.small <- co2 %>%
  mutate(filled = ifelse(is.na(co2_flux_filter),
                         1,
                         0)) %>%
  select(ts, NEP, GEP, Reco, wind_speed_filter, wind_dir, tair, Ts_20_KT_Avg,
         TS_1_1_1, TS_5_1_1, SWC_1_1_1, SWC_2_1_1, PAR_filter, VPD, filled) %>%
  mutate(GEP = -1*GEP,
         direction = ceiling(wind_dir),
         month = month(ts),
         doy = yday(ts),
         group = ifelse(PAR_filter >= 10 & (month >= 5 & month <= 9 | (month == 4 | month == 10) & GEP > 0),
                        'GS Day',
                        ifelse(PAR_filter < 10 & month >= 5 & month <= 9,
                               'GS Night',
                               ifelse(month <= 4 | month >= 10,
                                      'NGS',
                                      NA))),
         year = ifelse(month >= 5,
                       year(ts),
                       ifelse(month < 5,
                              year(ts) - 1,
                              NA)),
         month.factor = factor(month),
         mean.swc = ifelse(!is.na(SWC_1_1_1) & !is.na(SWC_2_1_1),
                           (SWC_1_1_1 + SWC_2_1_1)/2,
                           ifelse(!is.na(SWC_1_1_1),
                                  SWC_1_1_1,
                                  ifelse(!is.na(SWC_2_1_1),
                                         SWC_2_1_1,
                                         NA))),
         mean.ts.10 = ifelse(!is.na(TS_1_1_1) & !is.na(TS_5_1_1),
                             (TS_1_1_1 + TS_5_1_1)/2,
                             ifelse(!is.na(TS_1_1_1),
                                    TS_1_1_1,
                                    ifelse(!is.na(TS_5_1_1),
                                           TS_5_1_1,
                                           NA)))) %>%
  filter(!is.na(wind_dir)) %>%
  select(ts, year, month, month.factor, doy, group, NEP, GEP, Reco, WS = wind_speed_filter,
         WD = wind_dir, direction, tair, tsoil = Ts_20_KT_Avg, mean.ts.10, mean.swc,
         PAR = PAR_filter, VPD, filled)

# check that group assignment went as expected
ggplot(co2.small, aes(x = ts, y = NEP, color = group)) +
  geom_point()

# ensure there are no duplicate rows
co2.small <- unique(co2.small)
rm(co2)

# check distribution of fluxes across wind directions in different groups
# full screen this one, otherwise certain bars will not be visible, making it
ggplot(co2.small, aes(x = direction, group = group, fill = group)) +
  geom_bar(position = position_dodge())

# combine karst and roughness data for the ec tower
filenames <- list.files('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/flux_tower_footprint',
                        full.names = TRUE,
                        pattern = '^ffp_20.+csv')
karst_roughness <- map_dfr(filenames,
                           ~ fread(.x))
karst_roughness[, ts := ymd_hm(paste(paste(yyyy, mm, day, sep = '/'), paste(HH, MM, sep = ':')))]
karst_roughness <- karst_roughness[, .(ts, wind_dir, karst.pc, sd.mtopo)]
karst_roughness[, wind_dir_round := round(wind_dir, 2)]
karst_roughness[, karst.pc_round := round(karst.pc, 2)]
karst_roughness[, sd.mtopo_round := round(sd.mtopo, 2)]
test <- karst_roughness[, .N, by = 'ts']
# View(test[N > 1])
# View(test[N == 1])
duplicates <- karst_roughness[karst_roughness$ts %in% test[N > 1,]$ts,]
# View(duplicates[order(ts)])
karst_roughness <- unique(karst_roughness, by = c('ts', 'wind_dir_round', 'karst.pc_round', 'sd.mtopo_round'))
test <- karst_roughness[, .N, by = 'ts']
duplicates <- karst_roughness[karst_roughness$ts %in% test[N > 1,]$ts,]
# View(duplicates[order(ts)])
karst_roughness <- karst_roughness[, N := .N, by = 'ts']
karst_roughness <- karst_roughness[!(is.na(wind_dir & N == 2)),]
karst_roughness <- karst_roughness[, N := .N, by = 'ts']
all(karst_roughness$N == 1) # should be TRUE
karst_roughness[, N := NULL]
karst_roughness[, wind_dir_round := NULL]
karst_roughness[, karst.pc_round := NULL]
karst_roughness[, sd.mtopo_round := NULL]
rm(test, duplicates)

# Join co2 and karst/roughness data
co2.model.data <- co2.small %>%
  full_join(karst_roughness, by = 'ts') %>%
  rename(percent.thermokarst.ffp = karst.pc, mtopo15.sd.ffp = sd.mtopo) %>%
  as.data.table() %>%
  mutate(GEP = -1*GEP, # make GPP negative so that source/sink sides of graphs will be consistent
         direction.factor = case_when(
           direction <= 23 | direction >= 338 ~ 'N',
           direction >= 24 & direction <= 68 ~ 'NW',
           direction >= 69 & direction <= 113 ~ 'W',
           direction >= 114 & direction <= 157 ~ 'SW',
           direction >= 158 & direction <= 202 ~ 'S',
           direction >= 203 & direction <= 247 ~ 'SE',
           direction >= 248 & direction <= 292 ~ 'E',
           direction >= 293 & direction <= 337 ~ 'NE'
         ))
rm(co2.small)

# arrange data in chronological order
co2.model.data <- co2.model.data[order(ts)]

# check that the join got the right timestamps
test <- co2.model.data[!is.na(wind_dir), offset := round(WD, 2) - round(wind_dir, 2)]
test <- test[!is.na(offset) & round(offset, 4) != 0, ] # should have 0 rows
rm(test)

# remove duplicate/unneeded wind rows
co2.model.data[, wind_dir := NULL]
co2.model.data[, offset := NULL]

# remove filled data
co2.model.data <- co2.model.data[filled == 0]

# subset GS Day or Non-Growing Season/Night
co2.gs.day <- co2.model.data[group == 'GS Day']
co2.ngs.night <- co2.model.data[group != 'GS Day']
########################################################################################################################

### Model C Fluxes with Thermokarst ####################################################################################
# # Non-normality of errors should not impact the model itself, only the determination of statistical significance
# # of parameters via p-values. Can use bootstrapping instead (which is what we normally do for mixed effects models anyway).
# # See: https://data.library.virginia.edu/normality-assumption/

### NEE
# All times
nee.karst.model <- lm(NEP ~ percent.thermokarst.ffp,
                      data = co2.model.data)
# saveRDS(nee.karst.model,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_simple.rds')
nee.karst.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_simple.rds')
summary(nee.karst.model)
r2.label <- expression('R'^2 ~ '== ' ~ summary(nee.karst.model)$r.squared)
nee.karst.plot <- ggplot(co2.model.data,
                         aes(x = percent.thermokarst.ffp,
                             y = NEP,
                             color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(color = 'black',
              fill = 'gray',
              method = 'lm') +
  geom_text(aes(x = 0.3, y = 0.2,
                label = paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(summary(nee.karst.model)$r.squared, 4))),
            inherit.aes = FALSE,
            parse = TRUE,
            size = 3,
            vjust = 'outward',
            hjust = 'outward') +
  geom_text(aes(x = 0.295, y = 0.17,
                label = paste0('Slope = ', round(summary(nee.karst.model)$coefficients[2, 1], 3))),
            inherit.aes = FALSE,
            size = 3,
            vjust = 'outward',
            hjust = 'outward') +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      option = 'B',
                      discrete = TRUE) +
  theme_bw() +
  theme(legend.title = element_blank())
nee.karst.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_karst_plot.jpg',
#        nee.karst.plot,
#        height = 4,
#        width = 4)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_karst_plot.pdf',
#        nee.karst.plot,
#        height = 4,
#        width = 4)

# NGS/Night
nee.karst.plot.ngs <- ggplot(co2.ngs.night,
                   aes(x = percent.thermokarst.ffp,
                       y = NEP,
                       group = month.factor,
                       color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  geom_smooth(aes(fill = month.factor),
              method = 'lm') +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      option = 'B',
                      discrete = TRUE) +
  scale_fill_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      option = 'B',
                      discrete = TRUE) +
  theme_bw() +
  theme(legend.title = element_blank())
nee.karst.plot.ngs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_karst_plot_ngs.jpg',
#        nee.karst.plot.ngs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_karst_plot_ngs.pdf',
#        nee.karst.plot.ngs,
#        height = 6,
#        width = 6)

# simple model - NGS/Night
smallest <- NEP ~ 1
biggest <- NEP ~ percent.thermokarst.ffp*month.factor
start <- lm(NEP ~ percent.thermokarst.ffp*month.factor,
            data = co2.ngs.night)

stats::step(start, scope = list(lower = smallest, upper = biggest))

nee.karst.model.ngs <- lm(NEP ~ percent.thermokarst.ffp*month.factor,
                          data = co2.ngs.night)
# saveRDS(nee.karst.model.ngs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_simple_ngs.rds')
nee.karst.model.ngs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_simple_ngs.rds')
summary(nee.karst.model.ngs)
nee.contrast <- emmeans(nee.karst.model.ngs, specs = pairwise~month.factor)
nee.contrast
shapiro.test(sample(rstandard(nee.karst.model.ngs), 5000))

# check model residuals of model
# look at residuals
nee.karst.model.ngs.resid <- resid(nee.karst.model.ngs)
nee.karst.model.ngs.fitted <- fitted(nee.karst.model.ngs)
nee.karst.model.ngs.sqrt <- sqrt(abs(resid(nee.karst.model.ngs)))

# graph
hist(nee.karst.model.ngs$residuals)
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(nee.karst.model.ngs.fitted, nee.karst.model.ngs.resid, main='resid, nee.karst.model.ngs')
plot(nee.karst.model.ngs.fitted, nee.karst.model.ngs.sqrt, main='sqrt resid, nee.karst.model.ngs')
qqnorm(nee.karst.model.ngs.resid, main = 'nee.karst.model.ngs')
qqline(nee.karst.model.ngs.resid)
plot(co2.ngs.night$direction, nee.karst.model.ngs.resid) # inspect whether there
# is spatial autocorrelation in model residuals
par(mfrow=c(1,1))

# # this is a gross way to summarize and make everything look nice - better options?
nee.karst.model.ngs.ci <- extract_ci_lm(nee.karst.model.ngs)
nee.karst.model.ngs.table <- nee.karst.model.ngs.ci %>%
  mutate(`Final Variables` = str_replace(term, 'percent.thermokarst.ffp', 'TK'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 1 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^TK$', 'TK:Month - 1'),
         radius = coefs - min,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope'),
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefs - first(coefs) == 0,
                              round(coefs, 5),
                              round(first(coefs) + coefs, 5)),
         total.radius = ifelse(coefs - first(coefs) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('NEE', rep('', 23)),
         `Full Model` = c('TK*Group', rep('', 23)),
         R2 = c(round(summary(nee.karst.model.ngs)$r.squared, 3), rep('', 23))) %>%
  arrange(coefficient.type, month) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)
# write.csv(nee.karst.model.ngs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_table_ngs.csv',
# row.names = FALSE)


### NEE
# GS Day
nee.karst.plot.gs <- ggplot(co2.gs.day,
                         aes(x = percent.thermokarst.ffp,
                             y = NEP,
                             color = month.factor,
                             group = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(4, 10),
                      labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                      discrete = TRUE,
                      option = 'B',
                      begin = 1/3,
                      end = 5/6) +
  scale_fill_viridis(breaks = seq(4, 10),
                     labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                     discrete = TRUE,
                     option = 'B',
                     begin = 1/3,
                     end = 5/6) +
  theme_bw() +
  theme(legend.title = element_blank())
nee.karst.plot.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_karst_plot_gs.jpg',
#        nee.karst.plot.gs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_karst_plot_gs.pdf',
#        nee.karst.plot.gs,
#        height = 6,
#        width = 6)

# simple model - GS Day
smallest <- NEP ~ 1
biggest <- NEP ~ percent.thermokarst.ffp*month.factor
start <- lm(NEP ~ percent.thermokarst.ffp*month.factor,
            data = co2.gs.day)

stats::step(start, scope = list(lower = smallest, upper = biggest))

nee.karst.model.gs <- lm(NEP ~ percent.thermokarst.ffp*month.factor,
                      data = co2.gs.day)
# saveRDS(nee.karst.model.gs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_simple_gs.rds')
nee.karst.model.gs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_simple_gs.rds')
summary(nee.karst.model.gs)
nee.contrast <- emmeans(nee.karst.model.gs, specs = pairwise~month.factor)
nee.contrast
shapiro.test(sample(rstandard(nee.karst.model.gs), 5000))

# check model residuals of model2
# look at residuals
nee.karst.model.gs.resid <- resid(nee.karst.model.gs)
nee.karst.model.gs.fitted <- fitted(nee.karst.model.gs)
nee.karst.model.gs.sqrt <- sqrt(abs(resid(nee.karst.model.gs)))

# graph
hist(nee.karst.model.gs$residuals)
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(nee.karst.model.gs.fitted, nee.karst.model.gs.resid, main='resid, nee.karst.model.gs')
plot(nee.karst.model.gs.fitted, nee.karst.model.gs.sqrt, main='sqrt resid, nee.karst.model.gs')
qqnorm(nee.karst.model.gs.resid, main = 'nee.karst.model.gs')
qqline(nee.karst.model.gs.resid)
plot(co2.gs.day$direction, nee.karst.model.gs.resid)
par(mfrow=c(1,1))

# # this is a gross way to summarize and make everything look nice - better options?
nee.karst.model.gs.ci <- extract_ci_lm(nee.karst.model.gs)
nee.karst.model.gs.table <- nee.karst.model.gs.ci %>%
  mutate(`Final Variables` = str_replace(term, 'percent.thermokarst.ffp', 'TK'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 4'),
         `Final Variables` = str_replace(`Final Variables`, '^month.factor', 'Month - '),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^TK$', 'TK:Month - 4'),
         radius = coefs - min,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope'),
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefs - first(coefs) == 0,
                              round(coefs, 5),
                              round(first(coefs) + coefs, 5)),
         total.radius = ifelse(coefs - first(coefs) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('NEE', rep('', 13)),
         `Full Model` = c('TK*Month', rep('', 13)),
         R2 = c(round(summary(nee.karst.model.gs)$r.squared, 3), rep('', 13))) %>%
  arrange(coefficient.type, month) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)
# write.csv(nee.karst.model.gs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_table_gs.csv',
# row.names = FALSE)


# GPP
# GS Day
gpp.karst.plot.gs <- ggplot(co2.gs.day[GEP <= 0],
                   aes(x = percent.thermokarst.ffp,
                       y = GEP,
                       color = month.factor,
                       group = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm',
              aes(fill = month.factor)) +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Gross Primary Production' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(4, 10),
                      labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                      discrete = TRUE,
                      option = 'B',
                      begin = 1/3,
                      end = 5/6) +
  scale_fill_viridis(breaks = seq(4, 10),
                     labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                     discrete = TRUE,
                     option = 'B',
                     begin = 1/3,
                     end = 5/6) +
  theme_bw() +
  theme(legend.title = element_blank())
gpp.karst.plot.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/gpp_karst_plot_gs.jpg',
#        gpp.karst.plot.gs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/gpp_karst_plot_gs.pdf',
#        gpp.karst.plot.gs,
#        height = 6,
#        width = 6)

# simple model - GS Day
smallest <- GEP ~ 1
biggest <- GEP ~ percent.thermokarst.ffp*month.factor
start <- lm(GEP ~ percent.thermokarst.ffp*month.factor,
            data = co2.gs.day[GEP <= 0])

stats::step(start, scope = list(lower = smallest, upper = biggest))

gpp.karst.model.gs <- lm(GEP ~ percent.thermokarst.ffp*month.factor,
                 data = co2.gs.day[GEP <= 0])
# saveRDS(gpp.karst.model.gs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_karst_model_simple_gs.rds')
gpp.karst.model.gs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_karst_model_simple_gs.rds')
summary(gpp.karst.model.gs)
gpp.contrast <- emmeans(gpp.karst.model.gs, specs = pairwise~month.factor)
gpp.contrast
shapiro.test(sample(rstandard(gpp.karst.model.gs), 5000))

# check model residuals of model2
# look at residuals
gpp.karst.model.gs.resid <- resid(gpp.karst.model.gs)
gpp.karst.model.gs.fitted <- fitted(gpp.karst.model.gs)
gpp.karst.model.gs.sqrt <- sqrt(abs(resid(gpp.karst.model.gs)))

# graph
hist(gpp.karst.model.gs$residuals)
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(gpp.karst.model.gs.fitted, gpp.karst.model.gs.resid, main='resid, gpp.karst.model.gs')
plot(gpp.karst.model.gs.fitted, gpp.karst.model.gs.sqrt, main='sqrt resid, gpp.karst.model.gs')
qqnorm(gpp.karst.model.gs.resid, main = 'gpp.karst.model.gs')
qqline(gpp.karst.model.gs.resid)
plot(co2.gs.day[GEP <= 0]$direction, gpp.karst.model.gs.resid)
par(mfrow=c(1,1))

# this is a gross way to summarize and make everything look nice - better options?
gpp.karst.model.gs.ci <- extract_ci_lm(gpp.karst.model.gs)
gpp.karst.model.gs.table <- gpp.karst.model.gs.ci %>%
  mutate(`Final Variables` = str_replace(term, 'percent.thermokarst.ffp', 'TK'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 4 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'), 
                                    str_c(`Final Variables`, ' (Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^TK$', 'TK:Month - 4'),
         radius = coefs - min,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope'),
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefs - first(coefs) == 0,
                              round(coefs, 5),
                              round(first(coefs) + coefs, 5)),
         total.radius = ifelse(coefs - first(coefs) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('GPP', rep('', 13)),
         `Full Model` = c('TK*Month', rep('', 13)),
         R2 = c(round(summary(gpp.karst.model.gs)$r.squared, 3), rep('', 13))) %>%
  arrange(coefficient.type, month) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)
# write.csv(gpp.karst.model.gs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_karst_model_table_gs.csv',
# row.names = FALSE)


### Reco
# NGS/Night
reco.karst.plot.ngs <- ggplot(co2.ngs.night[Reco >= 0],
                    aes(x = percent.thermokarst.ffp,
                        y = Reco,
                        color = month.factor,
                        group = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Ecosystem Respiration' ~ ('g C' ~ 'm'^-2 ~ 's'^-2))) +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      option = 'B',
                      discrete = TRUE) +
  scale_fill_viridis(breaks = seq(1, 12),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                     option = 'B',
                     discrete = TRUE) +
  theme_bw() +
  theme(legend.title = element_blank())
reco.karst.plot.ngs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_karst_plot_ngs.jpg',
#        reco.karst.plot.ngs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_karst_plot_ngs.pdf',
#        reco.karst.plot.ngs,
#        height = 6,
#        width = 6)

# simple model - NGS/Night
smallest <- Reco ~ 1
biggest <- Reco ~ percent.thermokarst.ffp*month.factor
start <- lm(Reco ~ percent.thermokarst.ffp*month.factor,
            data = co2.ngs.night[Reco >= 0])

stats::step(start, scope = list(lower = smallest, upper = biggest))

reco.karst.model.ngs <- lm(Reco ~ percent.thermokarst.ffp*month.factor,
                 data = co2.ngs.night[Reco >= 0])
# saveRDS(reco.karst.model.ngs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_simple_ngs.rds')
reco.karst.model.ngs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_simple_ngs.rds')
summary(reco.karst.model.ngs)
reco.contrast <- emmeans(reco.karst.model.ngs, specs = pairwise~month.factor)
reco.contrast
shapiro.test(sample(rstandard(nee.karst.model.ngs), 5000))

# check model residuals of model2
# look at residuals
reco.karst.model.ngs.resid <- resid(reco.karst.model.ngs)
reco.karst.model.ngs.fitted <- fitted(reco.karst.model.ngs)
reco.karst.model.ngs.sqrt <- sqrt(abs(resid(reco.karst.model.ngs)))

# graph
hist(reco.karst.model.ngs$residuals)
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(reco.karst.model.ngs.fitted, reco.karst.model.ngs.resid, main='resid, reco.karst.model.ngs')
plot(reco.karst.model.ngs.fitted, reco.karst.model.ngs.sqrt, main='sqrt resid, reco.karst.model.ngs')
qqnorm(reco.karst.model.ngs.resid, main = 'reco.karst.model.ngs')
qqline(reco.karst.model.ngs.resid)
plot(co2.ngs.night[Reco >= 0]$direction, reco.karst.model.ngs.resid)
par(mfrow=c(1,1))

# this is a gross way to summarize and make everything look nice - better options?
reco.karst.model.ngs.ci <- extract_ci_lm(reco.karst.model.ngs)
reco.karst.model.ngs.table <- reco.karst.model.ngs.ci %>%
  mutate(`Final Variables` = str_replace(term, 'percent.thermokarst.ffp', 'TK'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 1 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^TK$', 'TK:Month - 1'),
         radius = coefs - min,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope'),
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefs - first(coefs) == 0,
                              round(coefs, 5),
                              round(first(coefs) + coefs, 5)),
         total.radius = ifelse(coefs - first(coefs) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('Reco', rep('', 23)),
         `Full Model` = c('TK*Group', rep('', 23)),
         R2 = c(round(summary(reco.karst.model.ngs)$r.squared, 3), rep('', 23))) %>%
  arrange(coefficient.type, month) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)
# write.csv(reco.karst.model.ngs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_table_ngs.csv',
#           row.names = FALSE)


### Reco
# GS Day
reco.karst.plot.gs <- ggplot(co2.gs.day[Reco >= 0],
                              aes(x = percent.thermokarst.ffp,
                                  y = Reco,
                                  color = month.factor,
                                  group = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('Ecosystem Respiration' ~ ('g C' ~ 'm'^-2 ~ 's'^-2))) +
  scale_color_viridis(breaks = seq(4, 10),
                      labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                      discrete = TRUE,
                      option = 'B',
                      begin = 1/3,
                      end = 5/6) +
  scale_fill_viridis(breaks = seq(4, 10),
                     labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                     discrete = TRUE,
                     option = 'B',
                     begin = 1/3,
                     end = 5/6) +
  theme_bw() +
  theme(legend.title = element_blank())
reco.karst.plot.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_karst_plot_gs.jpg',
#        reco.karst.plot.gs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_karst_plot_gs.pdf',
#        reco.karst.plot.gs,
#        height = 6,
#        width = 6)

# simple model - GS Day
smallest <- Reco ~ 1
biggest <- Reco ~ percent.thermokarst.ffp*month.factor
start <- lm(Reco ~ percent.thermokarst.ffp*month.factor,
            data = co2.gs.day[Reco >= 0])

stats::step(start, scope = list(lower = smallest, upper = biggest))

reco.karst.model.gs <- lm(Reco ~ percent.thermokarst.ffp*month.factor,
                         data = co2.gs.day[Reco >= 0])
# saveRDS(reco.karst.model.gs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_simple_gs.rds')
reco.karst.model.gs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_simple_gs.rds')
summary(reco.karst.model.gs)
reco.contrast <- emmeans(reco.karst.model.gs, specs = pairwise~month.factor)
reco.contrast
shapiro.test(sample(rstandard(reco.karst.model.gs), 5000))

# check model residuals of model2
# look at residuals
reco.karst.model.gs.resid <- resid(reco.karst.model.gs)
reco.karst.model.gs.fitted <- fitted(reco.karst.model.gs)
reco.karst.model.gs.sqrt <- sqrt(abs(resid(reco.karst.model.gs)))

# graph
hist(reco.karst.model.gs$residuals)
par(mfrow=c(2,2), mar = c(4,4,3,2))
plot(reco.karst.model.gs.fitted, reco.karst.model.gs.resid, main='resid, reco.karst.model.gs')
plot(reco.karst.model.gs.fitted, reco.karst.model.gs.sqrt, main='sqrt resid, reco.karst.model.gs')
qqnorm(reco.karst.model.gs.resid, main = 'reco.karst.model.gs')
qqline(reco.karst.model.gs.resid)
plot(co2.gs.day[Reco >= 0]$direction, reco.karst.model.gs.resid)
par(mfrow=c(1,1))

# this is a gross way to summarize and make everything look nice - better options?
reco.karst.model.gs.ci <- extract_ci_lm(reco.karst.model.gs)
reco.karst.model.gs.table <-reco.karst.model.gs.ci %>%
  mutate(`Final Variables` = str_replace(term, 'percent.thermokarst.ffp', 'TK'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 4'),
         `Final Variables` = str_replace(`Final Variables`, '^month.factor', 'Month - '),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^TK$', 'TK:Month - 4'),
         radius = coefs - min,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope'),
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefs - first(coefs) == 0,
                              round(coefs, 5),
                              round(first(coefs) + coefs, 5)),
         total.radius = ifelse(coefs - first(coefs) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('Reco', rep('', 13)),
         `Full Model` = c('TK*Month', rep('', 13)),
         R2 = c(round(summary(reco.karst.model.gs)$r.squared, 3), rep('', 13))) %>%
  arrange(coefficient.type, month) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)
# write.csv(reco.karst.model.gs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_table_gs.csv',
#           row.names = FALSE)
########################################################################################################################

### Model C Fluxes with Roughness ######################################################################################
### NEE
# NGS/Night
nee.roughness.plot.ngs <- ggplot(co2.ngs.night,
                             aes(x = mtopo15.sd.ffp,
                                 y = NEP,
                                 color = month.factor,
                                 month.factor = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Roughness (m)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      option = 'B',
                      discrete = TRUE) +
  scale_fill_viridis(breaks = seq(1, 12),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                     option = 'B',
                     discrete = TRUE) +
  theme_bw() +
  theme(legend.title = element_blank())
nee.roughness.plot.ngs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_roughness_plot_ngs.jpg',
#        nee.roughness.plot.ngs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_roughness_plot_ngs.pdf',
#        nee.roughness.plot.ngs,
#        height = 6,
#        width = 6)

# simple model - NGS/Night
smallest <- NEP ~ 1
biggest <- NEP ~ mtopo15.sd.ffp*month.factor
start <- lm(NEP ~ mtopo15.sd.ffp*month.factor,
            data = co2.ngs.night)

stats::step(start, scope = list(lower = smallest, upper = biggest))

nee.roughness.model.ngs <- lm(NEP ~ mtopo15.sd.ffp*month.factor,
                              data = co2.ngs.night)
# saveRDS(nee.roughness.model.ngs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_simple_ngs.rds')
nee.roughness.model.ngs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_simple_ngs.rds')
summary(nee.roughness.model.ngs)
nee.contrast <- emmeans(nee.roughness.model.ngs, specs = pairwise~month.factor)
nee.contrast

# this is a gross way to summarize and make everything look nice - better options?
nee.roughness.model.ngs.table <- data.frame(variables = names(nee.roughness.model.ngs[['coefficients']]),
                                            coefficient = nee.roughness.model.ngs[['coefficients']],
                                            min.ci = as.numeric(confint(nee.roughness.model.ngs)[,1]),
                                            max.ci = as.numeric(confint(nee.roughness.model.ngs)[,2]),
                                            row.names = NULL) %>%
  mutate(`Final Variables` = str_replace(variables, 'mtopo15.sd.ffp', 'Roughness'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 1 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^Roughness$', 'Roughness:Month - 1'),
         radius = coefficient - min.ci,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope')) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefficient - first(coefficient) == 0,
                              round(coefficient, 5),
                              round(first(coefficient) + coefficient, 5)),
         total.radius = ifelse(coefficient - first(coefficient) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('NEE', rep('', 23)),
         `Full Model` = c('Roughness*Group', rep('', 23)),
         R2 = c(round(summary(nee.roughness.model.ngs)$r.squared, 3), rep('', 23))) %>%
  arrange(coefficient.type) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)

# write.csv(nee.roughness.model.ngs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_table_ngs.csv',
#           row.names = FALSE)

### NEE
# GS Day only
nee.roughness.plot.gs <- ggplot(co2.gs.day,
                            aes(x = mtopo15.sd.ffp,
                                y = NEP,
                                color = month.factor,
                                group = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Roughness (m)') +
  scale_y_continuous(name = expression('Net Ecosystem Exchange' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(4, 10),
                      labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                      discrete = TRUE,
                      option = 'B',
                      begin = 1/3,
                      end = 5/6) +
  scale_fill_viridis(breaks = seq(4, 10),
                     labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                     discrete = TRUE,
                     option = 'B',
                     begin = 1/3,
                     end = 5/6) +
  theme_bw() +
  theme(legend.title = element_blank())
nee.roughness.plot.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_roughness_plot_gs.jpg',
#        nee.roughness.plot.gs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/nee_roughness_plot_gs.pdf',
#        nee.roughness.plot.gs,
#        height = 6,
#        width = 6)

# simple model - GS Day
smallest <- NEP ~ 1
biggest <- NEP ~ mtopo15.sd.ffp*month.factor
start <- lm(NEP ~ mtopo15.sd.ffp*month.factor,
            data = co2.gs.day)

stats::step(start, scope = list(lower = smallest, upper = biggest))

nee.roughness.model.gs <- lm(NEP ~ mtopo15.sd.ffp*month.factor,
                         data = co2.gs.day)
# saveRDS(nee.roughness.model.gs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_simple_gs.rds')
nee.roughness.model.gs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_simple_gs.rds')
summary(nee.roughness.model.gs)
nee.contrast <- emmeans(nee.roughness.model.gs, specs = pairwise~month.factor)
nee.contrast

# this is a gross way to summarize and make everything look nice - better options?
nee.roughness.model.gs.table <- data.frame(variables = names(nee.roughness.model.gs[['coefficients']]),
                                       coefficient = nee.roughness.model.gs[['coefficients']],
                                       min.ci = as.numeric(confint(nee.roughness.model.gs)[,1]),
                                       max.ci = as.numeric(confint(nee.roughness.model.gs)[,2]),
                                       row.names = NULL) %>%
  mutate(`Final Variables` = str_replace(variables, 'mtopo15.sd.ffp', 'Roughness'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 4'),
         `Final Variables` = str_replace(`Final Variables`, '^month.factor', 'Month - '),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^Roughness$', 'Roughness:Month - 4'),
         radius = coefficient - min.ci,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope')) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefficient - first(coefficient) == 0,
                              round(coefficient, 5),
                              round(first(coefficient) + coefficient, 5)),
         total.radius = ifelse(coefficient - first(coefficient) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('NEE', rep('', 13)),
         `Full Model` = c('Roughness*Month', rep('', 13)),
         R2 = c(round(summary(nee.roughness.model.gs)$r.squared, 3), rep('', 13))) %>%
  arrange(coefficient.type) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)

# write.csv(nee.roughness.model.gs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_table_gs.csv',
#           row.names = FALSE)

### GPP
# GS Day
gpp.roughness.plot.gs <- ggplot(co2.gs.day[GEP <= 0],
       aes(x = mtopo15.sd.ffp,
           y = GEP,
           group = month.factor,
           color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm',
              aes(fill = month.factor)) +
  scale_x_continuous(name = 'Roughness (m)') +
  scale_y_continuous(name = expression('Gross Primary Production' ~ ('g C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(4, 10),
                      labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                      discrete = TRUE,
                      option = 'B',
                      begin = 1/3,
                      end = 5/6) +
  scale_fill_viridis(breaks = seq(4, 10),
                     labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                     discrete = TRUE,
                     option = 'B',
                     begin = 1/3,
                     end = 5/6) +
  theme_bw() +
  theme(legend.title = element_blank())
gpp.roughness.plot.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/gpp_roughness_plot_gs.jpg',
#        gpp.roughness.plot.gs,
#        height = 6,
#        width = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/gpp_roughness_plot_gs.pdf',
#        gpp.roughness.plot.gs,
#        height = 6,
#        width = 6)

### GPP
# simple model - GS Day
smallest <- GEP ~ 1
biggest <- GEP ~ mtopo15.sd.ffp*month.factor
start <- lm(GEP ~ mtopo15.sd.ffp*month.factor,
            data = co2.gs.day[GEP <= 0])

stats::step(start, scope = list(lower = smallest, upper = biggest))

gpp.roughness.model.gs <- lm(GEP ~ mtopo15.sd.ffp*month.factor,
                      data = co2.gs.day[GEP <= 0])
# saveRDS(gpp.roughness.model.gs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_roughness_model_simple_gs.rds')
gpp.roughness.model.gs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_roughness_model_simple_gs.rds')
summary(gpp.roughness.model.gs)
gpp.roughness.contrast <- emmeans(gpp.roughness.model.gs, specs = pairwise~month.factor)
gpp.roughness.contrast

# this is a gross way to summarize and make everything look nice - better options?
gpp.roughness.model.gs.table <- data.frame(variables = names(gpp.roughness.model.gs[['coefficients']]),
                                    coefficient = gpp.roughness.model.gs[['coefficients']],
                                    min.ci = as.numeric(confint(gpp.roughness.model.gs)[,1]),
                                    max.ci = as.numeric(confint(gpp.roughness.model.gs)[,2]),
                                    row.names = NULL) %>%
  mutate(`Final Variables` = str_replace(variables, 'mtopo15.sd.ffp', 'Roughness'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 4 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'), 
                                    str_c(`Final Variables`, ' (Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^Roughness$', 'Roughness:Month - 4'),
         radius = coefficient - min.ci,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope')) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefficient - first(coefficient) == 0,
                              round(coefficient, 5),
                              round(first(coefficient) + coefficient, 5)),
         total.radius = ifelse(coefficient - first(coefficient) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('GPP', rep('', 13)),
         `Full Model` = c('Roughness*Month', rep('', 13)),
         R2 = c(round(summary(gpp.roughness.model.gs)$r.squared, 3), rep('', 13))) %>%
  arrange(coefficient.type) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)

# write.csv(gpp.roughness.model.gs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_roughness_model_table_gs.csv',
#           row.names = FALSE)

### Reco
# NGS/Night
reco.roughness.plot.ngs <- ggplot(co2.ngs.night[Reco >= 0],
                          aes(x = mtopo15.sd.ffp,
                              y = Reco,
                              color = month.factor,
                              month.factor = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Roughness (m)') +
  scale_y_continuous(name = expression('Ecosystem Respiration' ~ ('g C' ~ 'm'^-2 ~ 's'^-2))) +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      option = 'B',
                      discrete = TRUE) +
  scale_fill_viridis(breaks = seq(1, 12),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                     option = 'B',
                     discrete = TRUE) +
  theme_bw() +
  theme(legend.title = element_blank())
reco.roughness.plot.ngs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_roughness_plot_ngs.jpg',
#        reco.roughness.plot.ngs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_roughness_plot_ngs.pdf',
#        reco.roughness.plot.ngs,
#        height = 6,
#        width = 6)

### Reco
# simple model - NGS/Night
smallest <- Reco ~ 1
biggest <- Reco ~ mtopo15.sd.ffp*month.factor
start <- lm(Reco ~ mtopo15.sd.ffp*month.factor,
            data = co2.ngs.night[Reco >= 0])

stats::step(start, scope = list(lower = smallest, upper = biggest))

reco.roughness.model.ngs <- lm(Reco ~ mtopo15.sd.ffp*month.factor,
                               data = co2.ngs.night[Reco >= 0])
# saveRDS(reco.roughness.model.ngs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_simple_ngs.rds')
reco.roughness.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_simple_ngs.rds')
summary(reco.roughness.model)

# this is a gross way to summarize and make everything look nice - better options?
reco.roughness.model.ngs.table <- data.frame(variables = names(reco.roughness.model[['coefficients']]),
                                     coefficient = reco.roughness.model[['coefficients']],
                                     min.ci = as.numeric(confint(reco.roughness.model)[,1]),
                                     max.ci = as.numeric(confint(reco.roughness.model)[,2]),
                                     row.names = NULL) %>%
  mutate(`Final Variables` = str_replace(variables, 'mtopo15.sd.ffp', 'Roughness'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 1 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^Roughness$', 'Roughness:Month - 1'),
         radius = coefficient - min.ci,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope')) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefficient - first(coefficient) == 0,
                              round(coefficient, 5),
                              round(first(coefficient) + coefficient, 5)),
         total.radius = ifelse(coefficient - first(coefficient) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('NEE', rep('', 23)),
         `Full Model` = c('Roughness*Group', rep('', 23)),
         R2 = c(round(summary(reco.roughness.model)$r.squared, 3), rep('', 23))) %>%
  arrange(coefficient.type) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)

# write.csv(reco.roughness.model.ngs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_table_ngs.csv',
#           row.names = FALSE)

### Reco
# GS Day
reco.roughness.plot.gs <- ggplot(co2.gs.day[Reco >= 0],
                                  aes(x = mtopo15.sd.ffp,
                                      y = Reco,
                                      color = month.factor,
                                      month.factor = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(aes(fill = month.factor), method = 'lm') +
  scale_x_continuous(name = 'Roughness (m)') +
  scale_y_continuous(name = expression('Ecosystem Respiration' ~ ('g C' ~ 'm'^-2 ~ 's'^-2))) +
  scale_color_viridis(breaks = seq(4, 10),
                      labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                      discrete = TRUE,
                      option = 'B',
                      begin = 1/3,
                      end = 5/6) +
  scale_fill_viridis(breaks = seq(4, 10),
                     labels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                     discrete = TRUE,
                     option = 'B',
                     begin = 1/3,
                     end = 5/6) +
  theme_bw() +
  theme(legend.title = element_blank())
reco.roughness.plot.gs
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_roughness_plot_gs.jpg',
#        reco.roughness.plot.gs,
#        height = 6,
#        width = 6)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/reco_roughness_plot_gs.pdf',
#        reco.roughness.plot.gs,
#        height = 6,
#        width = 6)

# simple model - GS Day
smallest <- Reco ~ 1
biggest <- Reco ~ mtopo15.sd.ffp*month.factor
start <- lm(Reco ~ mtopo15.sd.ffp*month.factor,
            data = co2.gs.day[Reco >= 0])

stats::step(start, scope = list(lower = smallest, upper = biggest))

reco.roughness.model.gs <- lm(Reco ~ mtopo15.sd.ffp*month.factor,
                             data = co2.gs.day[Reco >= 0])
# saveRDS(reco.roughness.model.gs, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_simple_gs.rds')
reco.roughness.model.gs <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_simple_gs.rds')
summary(reco.roughness.model.gs)
reco.contrast <- emmeans(reco.roughness.model.gs, specs = pairwise~month.factor)
reco.contrast

# this is a gross way to summarize and make everything look nice - better options?
reco.roughness.model.gs.table <- data.frame(variables = names(reco.roughness.model.gs[['coefficients']]),
                                           coefficient = reco.roughness.model.gs[['coefficients']],
                                           min.ci = as.numeric(confint(reco.roughness.model.gs)[,1]),
                                           max.ci = as.numeric(confint(reco.roughness.model.gs)[,2]),
                                           row.names = NULL) %>%
  mutate(`Final Variables` = str_replace(variables, 'mtopo15.sd.ffp', 'Roughness'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 4'),
         `Final Variables` = str_replace(`Final Variables`, '^month.factor', 'Month - '),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]'),
                                    paste(`Final Variables`, '(Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^Roughness$', 'Roughness:Month - 4'),
         radius = coefficient - min.ci,
         coefficient.type = ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                   'intercept',
                                   'slope')) %>%
  group_by(coefficient.type) %>%
  mutate(Coefficient = ifelse(coefficient - first(coefficient) == 0,
                              round(coefficient, 5),
                              round(first(coefficient) + coefficient, 5)),
         total.radius = ifelse(coefficient - first(coefficient) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('reco', rep('', 13)),
         `Full Model` = c('Roughness*Month', rep('', 13)),
         R2 = c(round(summary(reco.roughness.model.gs)$r.squared, 3), rep('', 13))) %>%
  arrange(coefficient.type) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2, coefficient.type)

# write.csv(reco.roughness.model.gs.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_table_gs.csv',
#           row.names = FALSE)
########################################################################################################################

### Combine and Plot Models ############################################################################################
### All comparable Roughness and Thermokarst models have the same r2
### but a few more slopes are significant for thermokarst and thermokarst is the
### variable primarily of interest
### will only summarize thermokarst data
# data needed for plotting
nee.karst.model.ngs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_table_ngs.csv')
nee.karst.model.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_model_table_gs.csv')
gpp.karst.model.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_karst_model_table_gs.csv')
reco.karst.model.ngs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_table_ngs.csv')
reco.karst.model.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_model_table_gs.csv')
nee.roughness.model.ngs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_table_ngs.csv')
nee.roughness.model.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_roughness_model_table_gs.csv')
gpp.roughness.model.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_roughness_model_table_gs.csv')
reco.roughness.model.ngs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_table_ngs.csv')
reco.roughness.model.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_roughness_model_table_gs.csv')
# nee.karst.lme.ngs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_lme_table_ngs.csv')
# nee.karst.lme.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/nee_karst_lme_table_gs.csv')
# gpp.karst.lme.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gpp_karst_lme_table_gs.csv')
# reco.karst.lme.ngs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_lme_table_ngs.csv')
# reco.karst.lme.gs.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/reco_karst_lme_table_gs.csv')

co2.models.ngs <- rbind.data.frame(nee.karst.model.ngs.table %>%
                                       mutate(Response = 'NEE',
                                              Predictor = 'Thermokarst'),
                                     reco.karst.model.ngs.table %>%
                                       mutate(Response = 'Reco',
                                              Predictor = 'Thermokarst'),
                                     nee.roughness.model.ngs.table %>%
                                       mutate(Response = 'NEE',
                                              Predictor = 'Roughness'),
                                     reco.roughness.model.ngs.table %>%
                                       mutate(Response = 'Reco',
                                              Predictor = 'Roughness')) %>%
  rename(`Full Model` = Full.Model, `Final Variables` = Final.Variables, `Min CI` = Min.CI, `Max CI` = Max.CI) %>%
  select(-`Full Model`) %>%
  mutate(Month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(Response, Predictor) %>%
  mutate(R2 = round(first(as.numeric(R2)), 2),
         label = paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', round(R2, 2)),
         season = 'NGS/Night')

co2.models.gs <- rbind.data.frame(gpp.karst.model.gs.table %>%
                                      mutate(Response = 'GPP',
                                             Predictor = 'Thermokarst'),
                                    nee.karst.model.gs.table %>%
                                       mutate(Response = 'NEE',
                                              Predictor = 'Thermokarst'),
                                     reco.karst.model.gs.table %>%
                                       mutate(Response = 'Reco',
                                              Predictor = 'Thermokarst'),
                                    gpp.roughness.model.gs.table %>%
                                      mutate(Response = 'GPP',
                                             Predictor = 'Roughness'),
                                    nee.roughness.model.gs.table %>%
                                       mutate(Response = 'NEE',
                                              Predictor = 'Roughness'),
                                     reco.roughness.model.gs.table %>%
                                       mutate(Response = 'Reco',
                                              Predictor = 'Roughness')) %>%
  rename(`Full Model` = Full.Model, `Final Variables` = Final.Variables, `Min CI` = Min.CI, `Max CI` = Max.CI) %>%
  select(-`Full Model`) %>%
  mutate(Month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  group_by(Response, Predictor) %>%
  mutate(R2 = round(first(as.numeric(R2)), 2),
         label = paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', R2),
         season = 'GS Day')

# all seasons combined
co2.model.summary <- co2.models.ngs %>%
  rbind.data.frame(co2.models.gs) %>%
  ungroup() %>%
  mutate(coefficient.type = str_to_title(coefficient.type),
         Response = factor(Response,
                           levels = c('Reco', 'NEE', 'GPP')))
# write.csv(co2.model.summary,
#           '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/co2_model_parameters.csv',
#           row.names = FALSE)

rm(gpp.karst.model.gs.table,
   gpp.roughness.model.gs.table,
   nee.karst.model.gs.table,
   nee.karst.model.ngs.table,
   nee.roughness.model.gs.table,
   nee.roughness.model.ngs.table,
   reco.karst.model.gs.table,
   reco.karst.model.ngs.table,
   reco.roughness.model.gs.table,
   reco.roughness.model.ngs.table)

# r2.label.ngs <- co2.models.ngs %>%
#   select(Predictor, Response, label, season) %>%
#   distinct() %>%
#   as.data.frame() %>%
#   mutate(x = c(rep(0.15, 2), rep(0.6, 2)))
# r2.label.gs <- co2.models.gs %>%
#   select(Predictor, Response, label, season) %>%
#   distinct() %>%
#   as.data.frame() %>%
#   mutate(x = c(rep(-0.25, 3), rep(-0.8, 3)))
r2.label <- co2.model.summary %>%
  filter(Predictor == 'Thermokarst') %>%
  select(Predictor, Response, label, season) %>%
  distinct() %>%
  as.data.frame() %>%
  mutate(x = rep(0.15, 5))

rects <- co2.model.summary %>%
  select(Predictor, Response) %>%
  distinct() %>%
  as.data.frame()
rects <- rects %>%
  mutate(xmin = -Inf,
         xmax = 0,
         col = letters[1]) %>%
  rbind.data.frame(rects %>%
                     mutate(xmin = 0,
                            xmax = Inf,
                            col = letters[2]))

# ### all seasons, thermokarst only, slopes only
# effect.size.plot <- ggplot(filter(co2.model.summary, coefficient.type == 'slope' & Predictor == 'Thermokarst'),
#                            aes(x = Month, color = Response, group = Response)) +
#   geom_rect(data = filter(rects, Predictor == 'Thermokarst'),
#             aes(xmin = -Inf, xmax = Inf, ymin = xmin, ymax = xmax, fill = col),
#             alpha = 0.2,
#             inherit.aes = FALSE) +
#   geom_hline(yintercept = 0) +
#   geom_point(aes(y = Coefficient),
#              show.legend = FALSE) +
#   geom_errorbar(aes(ymin = `Min CI`, ymax = `Max CI`),
#                 width = 0.1,
#                 show.legend = FALSE) +
#   geom_text(data = filter(r2.label, Predictor == 'Thermokarst'),
#             aes(x = 12.5, y = x, label = label),
#             parse = TRUE,
#             inherit.aes = FALSE,
#             size = 3,
#             vjust = 'inward',
#             hjust = 'inward') +
#   scale_x_continuous(breaks = seq(1:12),
#                      labels = str_sub(month.name[1:12], start = 1, end = 3)) +
#   scale_y_continuous(name = 'Slope') +
#   scale_color_manual(breaks = c('GPP', 'Reco', 'NEE'),
#                      values = c('#99CC33', '#CC3300', '#000066')) +
#   scale_fill_manual(name = 'As thermokarst\nincreases:',
#                     breaks = c('a', 'b'),
#                     values = c('#99CC33', '#CC3300'),
#                     labels = c('Sink \U02191 or Source \U02193',
#                                'Sink \U02193 or Source \U02191')) +
#   facet_grid(season ~ Response, scales = 'free_y') +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         legend.position = c(0.01, 0.01),
#         legend.justification = c(0, 0))
# effect.size.plot
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size.jpg',
# #        effect.size.plot,
# #        height = 4,
# #        width = 7)
# # 
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size.pdf',
# #        effect.size.plot,
# #        height = 4,
# #        width = 7)

### all seasons, thermokarst only, with intercepts
effect.size.plot <- ggplot(filter(co2.model.summary, Predictor == 'Thermokarst'),
                           aes(x = Month, group = coefficient.type)) +
  geom_rect(data = filter(rects, Predictor == 'Thermokarst'),
            aes(xmin = -Inf, xmax = Inf, ymin = xmin, ymax = xmax, fill = col),
            alpha = 0.2,
            inherit.aes = FALSE) +
  geom_hline(yintercept = 0) +
  geom_point(aes(y = Coefficient, color = coefficient.type),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = `Min CI`, ymax = `Max CI`, color = coefficient.type),
                position = position_dodge(width = 0.5),
                width = 0.3) +
  geom_text(data = filter(r2.label, Predictor == 'Thermokarst'),
            aes(x = 12.5, y = x, label = label),
            parse = TRUE,
            inherit.aes = FALSE,
            size = 3,
            vjust = 'inward',
            hjust = 'inward') +
  scale_x_continuous(breaks = seq(1:12),
                     labels = str_sub(month.name[1:12], start = 1, end = 3)) +
  scale_color_manual(name = element_blank(),
                     values = c('gray', 'black')) +
  scale_fill_manual(name = element_blank(),
                    breaks = c('a', 'b'),
                    values = c('#99CC33', '#CC3300'),
                    labels = c('Sink',
                               'Source')) +
  facet_grid(season ~ Response, scales = 'free_y') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = c(0.999, 0.002),
        legend.justification = c(1, 0),
        legend.margin = margin(-0.1, 0.1, -0.1, 0.1, "inches"),
        legend.box.background = element_rect(color = 'gray40', fill = "white"),
        legend.box.margin = margin(0.196,0.6,0.34,0.24,"inches"))
effect.size.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size.jpg',
#        effect.size.plot,
#        height = 3.75,
#        width = 6.5)
# 
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size.pdf',
#        effect.size.plot,
#        height = 3.75,
#        width = 6.5)

# ### Some older plots
# ### Plot NGS/Night
# # both thermokarst and roughness
# effect.size.plot.ngs <- ggplot(filter(co2.models.ngs, coefficient.type == 'slope'),
#                            aes(y = Variable, color = Response, group = Response)) +
#   geom_rect(data = filter(rects, Response != 'GPP'),
#             aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = col),
#             alpha = 0.2,
#             inherit.aes = FALSE) +
#   geom_vline(xintercept = 0) +
#   geom_point(aes(x = Coefficient),
#              show.legend = FALSE) +
#   geom_errorbar(aes(xmin = `Min CI`, xmax = `Max CI`),
#                 width = 0.1,
#                 show.legend = FALSE) +
#   geom_text(data = r2.label.ngs,
#             aes(x = x, y = 12.5, label = label),
#             parse = TRUE,
#             inherit.aes = FALSE,
#             size = 3,
#             vjust = 'inward',
#             hjust = 'inward') +
#   scale_y_discrete(limits = rev,
#                    labels = c('Dec', 'Nov', 'Oct', 'Sep', 'Aug', 'Jul', 'Jun', 'May', 'Apr', 'Mar', 'Feb', 'Jan')) +
#   scale_x_continuous(name = 'Slope') +
#   scale_color_manual(breaks = c('Reco', 'NEE'),
#                      values = c('#CC3300', '#000066')) +
#   scale_fill_manual(breaks = c('a', 'b'),
#                     values = c('#99CC33', '#FF6633'),
#                     labels = c('Greater Sink/\nSmaller Source',
#                                'Greater Source/\nSmaller Sink')) +
#   facet_grid(Response ~ Predictor, scales = 'free_x') +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),
#         legend.title = element_blank())
# effect.size.plot.ngs
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size_ngs.jpg',
# #        effect.size.plot.ngs,
# #        height = 6,
# #        width = 6)
# # 
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size_ngs.pdf',
# #        effect.size.plot.ngs,
# #        height = 6,
# #        width = 6)
# 
# ### GS Day
# effect.size.plot.gs <- ggplot(filter(co2.models.gs, coefficient.type == 'slope'),
#                               aes(y = Variable, color = Response, group = Response)) +
#   geom_rect(data = rects,
#             aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = col),
#             alpha = 0.2,
#             inherit.aes = FALSE) +
#   geom_vline(xintercept = 0) +
#   geom_point(aes(x = Coefficient)) +
#   geom_errorbar(aes(xmin = `Min CI`, xmax = `Max CI`), width = 0.1) +
#   geom_text(data = r2.label.gs,
#             aes(x = x, y = 7.5, label = label),
#             parse = TRUE,
#             inherit.aes = FALSE,
#             size = 3,
#             vjust = 'inward',
#             hjust = 'inward') +
#   scale_y_discrete(limits = rev,
#                    labels = c('Oct', 'Sep', 'Aug', 'Jul', 'Jun', 'May', 'Apr')) +
#   scale_x_continuous(name = 'Slope') +
#   scale_color_manual(breaks = c('GPP', 'Reco', 'NEE'),
#                      values = c('#99CC33','#CC3300', '#000066')) +
#   scale_fill_manual(breaks = c('a', 'b'),
#                     values = c('#99CC33', '#FF6633'),
#                     labels = c('Greater Sink/\nSmaller Source',
#                                'Greater Source/\nSmaller Sink')) +
#   facet_grid(Response ~ Predictor, scales = 'free_x') +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),
#         legend.position = 'none')
# effect.size.plot.gs
# 
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size_gs.jpg',
# #        effect.size.plot.gs,
# #        height = 6,
# #        width = 6)
# # 
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/co2_effect_size_gs.pdf',
# #        effect.size.plot.gs,
# #        height = 6,
# #        width = 6)

### Scatterplot with smooth line for NEE at all times
co2.tk.plot <- ggplot(co2.model.data, aes(x = percent.thermokarst.ffp, y = NEP, color = month.factor)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_viridis(discrete = TRUE,
                      option = 'B') +
  theme_bw()
co2.tk.plot
########################################################################################################################

### CH4 Analysis
### Prep Data ##########################################################################################################
# load data from our files to get most recent years
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2015-2016/AK15_CO2&CH4_30Apr2019.Rdata')
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2016-2017/AK16_CO2&CH4_30Apr2019.Rdata')
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/AK17_CO2&CH4_30Apr2019.Rdata')
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_CO2&CH4_30Apr2019.Rdata')
Tower18.19[, u_var := NULL]
Tower18.19[, v_var := NULL]
Tower18.19[, w_var := NULL]
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_CO2&CH4.Rdata')
ch4.flux <- rbind(Tower15.16, Tower16.17, Tower17.18, Tower18.19, Tower19.20)
ch4.flux[, 46 := NULL]
ch4.flux[, year := year(ts)]
ch4.flux[, month := month(ts)]
ch4.flux[, week := week(ts)]

# load GPP in order to classify groups same as co2
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2016-2017/AK16_Carbon_new_30Apr2019.Rdata')
carbon.16 <- export
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2017-2018/AK17_Carbon_new_30Apr2019.Rdata')
carbon.17 <- export
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2018-2019/AK18_Carbon_new_30Apr2019.Rdata')
carbon.18 <- export
carbon.18[, u_var := NULL]
carbon.18[, v_var := NULL]
carbon.18[, w_var := NULL]
load('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Gradient/Eddy/2019-2020/AK19_Carbon.Rdata')
carbon.19 <- export
rm(export)

gpp <- rbind(carbon.16,
             carbon.17,
             carbon.18,
             carbon.19) %>%
  select(ts, GEP)


# Join ch4 and karst/roughness data
ch4.model.data <- ch4.flux %>%
  left_join(karst_roughness, by = c('ts', 'wind_dir')) %>%
  rename(percent.thermokarst.ffp = karst.pc, mtopo15.sd.ffp = sd.mtopo) %>%
  filter(!(is.na(ch4_flux_filter) | is.na(percent.thermokarst.ffp))) %>%
  mutate(year.factor = factor(year(ts)),
         month.factor = factor(month),
         ch4.flux.hh = ch4_flux_filter*(12.0107 * 1800)/1000, # convert to mg/half hour from micromol/s
         ch4.clean = ifelse(ch4.flux.hh > mean(ch4.flux.hh) + 3*sd(ch4.flux.hh) |
                              ch4.flux.hh < mean(ch4.flux.hh) - 3*sd(ch4.flux.hh), # remove outliers at 3 std dev for calculation of rolling mean
                            NA,
                            ch4.flux.hh),
         ch4.mean.15d = rollapply(ch4.clean, 720, mean, na.rm = TRUE, partial = TRUE),
         ch4.sd.15d = rollapply(ch4.clean, 720, sd, na.rm = TRUE, partial = TRUE),
         spike = ifelse(ch4.flux.hh > ch4.mean.15d + 3*ch4.sd.15d,
                        'release spike',
                        ifelse(ch4.flux.hh < ch4.mean.15d - 3*ch4.sd.15d,
                               'uptake spike',
                               'non-spike'))) %>%
  filter(!(year.factor == 2017 & ts > as_date('2017-12-01') |
             year.factor == 2018 & ts <= as_date('2018-05-12'))) %>%
  as.data.table() %>%
  left_join(gpp,
            by = c('ts')) %>%
  mutate(season = ifelse(month >= 5 & month <= 9,
                         'GS',
                         'NGS'),
         group = ifelse(PAR_filter >= 10 & (month >= 5 & month <= 9 | (month == 4 | month == 10) & GEP > 0),
                        'GS Day',
                        ifelse(PAR_filter < 10 & month >= 5 & month <= 9,
                               'GS Night',
                               ifelse(month <= 4 | month >= 10,
                                      'NGS',
                                      NA))),
         wind.speed.groups = factor(ifelse(wind_speed_filter < 4,
                                           '<4',
                                           ifelse(wind_speed_filter < 8,
                                                  '4-8',
                                                  ifelse(wind_speed_filter < 12,
                                                         '8-12',
                                                         ifelse(wind_speed_filter >= 12,
                                                                '>12',
                                                                NA)))),
                                    levels = c('<4', '4-8', '8-12', '>12')),
         thermokarst.groups = factor(ifelse(percent.thermokarst.ffp < 0.1,
                                            '<0.1',
                                            ifelse(percent.thermokarst.ffp < 0.2,
                                                   '0.1-0.2',
                                                   ifelse(percent.thermokarst.ffp < 0.3,
                                                          '0.2-0.3',
                                                          ifelse(percent.thermokarst.ffp >= 0.3,
                                                                 '>0.3',
                                                                 NA)))),
                                     levels = c('<0.1', '0.1-0.2', '0.2-0.3', '>0.3')),
         mean.swc = ifelse(!is.na(SWC_1_1_1) & !is.na(SWC_2_1_1),
                           (SWC_1_1_1 + SWC_2_1_1)/2,
                           ifelse(!is.na(SWC_1_1_1),
                                  SWC_1_1_1,
                                  ifelse(!is.na(SWC_2_1_1),
                                         SWC_2_1_1,
                                         NA))),
         mean.ts.10 = ifelse(!is.na(TS_1_1_1) & !is.na(TS_5_1_1),
                             (TS_1_1_1 + TS_5_1_1)/2,
                             ifelse(!is.na(TS_1_1_1),
                                    TS_1_1_1,
                                    ifelse(!is.na(TS_5_1_1),
                                           TS_5_1_1,
                                           NA))))

ch4.model.data <- ch4.model.data[order(ts)]

hist(ch4.model.data$ch4_flux_filter)
hist(ch4.model.data$ch4.flux.hh)

# make sure the groups came out correctly
ggplot(ch4.model.data, aes(x = ts, y = ch4.flux.hh, color = group)) +
  geom_point()
ggplot(ch4.model.data, aes(x = ts, y = ch4.flux.hh, color = spike)) +
  geom_point()

hist(filter(ch4.model.data, spike == 'non-spike')$ch4.flux.hh)
hist(filter(ch4.model.data, spike == 'release spike')$ch4.flux.hh)
hist(filter(ch4.model.data, spike == 'uptake spike')$ch4.flux.hh)

ch4.no.spike <- ch4.model.data %>%
  filter(spike == 'non-spike') %>%
  select(ch4.flux.hh, ch4.hyp.sine, month.factor, percent.thermokarst.ffp, wind_speed_filter, mean.swc, mean.ts.10) %>%
  mutate(ch4.scale = scale(ch4.hyp.sine),
         percent.thermokarst.scale = scale(percent.thermokarst.ffp),
         wind.speed.scale = scale(wind_speed_filter),
         mean.swc.scale = scale(mean.swc),
         mean.ts.10.scale = scale(mean.ts.10))

ch4.no.spike <- ch4.model.data %>%
  filter(spike == 'non-spike') %>%
  select(ch4.flux.hh, month.factor, percent.thermokarst.ffp, wind_speed_filter, mean.swc, mean.ts.10)
########################################################################################################################

### Check Diurnal Variation in Fluxes ##################################################################################
# # Look for diurnal variation
# ggplot(ch4.flux[year == 2016], aes(x = ts, y = ch4_flux_filter)) +
#   geom_point() +
#   facet_wrap(~week, scales = 'free_x') +
#   ggtitle('2016')
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/2016.jpg',
# #        height = 8,
# #        width = 15)
# 
# ggplot(ch4.flux[year == 2017], aes(x = ts, y = ch4_flux_filter)) +
#   geom_point() +
#   facet_wrap(~week, scales = 'free_x') +
#   ggtitle('2017')
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/2017.jpg',
# #        height = 8,
# #        width = 15)
# 
# ggplot(ch4.flux[year == 2018], aes(x = ts, y = ch4_flux_filter)) +
#   geom_point() +
#   facet_wrap(~week, scales = 'free_x') +
#   ggtitle('2018')
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/2018.jpg',
# #        height = 8,
# #        width = 15)
# 
# ggplot(ch4.flux[year == 2019], aes(x = ts, y = ch4_flux_filter)) +
#   geom_point() +
#   facet_wrap(~week, scales = 'free_x') +
#   ggtitle('2019')
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/2019.jpg',
# #        height = 8,
# #        width = 15)
# 
# ggplot(ch4.flux[week >= 17 & week <= 21 & year > 2015 & year < 2020],
#        aes(x = ts, y = ch4_flux_filter)) +
#   geom_point() +
#   facet_wrap(~year, scales = 'free_x', ncol = 1)
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/May_transition.jpg',
# #        height = 8,
# #        width = 15)
# 
# almostall <- ggplot(ch4.flux[!(year == 2017 & ts > '2017-12-01' |
#                                  year == 2018)],
#                     aes(x = tair, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('All years except late 2017 - 2018')
# all <- ggplot(ch4.flux, aes(x = tair, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('All years')
# eighteen <- ggplot(ch4.flux[year == 2018],
#                    aes(x = tair, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('2018')
# winter <- ggplot(ch4.flux[year == 2018 & ts <= as_date('2018-05-12') |
#                             year == 2017 & ts >= as_date('2017-12-01')],
#                  aes(x = tair, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('Suspect Winter')
# summer <- ggplot(ch4.flux[year == 2018 & ts > as_date('2018-05-12')],
#                  aes(x = tair, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   ggtitle('Suspect Summer')
# combined <- ggarrange(all, almostall, eighteen, winter, summer,
#                       nrow = 1,
#                       widths = c(0.78, 0.78, 0.78, 0.78, 1))
# combined
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/air_temp.jpg',
# #        combined,
# #        height = 8,
# #        width = 15)
# 
# almostall <- ggplot(ch4.flux[!(year == 2017 & ts > '2017-12-01' |
#                                  year == 2018)],
#                     aes(x = wind_speed_filter, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('All years except late 2017 - 2018')
# all <- ggplot(ch4.flux, aes(x = wind_speed_filter, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('All years')
# eighteen <- ggplot(ch4.flux[year == 2018],
#                    aes(x = wind_speed_filter, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('2018')
# winter <- ggplot(ch4.flux[year == 2018 & ts <= as_date('2018-05-12') |
#                             year == 2017 & ts >= as_date('2017-12-01')],
#                  aes(x = wind_speed_filter, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   theme(legend.position = 'none') +
#   ggtitle('Suspect Winter')
# summer <- ggplot(ch4.flux[year == 2018 & ts > as_date('2018-05-12')],
#                  aes(x = wind_speed_filter, y = ch4_flux_filter, color = year)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-0.3, 0.3)) +
#   scale_color_viridis(limits = c(2015, 2020)) +
#   ggtitle('Suspect Summer')
# combined <- ggarrange(all, almostall, eighteen, winter, summer,
#                       nrow = 1,
#                       widths = c(0.78, 0.78, 0.78, 0.78, 1))
# combined
# # ggsave('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/thermokarst_project/figures/methane_data_check/wind_speed.jpg',
# #        combined,
# #        height = 8,
# #        width = 15)
########################################################################################################################

### Plot Data ##########################################################################################################
# plot by thermokarst
ch4.plot <- ggplot(filter(ch4.model.data, spike == 'non-spike'),
                    aes(x = percent.thermokarst.ffp,
                        y = ch4.flux.hh,
                        color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm', fill = 'gray') +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = factor(seq(1:12)),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
                                 ' Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      discrete = TRUE,
                      option = 'B',
                      end = 0.9) +
  theme_bw() +
  theme(legend.title = element_blank())
ch4.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_tk.jpg',
#        ch4.plot,
#        width = 6,
#        height = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_tk.pdf',
#        ch4.plot,
#        width = 6,
#        height = 6)

ggplot(filter(ch4.model.data, spike == 'non-spike'),
       aes(x = percent.thermokarst.ffp,
           y = ch4_flux_filter)) +
  geom_point(alpha = 0.4,
             size = 1,
             aes(color = year.factor)) +
  scale_x_continuous(name = 'Thermokarst Cover (%)') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1))) +
  geom_smooth(method = 'lm', color = 'black', fill = 'gray') +
  facet_wrap(~ month.factor) +
  theme_bw() +
  theme(legend.title = element_blank())

# # by year to look at questionable 2018 data 
# # REMOVE THE DATE FILTER FOR CH4.MODEL.DATA BEFORE RUNNING THESE PLOTS!!!
# ch4.plot1 <- ggplot(ch4.model.data,
#        aes(x = percent.thermokarst.ffp, y = ch4_flux_filter, color = year.factor)) +
#   geom_point(alpha = 0.2,
#              size = 1) +
#   geom_smooth(method = 'lm', fill = 'gray', size = 0.5) +
#   scale_x_continuous(name = 'Thermokarst Cover (%)') +
#   scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1)),
#                      limits = c(-4, 4)) +
#   scale_color_viridis(discrete = TRUE,
#                       option = 'B',
#                       end = 0.9,
#                       breaks = c(2016, 2017, 2018, 2019)) +
#   theme_bw() +
#   theme(legend.position = 'none') +
#   ggtitle('All Data')
# ch4.plot1
# ch4.plot2 <- ggplot(ch4.model.data[!(year.factor == 2017 & ts > as_date('2017-12-01') |
#                                  year.factor == 2018 & ts <= as_date('2018-05-12'))],
#                     aes(x = percent.thermokarst.ffp, y = ch4_flux_filter, color = year.factor)) +
#   geom_point(alpha = 0.2,
#              size = 1) +
#   geom_smooth(method = 'lm', fill = 'gray', size = 0.5) +
#   scale_x_continuous(name = 'Thermokarst Cover (%)') +
#   scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1)),
#                      limits = c(-4, 4)) +
#   scale_color_viridis(discrete = TRUE,
#                       option = 'B',
#                       end = 0.9,
#                       breaks = c(2016, 2017, 2018, 2019)) +
#   theme_bw() +
#   theme(legend.title = element_blank()) +
#   ggtitle('Winter 2017-2018 Removed')
# ch4.plot2
# ch4.plot.compare <- ggarrange(ch4.plot1, ch4.plot2, ncol = 2, widths = c(0.83, 1))
# ch4.plot.compare
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_data_check/ch4_tk.jpg',
# #        ch4.plot.compare,
# #        width = 10,
# #        height = 6)

# do big storms (particularly in winter) come from non-thermokarst directions?
ggplot(ch4.model.data, aes(x = ts, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point()
ggplot(filter(ch4.model.data, spike == 'non-spike'), aes(x = ts, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point()

ggplot(ch4.model.data, aes(x = wind_dir, y = wind_speed_filter, color = month.factor)) +
  geom_point()
ggplot(filter(ch4.model.data, spike == 'non-spike'), aes(x = wind_dir, y = wind_speed_filter, color = month.factor)) +
  geom_point()

ggplot(ch4.model.data,
       aes(x = wind_dir,
           y = ch4.flux.hh,
           color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm', fill = 'gray') +
  scale_x_continuous(name = 'Wind Direction') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1)),
                     limits = c(-1, 1)) +
  scale_color_viridis(breaks = factor(seq(1:12)),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
                                 ' Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      discrete = TRUE,
                      option = 'B',
                      end = 0.9) +
  theme_bw() +
  theme(legend.title = element_blank())
ggplot(filter(ch4.model.data, spike == 'non-spike'),
       aes(x = wind_dir,
           y = ch4.flux.hh,
           color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm', fill = 'gray') +
  scale_x_continuous(name = 'Wind Direction') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1)),
                     limits = c(-1, 1)) +
  scale_color_viridis(breaks = factor(seq(1:12)),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
                                 ' Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      discrete = TRUE,
                      option = 'B',
                      end = 0.9) +
  theme_bw() +
  theme(legend.title = element_blank())

ggplot(ch4.model.data,
       aes(x = percent.thermokarst.ffp,
           y = wind_speed_filter,
           color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1)
ggplot(filter(ch4.model.data, spike == 'non-spike'),
       aes(x = percent.thermokarst.ffp,
           y = wind_speed_filter,
           color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1)

ggplot(ch4.model.data,
       aes(x = month.factor,
           y = wind_speed_filter,
           color = month.factor)) +
  geom_boxplot()
ggplot(filter(ch4.model.data, spike == 'non-spike'),
       aes(x = month.factor,
           y = wind_speed_filter,
           color = month.factor)) +
  geom_boxplot()

# plot by wind speed
# in the winter, wind speed seems to be a strong driver of fluxes
ch4.ws.plot <- ggplot(filter(ch4.model.data, spike == 'non-spike'),
                   aes(x = wind_speed_filter, y = ch4_flux_filter, color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm', aes(fill = month.factor)) +
  scale_x_continuous(name = 'Wind Speed (m/s)') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      discrete = TRUE,
                      option = 'B',
                      end = 0.9) +
  scale_fill_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      discrete = TRUE,
                      option = 'B',
                      end = 0.9) +
  theme_bw() +
  theme(legend.title = element_blank())
ch4.ws.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_ws.jpg',
#        ch4.ws.plot,
#        width = 6,
#        height = 6)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_ws.pdf',
#        ch4.ws.plot,
#        width = 6,
#        height = 6)

ggplot(filter(ch4.model.data, spike == 'non-spike'),
       aes(x = wind_speed_filter, y = ch4_flux_filter, color = month.factor)) +
  geom_point(alpha = 0.4,
             size = 1) +
  geom_smooth(method = 'lm', aes(fill = month.factor)) +
  scale_x_continuous(name = 'Wind Speed (m/s)') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1))) +
  scale_color_viridis(breaks = seq(1, 12),
                      labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                      discrete = TRUE,
                      option = 'B',
                      end = 0.9) +
  scale_fill_viridis(breaks = seq(1, 12),
                     labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                     discrete = TRUE,
                     option = 'B',
                     end = 0.9) +
  facet_grid(.~group) +
  theme_bw() +
  theme(legend.title = element_blank())

# # Test how relationship differs by year (is 2017-2018 bad?)
# ggplot(ch4.model.data[filled == 0],
#        aes(x = percent.thermokarst.slice, y = FCH4, color = year.factor, group = year.factor)) +
#   geom_point(alpha = 0.4,
#              size = 1) +
#   geom_smooth(method = 'lm',
#               aes(fill = year.factor)) +
#   scale_x_continuous(name = 'Thermokarst Cover (%)') +
#   scale_y_continuous(name = expression('CH'[4] ~ 'Flux' ~ ('mg C' ~ 'm'^-2 ~ 'hh'^-1))) +
#   theme_bw() +
#   theme(legend.title = element_blank())
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_by_year.jpg')
# smallest <- FCH4 ~ 1
# biggest <- FCH4 ~ percent.thermokarst.slice*year.factor
# start <- lm(FCH4 ~ percent.thermokarst.slice+year.factor,
#                 data = ch4.model.data[filled == 0])
# stats::step(start, scope = list(lower = smallest, upper = biggest))
# # using an aic improvement cutoff of 5 to add terms, we select FCH4 ~ 
# # percent.thermokarst.slice + year.factor. So the slopes are the same between 2018
# # and other years, but the intercept (flux magnitude) is different
########################################################################################################################

### Linear Model of Non-Spike Data #####################################################################################
# simple
smallest <- ch4.flux.hh ~ 1
biggest <- ch4.flux.hh ~ percent.thermokarst.ffp*month.factor
start <- lm(ch4.flux.hh ~ percent.thermokarst.ffp,
            data = ch4.no.spike)

stats::step(start, scope = list(lower = smallest, upper = biggest))

ch4.model <- lm(formula = ch4.flux.hh ~ percent.thermokarst.ffp*month.factor,
                     data = ch4.no.spike)
# saveRDS(ch4.model, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ch4_model_all_years.rds')
ch4.model <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ch4_model_all_years.rds')
summary(ch4.model)

# this is a gross way to summarize and make everything look nice - better options?
# simple model
ch4.model.ci <- extract_ci_lm(ch4.model)
ch4.model.table <- ch4.model.ci %>%
  mutate(`Final Variables` = str_replace(term, 'percent.thermokarst.ffp', 'TK'),
         `Final Variables` = str_replace(`Final Variables`, 'month.factor', 'Month - '),
         `Final Variables` = str_replace(`Final Variables`, '\\(Intercept\\)', 'Month - 1 (Intercept)'),
         `Final Variables` = ifelse(str_detect(`Final Variables`, '^Month - [:digit:]+$'),
                                    str_c(`Final Variables`, ' (Intercept)'),
                                    `Final Variables`),
         `Final Variables` = str_replace(`Final Variables`, '^TK$', 'TK:Month - 1'),
         radius = coefs - min,
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+')),
         coefficient.type = factor(ifelse(str_detect(`Final Variables`, '(Intercept)'),
                                          'intercept',
                                          'slope'),
                                   levels = c('intercept', 'slope')),
         month = as.numeric(str_extract(`Final Variables`, '[:digit:]+'))) %>%
  arrange(coefficient.type, month) %>%
  group_by(coefficient.type, month) %>%
  mutate(Coefficient = ifelse(coefs - first(coefs) == 0,
                              round(coefs, 5),
                              round(first(coefs) + coefs, 5)),
         total.radius = ifelse(coefs - first(coefs) == 0,
                               radius,
                               sqrt(radius^2 + first(radius)^2)),
         `Min CI` = Coefficient - total.radius,
         `Max CI` = Coefficient + total.radius) %>%
  ungroup() %>%
  mutate(Response = c('CH4', rep('', 23)),
         `Full Model` = c('TK*Month', rep('', 23)),
         R2 = c(round(summary(ch4.model)$r.squared, 3), rep('', 23))) %>%
  select(Response, `Full Model`, `Final Variables`, Coefficient, `Min CI`, `Max CI`, R2)

# write.csv(ch4.model.table, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ch4_model_table_all_years.csv',
#           row.names = FALSE)
ch4.model.table <- read.csv('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/ch4_model_table_all_years.csv') %>%
  rename('Response' = 1, 'Full Model' = 2, 'Final Variables' = 3,
         'Coefficient' = 4, 'Min CI' = 5, 'Max CI' = 6, 'R2' = 7)

ch4.slopes <- slice(ch4.model.table, 13:24) %>%
  select(3:6) %>%
  mutate(month = seq(1:12),
         coefficient = 'Slope')
ch4.intercepts <- slice(ch4.model.table, 1:12) %>%
  select(3:6) %>%
  mutate(month = seq(1:12),
         coefficient = 'Intercept')
ch4.coefficients <- rbind(ch4.intercepts, ch4.slopes)
text <- data.frame(x = 12,
                   y = -0.0225,
                   label = paste0(as.character(expression('R'^2 ~ ' = ')), ' ~ ', ch4.model.table$R2[1]))
ch4.rects <- data.frame(ymin = c(-Inf, 0),
                        ymax = c(0, Inf),
                        col = c('a', 'b'))
# the higher methane uptake with higher
# thermokarst is probably actually driven by high release at low thermokarst
# which is a coincidence because winter storms with high wind speeds and high
# methane release happen to come from a direction with low thermokrst!
ch4.coefficients.plot <- ggplot(ch4.coefficients,
                                aes(x = month, y = Coefficient, color = coefficient)) +
  geom_rect(data = ch4.rects,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = col),
            alpha = 0.2,
            inherit.aes = FALSE) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = `Min CI`, ymax = `Max CI`),
                position = position_dodge(width = 0.5),
                width = 0.3) +
  geom_text(data = text,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            parse = TRUE,
            size = 3,
            vjust = 'inward',
            hjust = 'inward') +
  scale_x_continuous(breaks = seq(1, 12),
                     labels = month.name[seq(1, 12)],
                     minor_breaks = NULL) +
  scale_y_continuous(name = 'Coefficient') +
  scale_color_manual(name = element_blank(),
                     values = c('gray', 'black')) +
  scale_fill_manual(name = element_blank(),
                    breaks = c('a', 'b'),
                    values = c('#99CC33', '#CC3300'),
                    labels = c('Sink',
                               'Source')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ch4.coefficients.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_coefficients_all_years.jpg',
#        ch4.coefficients.plot,
#        height = 4,
#        width = 4)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_coefficients_all_years.pdf',
#        ch4.coefficients.plot,
#        height = 4,
#        width = 4)

# # Plot with only slopes
# ch4.slopes.plot <- ggplot(ch4.slopes, aes(x = month, y = Coefficient)) +
#   geom_rect(data = ch4.rects,
#             aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = col),
#             alpha = 0.2,
#             inherit.aes = FALSE) +
#   geom_point() +
#   geom_errorbar(aes(ymin = `Min CI`, ymax = `Max CI`),
#                 width = 0.1) +
#   geom_text(data = text,
#             aes(x = x, y = y, label = label),
#             parse = TRUE,
#             size = 3,
#             vjust = 'inward',
#             hjust = 'inward') +
#   scale_x_continuous(breaks = seq(1, 12),
#                      labels = month.name[seq(1, 12)],
#                      minor_breaks = NULL) +
#   scale_y_continuous(name = 'Slope') +
#   scale_fill_manual(name = 'As thermokarst\nincreases:',
#                     breaks = c('a', 'b'),
#                     values = c('#99CC33', '#CC3300'),
#                     labels = c('Sink \U02191 or Source \U02193',
#                                'Sink \U02193 or Source \U02191')) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# ch4.slopes.plot
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_slopes_all_years.jpg',
# #        ch4.slopes.plot,
# #        height = 4,
# #        width = 4)
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_slopes_all_years.pdf',
# #        ch4.slopes.plot,
# #        height = 4,
# #        width = 4)

# # Plot with only intercepts
# ch4.intercepts.plot <- ggplot(ch4.intercepts, aes(x = month, y = Coefficient)) +
#   geom_rect(data = filter(rects, Response == 'NEE'),
#             aes(xmin = -Inf, xmax = Inf, ymin = xmin, ymax = xmax, fill = col),
#             alpha = 0.2,
#             inherit.aes = FALSE) +
#   geom_point() +
#   geom_errorbar(aes(ymin = `Min CI`, ymax = `Max CI`),
#                 width = 0.1) +
#   scale_x_continuous(breaks = seq(1, 12),
#                      labels = month.name[seq(1, 12)],
#                      minor_breaks = NULL) +
#   scale_y_continuous(name = 'Intercept') +
#   scale_fill_manual(breaks = c('a', 'b'),
#                     values = c('#99CC33', '#CC3300'),
#                     labels = c('Sink',
#                                'Source')) +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#         legend.title = element_blank())
# ch4.intercepts.plot
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_intercepts_all_years.jpg',
# #        ch4.intercepts.plot,
# #        height = 4,
# #        width = 4)
# # ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/ch4_intercepts_all_years.pdf',
# #        ch4.intercepts.plot,
# #        height = 4,
# #        width = 4)
########################################################################################################################

### Investigate Pulses #################################################################################################
### Best Plots
# wind speed important in winter spikes
ch4.ws.plot <- ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Wind Speed (m/s)') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis(name = 'Thermokarst % Cover') +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_blank(),
        strip.text = element_blank())
ch4.ws.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_release_ws_tk_season.jpg')

# Soil Moisture important in summer spikes
ch4.swc.plot <- ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = mean.swc, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Soil Moisture (%)') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis(name = 'Thermokarst % Cover') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.y = element_blank(),
        axis.text.y = element_blank())
ch4.swc.plot
ch4.pulse.plot <- ggarrange(ch4.ws.plot,
          ch4.swc.plot,
          ncol = 2,
          widths = c(1, 0.945),
          legend = 'bottom',
          legend.grob = get_legend(ch4.swc.plot))
ch4.pulse.plot
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_release.jpg',
#        ch4.pulse.plot,
#        height = 4,
#        width = 4)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_release.pdf',
#        ch4.pulse.plot,
#        height = 4,
#        width = 4)


### Pulse Release
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = wind_speed_filter)) +
  geom_point()
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = wind_speed_filter)) +
  geom_point() +
  facet_grid(wind.speed.groups ~ .) +
  scale_color_viridis()
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = month.factor)) +
  geom_point() +
  facet_grid(wind.speed.groups ~ .) +
  scale_color_viridis(discrete = TRUE,
                      option = 'B')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_release_tk_ws_month.jpg')
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point() +
  facet_grid(thermokarst.groups ~ .) +
  scale_color_viridis()
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = month.factor)) +
  geom_point() +
  facet_grid(thermokarst.groups ~ .) +
  scale_color_viridis(discrete = TRUE,
                      option = 'B')
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_release_ws_tk_month.jpg')
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = season)) +
  geom_point() +
  facet_grid(thermokarst.groups ~ .) +
  scale_color_viridis(discrete = TRUE,
                      option = 'B')

# separated by growing season
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = mean.swc)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Wind Speed') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis() +
  theme_bw()
# ggplot(filter(ch4.model.data, spike == 'release spike' & season == 'NGS'),
#        aes(x = wind_speed_filter, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
#   geom_point() +
#   facet_grid(season ~ .) +
#   scale_color_viridis() +
#   geom_smooth(method = "nls", formula = y ~ a + b*exp(c * x),
#               se=F,
#               method.args = list(start = c(a = 10, b = 1, c = 0.5)),
#               color = 'black') +
#   theme_bw()
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = mean.swc, group = season)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Wind Speed') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis() +
  theme_bw()

ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = mean.swc, group = season)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Thermokarst % Cover') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis(name = 'Soil Moisture') +
  theme_bw()
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_release_tk_ws_season.jpg')

# air temp doesn't matter for summer
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = tair, y = ch4.flux.hh, color = wind_speed_filter)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_color_viridis() +
  theme_bw()
# soil temp sensors reminder
ggplot(ch4.model.data, aes(x = ts)) +
  geom_line(aes(y = TS_1_1_1, color = '10')) +
  geom_line(aes(y = TS_2_1_1, color = '20')) +
  geom_line(aes(y = TS_3_1_1, color = '30')) +
  geom_line(aes(y = TS_4_1_1, color = '40')) +
  geom_line(aes(y = TS_5_1_1, color = '10')) +
  geom_line(aes(y = TS_6_1_1, color = '20')) +
  geom_line(aes(y = TS_7_1_1, color = '30')) +
  geom_line(aes(y = TS_8_1_1, color = '40'))
# soil temp: 10 (2 sensors at tower only!)
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = mean.ts.10, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_color_viridis() +
  theme_bw()
# deeper temps show less relationship

# soil moisture (sensor at tower only!)
# soil moisture seems to drive pulses
ggplot(ch4.model.data, aes(x = ts)) +
  geom_line(aes(y = SWC_1_1_1, color = '1')) +
  geom_line(aes(y = SWC_2_1_1, color = '2'))
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = mean.swc, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Soil Moisture') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis() +
  theme_bw()
ggplot(filter(ch4.model.data, spike == 'release spike'),
       aes(x = mean.swc, y = ch4.flux.hh, group = season, color = month.factor)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_x_continuous(name = 'Soil Moisture') +
  scale_y_continuous(name = expression('CH'[4] ~ 'Flux (mg C m'^-2 ~ 'half hour'^-1 ~ ')')) +
  scale_color_viridis(discrete = TRUE,
                      option = 'B') +
  theme_bw()
ggplot(filter(ch4.model.data, spike == 'release spike' & season == 'GS' & !(month == 5 & ch4.flux.hh > 4)), # remove one outlier that influences models a lot
       aes(x = mean.swc, y = ch4.flux.hh, group = season, color = month.factor)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE,
                      option = 'B',
                      begin = 5/12,
                      end = 3/4) +
  geom_smooth(method = "nls", formula = y ~ a*exp(b*x),
              se=F,
              method.args = list(start = c(a = 1, b = 0.1)),
              color = 'black') +
  theme_bw()


### plot snow depth to see if high methane relates to snow depth
snow.plot <- ggplot(ch4.flux[SNOWD_1_1_1 < 2 & SNOWD_1_1_1 > -2], aes(x = ts, y = SNOWD_1_1_1)) +
  geom_point()
plotly::ggplotly(snow.plot)

# test how non linear models work - not looking so good...
model <- nls(ch4.flux.hh ~ a + b*exp(c*wind_speed_filter),
             data = filter(ch4.model.data, spike == 'release spike' & season == 'NGS'),
             start = c(a = 1, b = 1, c = 0.5))
summary(model)

model <- nls(ch4.flux.hh ~ a*exp(b*mean.swc),
             data = filter(ch4.model.data, spike == 'release spike' & season == 'GS'),
             start = c(a = 1, b = 0.1))
summary(model)

model <- nls(ch4.flux.hh ~ a*exp(b*percent.thermokarst.ffp),
             data = filter(ch4.model.data, spike == 'release spike' & season == 'GS'),
             start = c(a = 1, b = 1))
summary(model)

model <- nls(ch4.flux.hh ~ a*exp(b*mean.swc + c*percent.thermokarst.ffp),
             data = filter(ch4.model.data, spike == 'release spike' & season == 'GS'),
             start = c(a = 1, b = 0.1, c = 1))
summary(model)

model.fit <- ch4.model.data %>%
  filter(spike == 'release spike' & season == 'GS') %>%
  mutate(fit = predict(model, newdata = filter(ch4.model.data, spike == 'release spike' & season == 'GS')))

# try a regression tree (with all data, or just pulses?)
library(tree)
ch4.tree.data <- ch4.model.data %>%
  select(ch4.flux.hh, spike, season, tair, wind.speed = wind_speed_filter,
         percent.thermokarst = percent.thermokarst.ffp,
         roughness = mtopo15.sd.ffp, mean.swc, tsoil = mean.ts.10)

set.seed(10112020)
# non-spike
# ch4.non.spike <- ch4.tree.data %>%
#   filter(spike == 'non-spike')
# train.all <- sample(1:nrow(ch4.non.spike), 9750)
# tree.all.data <- tree(ch4.flux.hh ~ ., data = ch4.non.spike, subset = train.all)
# saveRDS(tree.all.data, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/methane_tree_no_spikes.rds')
tree.all.data <- readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/methane_tree_no_spikes.rds')
summary(tree.all.data)
plot(tree.all.data)
text(tree.all.data, pretty = 0)
tree.all.perf <- ch4.non.spike %>%
  slice(-train.all) %>%
  mutate(fit = predict(tree.all.data, ch4.non.spike[-train.all,]))
ggplot(tree.all.perf, aes(x = ch4.flux.hh, y = fit)) +
  geom_point() +
  geom_smooth(method = 'lm')
cv.tree.all <- cv.tree(tree.all.data, FUN = prune.tree)
cv.tree.all
plot(cv.tree.all)
# prune.tree.all <- prune.tree(tree.all.data, best = 4)
# plot(prune.tree.all)
# text(prune.tree.all, pretty=0)
# summary(prune.tree.all)

# pulse release - this varies quite a bit from one run to another. Is this a problem?
ch4.tree.release <- filter(ch4.tree.data, spike == 'release spike')
train.release <- sample(1:nrow(ch4.tree.release), 186)
tree.release.spike <- tree(ch4.flux.hh ~ ., data = ch4.tree.release, subset = train.release)
# saveRDS(tree.release.spike, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/methane_tree_release_spikes.rds')
readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/methane_tree_release_spikes.rds')
summary(tree.release.spike)
plot(tree.release.spike)
text(tree.release.spike, pretty = 0)
tree.release.perf <- ch4.tree.release %>%
  slice(-train.release) %>%
  mutate(fit = predict(tree.release.spike, ch4.tree.release[-train.release,]))
ggplot(tree.release.perf, aes(x = ch4.flux.hh, y = fit)) +
  geom_point() +
  geom_smooth(method = 'lm')
cv.tree.release <- cv.tree(tree.release.spike, FUN = prune.tree)
cv.tree.release
plot(cv.tree.release)
prune.tree.release <- prune.tree(tree.release.spike, best = 7)
plot(prune.tree.release)
text(prune.tree.release, pretty=0)
summary(prune.tree.release)

# pulse uptake - this one is particularly changeable and I think it's due to the small number of data points which show a lot of uptake
# don't know if I want to include this
ch4.tree.uptake <- filter(ch4.tree.data, spike == 'uptake spike')
train.uptake <- sample(1:nrow(ch4.tree.uptake), 86)
tree.uptake.spike <- tree(ch4.flux.hh ~ ., data = ch4.tree.uptake, subset = train.uptake)
# saveRDS(tree.uptake.spike, '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/methane_tree_uptake_spikes.rds')
# readRDS('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/methane_tree_uptake_spikes.rds')
summary(tree.uptake.spike)
plot(tree.uptake.spike)
text(tree.uptake.spike, pretty = 0)
tree.uptake.perf <- ch4.tree.uptake %>%
  slice(-train.uptake) %>%
  mutate(fit = predict(tree.uptake.spike, ch4.tree.uptake[-train.uptake,]))
ggplot(tree.uptake.perf, aes(x = ch4.flux.hh, y = fit)) +
  geom_point() +
  geom_smooth(method = 'lm')
cv.tree.uptake <- cv.tree(tree.uptake.spike, FUN = prune.tree)
cv.tree.uptake
plot(cv.tree.uptake)
prune.tree.uptake <- prune.tree(tree.uptake.spike, best = 5)
plot(prune.tree.uptake)
text(prune.tree.uptake, pretty=0)

summary(prune.tree.uptake)

ggplot(model.fit, aes(x = mean.swc)) +
  geom_point(aes(y = ch4.flux.hh, color = 'measurements')) +
  geom_point(aes(y = fit, color = 'predicted'))
ggplot(model.fit, aes(x = percent.thermokarst.ffp)) +
  geom_point(aes(y = ch4.flux.hh, color = 'measurements')) +
  geom_point(aes(y = fit, color = 'predicted'))
ggplot(model.fit, aes(x = ch4.flux.hh, y = fit)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 2))


### Pulse uptake
ggplot(filter(ch4.model.data, spike == 'uptake spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = wind_speed_filter)) +
  geom_point()
ggplot(filter(ch4.model.data, spike == 'uptake spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = wind_speed_filter)) +
  geom_point() +
  facet_grid(wind.speed.groups ~ .) +
  scale_color_viridis()
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_uptake_tk_ws.jpg')
ggplot(filter(ch4.model.data, spike == 'uptake spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = wind_speed_filter)) +
  geom_point() +
  facet_grid(season ~ .) +
  scale_color_viridis() +
  theme_bw()
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/methane_pulse_uptake_tk_ws_season.jpg')
ggplot(filter(ch4.model.data, spike == 'uptake spike'),
       aes(x = percent.thermokarst.ffp, y = ch4.flux.hh, color = month.factor)) +
  geom_point() +
  facet_grid(wind.speed.groups ~ .) +
  scale_color_viridis(discrete = TRUE,
                      option = 'B')
ggplot(filter(ch4.model.data, spike == 'uptake spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = percent.thermokarst.ffp)) +
  geom_point() +
  facet_grid(thermokarst.groups ~ .) +
  scale_color_viridis()
ggplot(filter(ch4.model.data, spike == 'uptake spike'),
       aes(x = wind_speed_filter, y = ch4.flux.hh, color = month.factor)) +
  geom_point() +
  facet_grid(thermokarst.groups ~ .) +
  scale_color_viridis(discrete = TRUE,
                      option = 'B')
########################################################################################################################