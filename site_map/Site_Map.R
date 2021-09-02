##############################################################################################################
###                                Overview Map of GPS Survey                                              ###
###                                    Code by HGR 2/2019                                                  ###
##############################################################################################################

### Load Libraries ###########################################################################################
library(sf)
library(lwgeom)
library(raster)
library(ggmap)
library(ggthemes)
library(ggnewscale)
library(viridis)
library(scales)
# library(RStoolbox)
library(ggpubr)
library(ggsn)
library(tidyverse)
##############################################################################################################

### Load Data ################################################################################################
# buffered extent for RGB imagery and hillshade
crop_extent_final <- extent(matrix(c(386500, 396500, 7080000, 7090000), 
                                   nrow = 2, byrow = TRUE))
# emldtm <- crop(raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM/NEON_DTM_2017.tif'),
#                y = crop_extent_final)

# study extent for plotting
extent_sf <- data.frame(x = c(387000, 387000, 396000, 396000),
                        y = c(7080500, 7089500, 7089500, 7080500)) %>%
  st_as_sf(coords = c('x', 'y'),
           crs = st_crs(emldtm)) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast('POLYGON')

# 5 m RGB
emlrgb18 <- crop(projectRaster(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/QA/Camera/2018_HEAL_2_all_5m_ll_geo.tif'),
                             crs = crs(emldtm)),
               y = crop_extent_final)

# ec tower
ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
ec_sf <- st_sf(geometry = ec, crs = 32606)
circle <- st_buffer(ec, dist = 225)
circle_sf <- st_sf(geometry = circle)
rm(ec, circle)

# EML watershed boundary
eml_wtrshd <- st_read("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/eml_bnd/boundry_poly3.shp")

# thermokarst classification
# thermokarst.brick <- brick(raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_raster_final_1.tif'),
#                            raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_raster_final_2.tif'),
#                            raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/output/karst_combined_1_raster_final_3.tif'))
# thermokarst <- calc(thermokarst.brick, max)

# cipehr
cipehr <- st_read('/home/heidi/Documents/School/NAU/Schuur Lab/GPS/All_Points/Site_Summary_Shapefiles/EML_Sites.shp') %>%
  filter(str_detect(Name, 'CiPEHR'))
##############################################################################################################

### Prep RGB Image ###########################################################################################
# stretch raster before conversion to df
emlrgb18.stretch <- stretch(emlrgb18,
                            minv = 0,
                            maxv = 255,
                            minq = 0.01,
                            maxq = 1)

# convert decimal to hexadecimal
emlrgb18.df <- as.data.frame(emlrgb18.stretch,
                             xy = TRUE) %>%
  rename(r = 3, g = 4, b = 5) %>%
  filter(!(is.na(r) | is.na(g) | is.na(b))) %>%
  mutate(color.hex = factor(rgb(r, g, b, maxColorValue = 255)))

# get RGB imagery for zoomed in plots
# pond (x > 389100 & x < 390100) & (y > 7088500 & y < 7089500)
pond.extent <- extent(matrix(c(389100, 390100, 7088500, 7089500), 
                             nrow = 2, byrow = TRUE))
rgb.pond <- crop(merge(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7088000_image.tif'),
                       brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7088000_image.tif'),
                       brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7089000_image.tif'),
                       brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7089000_image.tif')), 
                 pond.extent)
writeRaster(rgb.pond,
             '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/pond_rgb.tif')

# stretch raster before conversion to df
rgb.pond.stretch <- stretch(rgb.pond,
                            minv = 0,
                            maxv = 255,
                            minq = 0.01,
                            maxq = 1)

# convert decimal to hexadecimal
rgb.pond.df <- as.data.frame(rgb.pond.stretch,
                             xy = TRUE) %>%
  rename(r = 3, g = 4, b = 5) %>%
  filter(!(is.na(r) | is.na(g) | is.na(b))) %>%
  mutate(color.hex = factor(rgb(r, g, b, maxColorValue = 255)))

# moraine and cipehr (x > 389900 & x < 390900) & (y > 7085500 & y < 7086500)
moraine.extent <- extent(matrix(c(389900, 390900, 7085500, 7086500), 
                                nrow = 2, byrow = TRUE))
rgb.moraine <- crop(merge(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7085000_image.tif'),
                          brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7085000_image.tif'),
                          brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7086000_image.tif'),
                          brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_390000_7086000_image.tif')), 
                    moraine.extent)
writeRaster(rgb.moraine,
            '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/moraine_rgb.tif')

# stretch raster before conversion to df
rgb.moraine.stretch <- stretch(rgb.moraine,
                               minv = 0,
                               maxv = 255,
                               minq = 0.01,
                               maxq = 1)

# convert decimal to hexadecimal
rgb.moraine.df <- as.data.frame(rgb.moraine.stretch,
                                xy = TRUE) %>%
  rename(r = 3, g = 4, b = 5) %>%
  filter(!(is.na(r) | is.na(g) | is.na(b))) %>%
  mutate(color.hex = factor(rgb(r, g, b, maxColorValue = 255)))

# gradient (x > 388900 & x < 389900) & (y > 7085250 & y < 7086250)
gradient.extent <- extent(matrix(c(388900, 389900, 7085250, 7086250), 
                                 nrow = 2, byrow = TRUE))
rgb.gradient <- crop(merge(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_388000_7085000_image.tif'),
                           brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7085000_image.tif'),
                           brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_388000_7086000_image.tif'),
                           brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/L3/Camera/Mosaic/V01/2018_HEAL_2_389000_7086000_image.tif')), 
                     gradient.extent)
writeRaster(rgb.gradient,
            '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/gradient_rgb.tif')

# stretch raster before conversion to df
rgb.gradient.stretch <- stretch(rgb.gradient,
                                minv = 0,
                                maxv = 255,
                                minq = 0.01,
                                maxq = 1)

# convert decimal to hexadecimal
rgb.gradient.df <- as.data.frame(rgb.gradient.stretch,
                                 xy = TRUE) %>%
  rename(r = 3, g = 4, b = 5) %>%
  filter(!(is.na(r) | is.na(g) | is.na(b))) %>%
  mutate(color.hex = factor(rgb(r, g, b, maxColorValue = 255)))
##############################################################################################################

### Prep Hillshade ###########################################################################################
# emldtm5 <- aggregate(emldtm, fact = 5)
# writeRaster(emldtm5,
#             '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_dtm5_for_plotting.tif')
emldtm5 <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_dtm5_for_plotting.tif')
emldtm5.df <- emldtm5 %>%
  as.data.frame(xy = TRUE) %>%
  rename(elevation = 3)
# emlslope5 <- terrain(emldtm5, opt = 'slope')
# emlaspect5 <- terrain(emldtm5, opt = 'aspect')
# emlhillshade <- crop(hillShade(slope = emlslope5, aspect = emlaspect5, direction = 180), crop_extent_final)
# emlhillshade.stretch <- stretch(emlhillshade,
#                         minv = 0,
#                         maxv = 1,
#                         minq = 0.01,
#                         maxq = 0.9)
# writeRaster(emlhillshade.stretch,
#             '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_hillshade_for_plotting.tif')
emlhillshade.stretch <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_hillshade_for_plotting.tif')
emlhillshd.df <- emlhillshade.stretch %>%
  as.data.frame(xy = TRUE) %>%
  rename(hillshd = 3)
##############################################################################################################

### Prep Thermokarst Raster ##################################################################################
# to plot features only
# thermokarst5 <- aggregate(thermokarst, fact = 5)
# writeRaster(thermokarst5,
#             '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_tk5_for_plotting.tif')
thermokarst5 <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_tk5_for_plotting.tif')
thermokarst.df <- thermokarst5 %>%
  as.data.frame(xy = TRUE) %>%
  rename(tk = 3) %>%
  mutate(tk = factor(ifelse(is.nan(tk),
                            NA,
                            tk)))
##############################################################################################################

### Prep Depth Raster ##################################################################################
# cell by cell depth
depth5 <- raster('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/eml_depth5_for_plotting.tif')
depth.df <- depth5 %>%
  as.data.frame(xy = TRUE) %>%
  rename(depth = 3) %>%
  mutate(depth = ifelse(is.nan(depth),
                        NA,
                        ifelse(depth > 0,
                               0,
                               depth*-1))) %>%
  filter(!is.na(depth))

# average depth by feature
# karst_1_stats_sf <- read_sf('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/karst_1_stats.shp') %>%
#   rename(min.depth = min_d,
#          mean.depth = mean_d,
#          median.depth = med_d,
#          max.depth = max_d,
#          sd.depth = sd_d,
#          se.depth = se_d,
#          min.depth.clean = min_d_c,
#          mean.depth.clean = mean_d_c,
#          median.depth.clean = med_d_c,
#          max.depth.clean = max_d_c,
#          sd.depth.clean = sd_d_c,
#          se.depth.clean= se_d_c)
# karst_1_stats_sf <- karst_1_stats_sf %>%
#   mutate(volume = size*mean.depth,
#          shape = as.numeric(4*pi*st_area(karst_1_stats_sf)/st_perimeter(karst_1_stats_sf)^2))
# 
# karst_depth_sf <- karst_1_stats_sf %>%
#   select(year, mean.depth)
# tk.mean.depth <- brick(rasterize(as(filter(karst_depth_sf, year == 2017), 'Spatial'),
#                                  emldtm5,
#                                  field = 'mean.depth'),
#                        rasterize(as(filter(karst_depth_sf, year == 2018), 'Spatial'),
#                                  emldtm5,
#                                  field = 'mean.depth'),
#                        rasterize(as(filter(karst_depth_sf, year == 2019), 'Spatial'),
#                                  emldtm5,
#                                  field = 'mean.depth'))
# writeRaster(tk.mean.depth,
#             '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_mean_depth.tif')
tk.mean.depth <- brick('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_mean_depth.tif')
tk.mean.depth.mean <- calc(tk.mean.depth, mean, na.rm = TRUE)
tk.mean.depth.df <- tk.mean.depth.mean %>%
  as.data.frame(xy = TRUE) %>%
  rename(mean.depth = 3) %>%
  mutate(mean.depth = ifelse(is.nan(mean.depth),
                             NA,
                             ifelse(mean.depth > 0,
                                    0, 
                                    mean.depth*-1))) %>%
  filter(!is.na(mean.depth))
##############################################################################################################

### Plot with Hillshade and RGB ##############################################################################
site.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF') +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, aes(x = x, y = y, fill = color.hex), inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex)) +
  geom_sf(data = ec_sf, inherit.aes = FALSE) +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  scale_x_continuous(name = 'Longitude (m)',
                     limits = c(386500, 396500)) +
  scale_y_continuous(name = 'Latitude (m)',
                     limits = c(7080000, 7090000)) +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE) +
  theme_bw() +
  theme(legend.position = "none", # don't try to run without this line, it will destroy everything (i.e. take forever and probably crash RStudio)
        axis.title = element_blank(),
        axis.text = element_blank())
        
site.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/site_map.jpg',
#        site.map,
#        height = 3.25,
#        width = 4)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/site_map.pdf',
#        site.map,
#        height = 3.25,
#        width = 4)
##############################################################################################################

### Add in Thermokarst Classification ########################################################################
tk.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, aes(x = x, y = y, fill = color.hex), inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = thermokarst.df,
              aes(x = x, y = y, fill = tk),
              inherit.aes = FALSE) +
  scale_fill_manual(values = c('#463480FF'),
                    na.value = NA) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())
tk.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_map.jpg',
#        tk.map,
#        height = 5.5,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_map.pdf',
#        tk.map,
#        height = 5.5,
#        width = 6.5)

labels <- data.frame(x = c(387000),
                     y = c(7089500),
                     labels = c('Study Extent'))
stretch.factor <- 1/2
tk.depth.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = depth.df,
              aes(x = x, y = y, fill = depth^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nDepth (m)',
                     option = 'C',
                     limits = c(0, 1)^(stretch.factor),
                     breaks = c(seq(0, 1, by = 0.1)^(stretch.factor)),
                     labels = c(seq(0, 0.9, by = 0.1), '1+'),
                     oob = squish,
                     direction = -1) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = cipehr, inherit.aes = FALSE, fill = 'transparent', color = 'gray80') +
  # geom_text(data = labels, 
  #           aes(x = x, y = y, label = labels),
  #           hjust = 0,
  #           vjust = -0.2,
  #           inherit.aes = FALSE) +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  coord_sf(clip = "off",
           datum = st_crs(ec_sf),
           expand = FALSE) +
  theme_bw() +
  theme(legend.key.height = unit(2, 'lines')) +
  north(data = extent_sf, scale = 0.05, symbol = 12, anchor = c('x' = 396470, 'y' = 7089970))
tk.depth.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_depth_map_v1.jpg',
#        tk.depth.map,
#        height = 6,
#        width = 7.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_depth_map_v1.pdf',
#        tk.depth.map,
#        height = 6,
#        width = 7.5)

tk.depth.elev.map <- ggplot(emldtm5.df, aes(x = x, y = y, fill = elevation)) +
  geom_raster() +
  scale_fill_gradient(name = 'Elevation',
                      low = '#333333', high = '#FFFFFF') +
  new_scale('fill') +
  geom_raster(data = depth.df,
              aes(x = x, y = y, fill = depth^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nDepth (m)',
                     option = 'C',
                     limits = c(0, 1)^(stretch.factor),
                     breaks = c(seq(0, 1, by = 0.2)^(stretch.factor)),
                     labels = c(seq(0, 0.9, by = 0.2), '1+'),
                     oob = squish,
                     direction = -1) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_text(data = labels, 
            aes(x = x, y = y, label = labels),
            hjust = 0,
            vjust = -0.2,
            inherit.aes = FALSE) +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE) +
  theme_bw() +
  theme()
tk.depth.elev.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_depth_map_v2.jpg',
#        tk.depth.elev.map,
#        height = 5.5,
#        width = 7.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_depth_map_v2.pdf',
#        tk.depth.elev.map,
#        height = 5.5,
#        width = 7.5)

### Combine site map with classification on elevation
tk.elev.map <- ggplot(emldtm5.df, aes(x = x, y = y, fill = elevation)) +
  geom_raster() +
  scale_fill_gradient(name = 'Elevation\n(m)',
                      low = '#101010', high = '#FFFFFF',
                      breaks = c(500, 1000)) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  coord_sf(clip = "off",
           datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(min(emldtm5.df$x), max(emldtm5.df$x)),
           ylim = c(min(emldtm5.df$y), max(emldtm5.df$y))) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key.height = unit(0.1, 'inches'),
        legend.key.width = unit(0.1, 'inches'),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = 'bottom') +
  geom_text(aes(x = 385000, y = 7090000, label = 'B'))
tk.elev.map

tk.depth.elev.rgb <- tk.depth.map +
  theme(legend.justification = 'top') +
  annotation_custom(ggplotGrob(tk.elev.map),
                    xmin = 396510,
                    xmax = 399500,
                    ymin = 7080010,
                    ymax = 7084000)

tk.depth.elev.rgb
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_depth_map_v4.jpg',
#        tk.depth.elev.rgb,
#        height = 4.75,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_depth_map_v4.pdf',
#        tk.depth.elev.rgb,
#        height = 4.75,
#        width = 6.5)
##############################################################################################################

### Map of Mean Depth ########################################################################################
labels <- data.frame(x = c(387000),
                     y = c(7089500),
                     labels = c('Study Extent'))

stretch.factor <- 1/2
tk.depth.map <- ggplot(data = emlhillshd.df, aes(x = x, y = y)) +
  geom_raster(aes(fill = hillshd)) +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = tk.mean.depth.df,
              aes(x = x, y = y, fill = mean.depth^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nMean Depth (m)',
                     option = 'C',
                     limits = c(0, 1)^(stretch.factor),
                     breaks = c(seq(0, 1, by = 0.1)^(stretch.factor)),
                     labels = c(seq(0, 0.9, by = 0.1), '1+'),
                     oob = squish,
                     direction = -1) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = cipehr, inherit.aes = FALSE, fill = 'transparent', color = 'gray80') +
  # scale_x_continuous(name = 'Longitude (m)') +
  # scale_y_continuous(name = 'Latitude (m)') +
  coord_sf(clip = "off",
           datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(min(emlrgb18.df$x), max(emlrgb18.df$x)),
           ylim = c(min(emlrgb18.df$y), max(emlrgb18.df$y))) +
  theme_bw() +
  theme(legend.key.height = unit(2, 'lines'),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  north(data = extent_sf, scale = 0.05, symbol = 12, anchor = c('x' = 396470, 'y' = 7089900)) +
  geom_text(aes(x = 3964700, y = 7089970, label = 'N')) +
  scalebar(location = "bottomleft", anchor = c(x = 386750, y = 7080250), 
           dist = 500, dist_unit = 'm', transform = FALSE, st.size = 4, st.dist = 0.01,
           x.min = min(emlrgb18.df$x),
           x.max = max(emlrgb18.df$x),
           y.min = min(emlrgb18.df$y),
           y.max = max(emlrgb18.df$y))
  # geom_text(aes(x = 385000, y = 7090000, label = 'A'))
tk.depth.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_depth_map.jpg',
#        tk.depth.map,
#        height = 6,
#        width = 7.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_depth_map.pdf',
#        tk.depth.map,
#        height = 6,
#        width = 7.5)

### Zoom in on specific features
### Will need to redo with 1 m data!
# Thaw pond N of EML
ggplot(data = filter(emlhillshd.df, 
                     (x > 389100 & x < 390100) & (y > 7088500 & y < 7089500)), 
       aes(x = x, y = y)) +
  geom_raster(aes(fill = hillshd)) +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = filter(emlrgb18.df, 
                            (x > 389100 & x < 390100) & (y > 7088500 & y < 7089500)), 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(breaks = levels(filter(emlrgb18.df, 
                                           (x > 389100 & x < 390100) & (y > 7088500 & y < 7089500))$color.hex),
                    values = levels(filter(emlrgb18.df, 
                                           (x > 389100 & x < 390100) & (y > 7088500 & y < 7089500))$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = filter(tk.mean.depth.df, 
                            (x > 389100 & x < 390100) & (y > 7088500 & y < 7089500)),
              aes(x = x, y = y, fill = mean.depth^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nMean Depth (m)',
                     option = 'C',
                     limits = c(0, 1)^(stretch.factor),
                     breaks = c(seq(0, 1, by = 0.1)^(stretch.factor)),
                     labels = c(seq(0, 0.9, by = 0.1), '1+'),
                     oob = squish,
                     direction = -1) +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(389100, 390100),
           ylim = c(7088500, 7089500)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scalebar(location = "bottomleft", anchor = c(x = 389150, y = 7088550), 
           dist = 100, dist_unit = 'm', transform = FALSE, st.size = 4, 
           height = 0.005, st.dist = 0.0025,
           x.min = min(emlrgb18.df$x),
           x.max = max(emlrgb18.df$x),
           y.min = min(emlrgb18.df$y),
           y.max = max(emlrgb18.df$y))

# Terminal Moraine and CiPEHR
# extent  389900, 390900, 7085900, 7086900
ggplot(data = filter(emlhillshd.df, 
                     (x > 389900 & x < 390900) & (y > 7085500 & y < 7086500)), 
       aes(x = x, y = y)) +
  geom_raster(aes(fill = hillshd)) +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = filter(emlrgb18.df, 
                            (x > 389900 & x < 390900) & (y > 7085500 & y < 7086500)), 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(breaks = levels(filter(emlrgb18.df, 
                                           (x > 389900 & x < 390900) & (y > 7085500 & y < 7086500))$color.hex),
                    values = levels(filter(emlrgb18.df, 
                                           (x > 389900 & x < 390900) & (y > 7085500 & y < 7086500))$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = filter(tk.mean.depth.df, 
                            (x > 389900 & x < 390900) & (y > 7085500 & y < 7086500)),
              aes(x = x, y = y, fill = mean.depth^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nMean Depth (m)',
                     option = 'C',
                     limits = c(0, 1)^(stretch.factor),
                     breaks = c(seq(0, 1, by = 0.1)^(stretch.factor)),
                     labels = c(seq(0, 0.9, by = 0.1), '1+'),
                     oob = squish,
                     direction = -1) +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(389900, 390900),
           ylim = c(7085500, 7086500)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scalebar(location = "bottomleft", anchor = c(x = 389950, y = 7085550), 
           dist = 100, dist_unit = 'm', transform = FALSE, st.size = 4, 
           height = 0.005, st.dist = 0.0025,
           x.min = min(emlrgb18.df$x),
           x.max = max(emlrgb18.df$x),
           y.min = min(emlrgb18.df$y),
           y.max = max(emlrgb18.df$y))

# Gradient
# extent  389000, 390000, 7085000, 7086000
ggplot(data = filter(emlhillshd.df, 
                     (x > 388900 & x < 389900) & (y > 7085250 & y < 7086250)), 
       aes(x = x, y = y)) +
  geom_raster(aes(fill = hillshd)) +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = filter(emlrgb18.df, 
                            (x > 388900 & x < 389900) & (y > 7085200 & y < 7086200)), 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(breaks = levels(filter(emlrgb18.df, 
                                           (x > 388900 & x < 389900) & (y > 7085200 & y < 7086200))$color.hex),
                    values = levels(filter(emlrgb18.df, 
                                           (x > 388900 & x < 389900) & (y > 7085200 & y < 7086200))$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = filter(tk.mean.depth.df, 
                            (x > 388900 & x < 389900) & (y > 7085200 & y < 7086200)),
              aes(x = x, y = y, fill = mean.depth^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nMean Depth (m)',
                     option = 'C',
                     limits = c(0, 1)^(stretch.factor),
                     breaks = c(seq(0, 1, by = 0.1)^(stretch.factor)),
                     labels = c(seq(0, 0.9, by = 0.1), '1+'),
                     oob = squish,
                     direction = -1) +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(388900, 389900),
           ylim = c(7085200, 7086200)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scalebar(location = "bottomleft", anchor = c(x = 388950, y = 7085250), 
           dist = 100, dist_unit = 'm', transform = FALSE, st.size = 4, 
           height = 0.005, st.dist = 0.0025,
           x.min = min(emlrgb18.df$x),
           x.max = max(emlrgb18.df$x),
           y.min = min(emlrgb18.df$y),
           y.max = max(emlrgb18.df$y))


tk.depth.elev.rgb <- tk.depth.map +
  theme(legend.justification = 'top') +
  annotation_custom(ggplotGrob(tk.elev.map),
                    xmin = 396510,
                    xmax = 399500,
                    ymin = 7080010,
                    ymax = 7084000)

tk.depth.elev.rgb
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_depth_elev_map.jpg',
#        tk.depth.elev.rgb,
#        height = 4.75,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_depth_elev_map.pdf',
#        tk.depth.elev.rgb,
#        height = 4.75,
#        width = 6.5)
##############################################################################################################

### Map of Shape #############################################################################################
# karst_shape_sf <- karst_1_stats_sf %>%
#   select(year, shape)
# tk.shape <- brick(rasterize(as(filter(karst_shape_sf, year == 2017), 'Spatial'),
#                             emldtm5,
#                             field = 'shape'),
#                   rasterize(as(filter(karst_shape_sf, year == 2018), 'Spatial'),
#                             emldtm5,
#                             field = 'shape'),
#                   rasterize(as(filter(karst_shape_sf, year == 2019), 'Spatial'),
#                             emldtm5,
#                             field = 'shape'))
# writeRaster(tk.shape,
#             '/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_shape.tif')
tk.shape <- brick('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/analysis/thermokarst_shape.tif')
tk.mean.shape <- calc(tk.shape, mean, na.rm = TRUE)

tk.mean.shape.df <- tk.mean.shape %>%
  as.data.frame(xy = TRUE) %>%
  rename(mean.shape = 3) %>%
  mutate(mean.shape = ifelse(is.nan(mean.shape),
                             NA,
                             mean.shape)) %>%
  filter(!is.na(mean.shape))

stretch.factor <- 1/3
tk.mean.shape.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = tk.mean.shape.df,
              aes(x = x, y = y, fill = mean.shape^(stretch.factor)),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nMean Shape',
                     option = 'C',
                     limits = c(0, 
                                max(tk.mean.shape.df$mean.shape))^(stretch.factor),
                     breaks = c(seq(0, 0.7, by = 0.1)^(stretch.factor)),
                     labels = seq(0, 0.7, by = 0.1),
                     direction = -1) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = cipehr, inherit.aes = FALSE, fill = 'transparent', color = 'gray80') +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  coord_sf(clip = "off",
           datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(min(emlrgb18.df$x), max(emlrgb18.df$x)),
           ylim = c(min(emlrgb18.df$y), max(emlrgb18.df$y))) +
  theme_bw() +
  theme(legend.key.height = unit(2, 'lines')) +
  north(data = extent_sf, scale = 0.05, symbol = 12, anchor = c('x' = 396470, 'y' = 7089970))# +
  # geom_text(aes(x = 385000, y = 7090000, label = 'A'))
tk.mean.shape.map

# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_shape_map.jpg',
#        tk.mean.shape.map,
#        height = 6,
#        width = 7.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_shape_map.pdf',
#        tk.mean.shape.map,
#        height = 6,
#        width = 7.5)

tk.mean.shape.elev.rgb <- tk.mean.shape.map +
  theme(legend.justification = 'top') +
  annotation_custom(ggplotGrob(tk.elev.map),
                    xmin = 396510,
                    xmax = 399500,
                    ymin = 7080010,
                    ymax = 7084000)

tk.mean.shape.elev.rgb
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_shape_elev_map_2018.jpg',
#        tk.mean.shape.elev.rgb,
#        height = 4.75,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_mean_shape_elev_map_2018.pdf',
#        tk.mean.shape.elev.rgb,
#        height = 4.75,
#        width = 6.5)

# 2018 only
tk.shape.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF',
                      guide = FALSE) +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, 
              aes(x = x, y = y, fill = color.hex), 
              inherit.aes = FALSE, 
              alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex),
                    guide = FALSE) +
  new_scale('fill') +
  geom_sf(data = filter(karst_1_stats_sf, year == 2018),
              aes(color = shape, fill = shape),
              inherit.aes = FALSE) +
  scale_fill_viridis(name = 'Thermokarst\nShape',
                     option = 'C') +
  scale_color_viridis(name = 'Thermokarst\nShape',
                     option = 'C') +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = extent_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = cipehr, inherit.aes = FALSE, fill = 'transparent', color = 'gray80') +
  scale_x_continuous(name = 'Longitude (m)') +
  scale_y_continuous(name = 'Latitude (m)') +
  coord_sf(clip = "off",
           datum = st_crs(ec_sf),
           expand = FALSE,
           xlim = c(min(emlhillshd.df$x), max(emlhillshd.df$x))) +
  theme_bw() +
  north(data = extent_sf, scale = 0.05, symbol = 12, anchor = c('x' = 396470, 'y' = 7089970)) +
  # geom_text(aes(x = 385000, y = 7090000, label = 'A'))
tk.shape.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_shape_map_2018.jpg',
#        tk.shape.map,
#        height = 6,
#        width = 7.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_shape_map_2018.pdf',
#        tk.shape.map,
#        height = 6,
#        width = 7.5)

tk.shape.elev.rgb <- tk.shape.map +
  theme(legend.justification = 'top') +
  annotation_custom(ggplotGrob(tk.elev.map),
                    xmin = 396510,
                    xmax = 399500,
                    ymin = 7080010,
                    ymax = 7084000)

tk.shape.elev.rgb
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_shape_elev_map_2018.jpg',
#        tk.shape.elev.rgb,
#        height = 4.75,
#        width = 6.5)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_shape_elev_map_2018.pdf',
#        tk.shape.elev.rgb,
#        height = 4.75,
#        width = 6.5)
##############################################################################################################