##############################################################################################################
###                                Overview Map of GPS Survey                                              ###
###                                    Code by HGR 2/2019                                                  ###
##############################################################################################################

### Load Libraries ###########################################################################################
library(sf)
library(raster)
# library(ggmap)
library(ggthemes)
library(ggnewscale)
library(viridis)
# library(RStoolbox)
# library(ggpubr)
library(tidyverse)
##############################################################################################################

### Load Data ################################################################################################
emldtm <- raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM/NEON_DTM_2017.tif')
crop_extent_final <- extent(matrix(c(387000, 396000, 7080500, 7089500), nrow = 2, byrow = TRUE))
# crop_extent_test <- extent(matrix(c(390500, 390800, 7085800, 7086100), nrow = 2, byrow = TRUE))
# emlrgb17 <- crop(projectRaster(brick('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/NEON/Airborne_Data_2017/2017_HEAL_RGB/QA/Camera/2017_HEAL_1_all_5m_ll_geo.tif'),
#                              crs = crs(emldtm)),
#                y = crop_extent_final)
emlrgb18 <- crop(projectRaster(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2018_HEAL_RGB/QA/Camera/2018_HEAL_2_all_5m_ll_geo.tif'),
                             crs = crs(emldtm)),
               y = crop_extent_final)
# emlrgb19 <- crop(brick('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2019_HEAL_RGB/2019_HEAL_3_all_1m_UTM_geo.tif'),
#                  y = crop_extent_final)
# emlrgb19 <- emlrgb19 <- aggregate(emlrgb19, fact = 5)
# writeRaster(emlrgb19, '/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2019_HEAL_RGB/2019_HEAL_3_all_5m_UTM_geo.tif')
# emlrgb19 <- raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/RGB/2019_HEAL_RGB/2019_HEAL_3_all_5m_UTM_geo.tif')
ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
ec_sf <- st_sf(geometry = ec, crs = 32606)
circle <- st_buffer(ec, dist = 225)
circle_sf <- st_sf(geometry = circle)
eml_wtrshd <- st_read("/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/eml_bnd/boundry_poly3.shp")
thermokarst <- raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/output/karst_combined_1_final.tif')
##############################################################################################################

### Possible to plot RGB data with ggplot? ###################################################################
# stretch raster before conversion to df
# emlrgb17.stretch <- stretch(emlrgb17,
#                             minv = 40,
#                             maxv = 250,
#                             minq = 0,
#                             maxq = 1)
# 
emlrgb18.stretch <- stretch(emlrgb18,
                            minv = 0,
                            maxv = 255,
                            minq = 0.01,
                            maxq = 1)

# convert decimal to hexadecimal
# emlrgb17.df <- as.data.frame(emlrgb17.stretch,
#                            xy = TRUE) %>%
#   rename(r = 3, g = 4, b = 5) %>%
#   mutate(color.hex = factor(rgb(r, g, b, maxColorValue = 255)))
# 
emlrgb18.df <- as.data.frame(emlrgb18.stretch,
                             xy = TRUE) %>%
  rename(r = 3, g = 4, b = 5) %>%
  mutate(color.hex = factor(rgb(r, g, b, maxColorValue = 255)))

# ggplot(emlrgb17.df, aes(x = x, y = y, fill = color.hex)) +
#   geom_raster() +
#   scale_fill_manual(values = levels(emlrgb17.df$color.hex)) +
#   geom_sf(data = ec_sf, inherit.aes = FALSE) +
#   geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
#   geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
#   coord_sf(datum = st_crs(ec_sf),
#            expand = FALSE) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_blank()) # don't try to run without this line, it will destroy everything (i.e. take forever and probably crash RStudio)
# 
# ggplot(emlrgb18.df, aes(x = x, y = y, fill = color.hex)) +
#   geom_raster() +
#   scale_fill_manual(values = levels(emlrgb18.df$color.hex)) +
#   geom_sf(data = ec_sf, inherit.aes = FALSE) +
#   geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
#   geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
#   coord_sf(datum = st_crs(ec_sf),
#            expand = FALSE) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_blank()) # don't try to run without this line, it will destroy everything (i.e. take forever and probably crash RStudio)
##############################################################################################################

### Add Hillshade ############################################################################################
emldtm5 <- aggregate(emldtm, fact = 5)
emlslope5 <- terrain(emldtm5, opt = 'slope')
emlaspect5 <- terrain(emldtm5, opt = 'aspect')
emlhillshade <- crop(hillShade(slope = emlslope5, aspect = emlaspect5, direction = 180), crop_extent_final)
emlhillshade.stretch <- stretch(emlhillshade,
                        minv = 0,
                        maxv = 1,
                        minq = 0.01,
                        maxq = 0.9)

emlhillshd.df <- emlhillshade.stretch %>%
  as.data.frame(xy = TRUE) %>%
  rename(hillshd = 3)

# ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
#   geom_raster() +
#   scale_fill_gradient(low = '#000000', high = '#FFFFFF') +
#   geom_sf(data = ec_sf, inherit.aes = FALSE) +
#   geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
#   geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
#   coord_sf(datum = st_crs(ec_sf),
#            expand = FALSE) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_blank()) # don't try to run without this line, it will destroy everything (i.e. take forever and probably crash RStudio)

site.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF') +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, aes(x = x, y = y, fill = color.hex), inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex)) +
  geom_sf(data = ec_sf, inherit.aes = FALSE) +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank()) # don't try to run without this line, it will destroy everything (i.e. take forever and probably crash RStudio)
site.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/site_map.jpg',
#        site.map,
#        height = 6,
#        width = 7)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/site_map.pdf',
#        site.map,
#        height = 6,
#        width = 7)
##############################################################################################################

### Add in Thermokarst Classification ########################################################################
thermokarst5 <- aggregate(thermokarst, fact = 5)
thermokarst.df <- thermokarst5 %>%
  as.data.frame(xy = TRUE) %>%
  rename(tk = 3) %>%
  mutate(tk = factor(ifelse(is.nan(tk),
                     NA,
                     tk)))

tk.map <- ggplot(emlhillshd.df, aes(x = x, y = y, fill = hillshd)) +
  geom_raster() +
  scale_fill_gradient(low = '#000000', high = '#FFFFFF') +
  new_scale('fill') +
  geom_raster(data = emlrgb18.df, aes(x = x, y = y, fill = color.hex), inherit.aes = FALSE, alpha = 0.8) +
  scale_fill_manual(values = levels(emlrgb18.df$color.hex)) +
  new_scale('fill') +
  geom_raster(data = thermokarst.df,
              aes(x = x, y = y, fill = tk),
              inherit.aes = FALSE) +
  scale_fill_manual(values = c('#463480FF'),
                    na.value = NA) +
  geom_sf(data = ec_sf, inherit.aes = FALSE, color = 'black') +
  geom_sf(data = circle_sf, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  geom_sf(data = eml_wtrshd, inherit.aes = FALSE, fill = 'transparent', color = 'black') +
  coord_sf(datum = st_crs(ec_sf),
           expand = FALSE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank())
tk.map
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_map.jpg',
#        tk.map,
#        height = 6,
#        width = 7)
# ggsave('/home/heidi/Documents/School/NAU/Schuur Lab/Remote Sensing/thermokarst_project/figures/thermokarst_map.pdf',
#        tk.map,
#        height = 6,
#        width = 7)
##############################################################################################################