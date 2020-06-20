########################################################################################################################
###                           Subsidence/Thermokarst Analysis from NEON Data                                         ###
###                                         Code by HGR 5/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(tidyverse)
library(sf)
library(lwgeom)
########################################################################################################################

### Load Data ##########################################################################################################
sub <- brick(stack('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2018.tif',
                   'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/Subsidence/subsidence_2017_2019.tif'))

karst_1 <- brick(stack("/scratch/hgr7/int_output/karst_combined_1_filter_1.tif",
                       "/scratch/hgr7/int_output/karst_combined_1_filter_2.tif",
                       "/scratch/hgr7/int_output/karst_combined_1_filter_3.tif"))
karst_3 <- brick(stack("/scratch/hgr7/int_output/karst_combined_3_filter_1.tif",
                       "/scratch/hgr7/int_output/karst_combined_3_filter_2.tif",
                       "/scratch/hgr7/int_output/karst_combined_3_filter_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')
crs(karst_3) <- CRS('+init=epsg:32606')

eml_wtrshd <- st_read("/scratch/hgr7/eml_bnd/boundry_poly3.shp")

filenames <- list.files('/scratch/hgr7/output',
                        full.names = TRUE,
                        pattern = 'shp$')

karst_1_poly <- map(filenames[which(str_detect(filenames, pattern = 'karst_combined_1'))],
                    ~ st_read(.x))
names(karst_1_poly) <- c(2017, 2018, 2019)

karst_3_poly <- map(filenames[which(str_detect(filenames, pattern = 'karst_combined_3'))],
                    ~ st_read(.x))
names(karst_3_poly) <- c(2017, 2018, 2019)

filenames <- list.files('/scratch/hgr7/int_output',
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
########################################################################################################################

### Calculate Thermokarst Coverage #####################################################################################
# calculate area (each cell is 1 m^2, so just getting the number of cells is the area in m^2)
karst_area <- karst %>%
  as.data.frame() %>%
  rename(class.2017 = 1, class.2018 = 2, class.2019 = 3) %>%
  gather(key = 'year', value = 'thermokarst', 1:3) %>%
  mutate(year = as.numeric(str_sub(year, 7)),
         thermokarst = ifelse(thermokarst == 0,
                              'undisturbed',
                              ifelse(thermokarst == 1,
                                     'thermokarst',
                                     NA))) %>%
  group_by(year, thermokarst) %>%
  summarise(n = n())

# summarize
karst_cover <- karst_area %>%
  filter(!is.na(thermokarst)) %>%
  spread(key = 'thermokarst', value = 'n') %>%
  mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))

# calculate thermokarst for eml watershed
karst_eml_df <- mask(karst, as(eml_wtrshd, 'Spatial')) %>%
  as.data.frame(xy = TRUE) %>%
  rename(class.2017 = 3, class.2018 = 4, class.2019 = 5)

karst_eml_area <- karst_eml_df %>%
  filter(!is.na(class.2017)) %>%
  gather(key = 'year', value = 'thermokarst', 3:5) %>%
  mutate(year = as.numeric(str_sub(year, 7)),
         thermokarst = ifelse(thermokarst == 0,
                              'undisturbed',
                              ifelse(thermokarst == 1,
                                     'thermokarst',
                                     NA))) %>%
  group_by(year, thermokarst) %>%
  summarise(n = n())

karst_eml_cover <- karst_eml_area %>%
  filter(!is.na(thermokarst)) %>%
  spread(key = 'thermokarst', value = 'n') %>%
  mutate(percent.thermokarst = thermokarst/(thermokarst + undisturbed))
########################################################################################################################

### Identify Thermokarst Edges #########################################################################################
karst_na <- karst
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

# combine edges with thermokarst classification (1 = thermokarst, 2 = thermokarst edge)
# and then extract thermokarst values from that
karst_edges <- brick(karst[[1]] + edges_0[[1]],
                     karst[[2]] + edges_0[[2]],
                     karst[[3]] + edges_0[[3]])

plot(karst_edges[[1]])
plot(karst_edges[[2]])
plot(karst_edges[[3]])
########################################################################################################################

### Mixed Effects Model of Subsidence by Thermokarst Class Over Time ###################################################
# take stratified random sample of cells (currently set up for non-thermokarst, thermokarst center and thermokarst edges)
set.seed(333)
samples <- st_as_sf(sampleStratified(karst_edges[[1]], size = 1000, xy = TRUE, sp = TRUE)) %>%
  select(-4)
ggplot(samples, aes(x = x, y = y)) +
  geom_point() +
  coord_fixed()

# extract values from subsidence brick
# use a buffer to average out erroneous very high/low reads
# but this averages some cells that may not be of the same class - should try to find a way to fix?
# averaging the subsidence for the entire thermokarst feature that the point falls within would be a possibility
# for non-thermokarst points, a buffer could be conditional upon the cells not being thermokarst
# I would have to use both the subsidence and the thermokarst layers and I don't think raster::extract
# would work
sub_extract <- st_as_sf(raster::extract(sub, as(samples, 'Spatial'), buffer = 1.5, layer = 1, nl = 2, sp = TRUE)) %>%
  rename(sub.2018 = 4,
         sub.2019 = 5) %>%
  mutate(sub.2017 = 0) %>%
  st_drop_geometry() %>%
  gather(key = year, value = sub, sub.2018:sub.2017) %>%
  mutate(year = as.numeric(str_sub(year, 5))) %>%
  arrange(year, cell)

# extract values from thermokarst classification brick
# use a buffer to 
karst_extract <- st_as_sf(raster::extract(karst_edges, as(samples, 'Spatial'), layer = 1, nl = 3, sp = TRUE)) %>%
  rename(tk.2017 = 4,
         tk.2018 = 5,
         tk.2019 = 6) %>%
  st_drop_geometry() %>%
  gather(key = year, value = thermokarst, tk.2017:tk.2019) %>%
  mutate(year = as.numeric(str_sub(year, 4))) %>%
  arrange(year, cell)

# join subsidence and thermokarst extract dataframes
sub_karst <- karst_extract %>%
  full_join(sub_extract, by = c('cell', 'x', 'y', 'year')) %>%
  group_by(year, cell, x, y) %>%
  summarise(thermokarst = round(mean(thermokarst)),
            sub = mean(sub))

ggplot(sub_karst, aes(x = year, y = sub, color = as.factor(thermokarst))) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm')
########################################################################################################################

### Create EC Tower Footprint Slices ###################################################################################
# # point for ec tower location
# ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
# ec_sf <- st_sf(geometry = ec, crs = 32606)
# 
# # create a circle around the ec tower with radius = 200 m
# circle <- st_buffer(ec, dist = 200)
# circle_sf <- st_sf(geometry = circle)
# 
# # create 360 lines at 1 degree angles around ec tower that reach to the circle
# # start by creating a single line the length of the diameter
# line <- st_sfc(st_linestring(matrix(c(389589.25, 389189.25, 7085586.3, 7085586.3), nrow = 2)), crs = 32606)
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
# # split each line into two halves at the ec tower
# # and repeat the first line at the end to make sure that the last polygon isn't missing
# ec_snap <- st_snap(ec_sf, line_sf, tol = 1e-9)
# split_lines <- st_collection_extract(st_split(line_sf$geometry,
#                                               ec_snap$geometry),
#                                      'LINESTRING')
# # add one more line - this doesn't add in the last, missing polygon when I split the circle...
# # split_lines[[361]] <- split_lines[[1]]
# 
# # split the circle using the lines to create polygons
# # this is losing one polygon, though...
# # and it appears to be a polygon in the middle of the list of geometries
# split_lines <- st_snap(split_lines, circle_sf, tol = 0.1)
# wedges <- st_as_sf(st_collection_extract(st_split(circle_sf$geometry,
#                                                   split_lines),
#                                          "POLYGON"))
# 
# wedges_sf <- wedges %>%
#   mutate(n = seq(1:360))
# 
# ggplot() + geom_sf(data = wedges_sf, aes(color = n)) + geom_sf(data = ec_sf) + geom_sf(data = circle_sf, fill = NA)
# 
# st_write(wedges_sf, 'Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/wedges_poly.shp')
wedges_sf <- st_read('Z:/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/Heidi_Thermokarst_Data/analysis/wedges_poly.shp')

ggplot() + geom_sf(data = wedges_sf, aes(color = n)) + coord_sf(datum = st_crs(32606))

# tower_extract <- raster::extract(karst_1, as(wedges_sf, 'Spatial'), layer = 1, nl = 3, cellnumbers = TRUE, df = TRUE) %>%
#   as.data.frame()
# tower_extract_neat <- tower_extract %>%
#   rename(karst.2017 = 3,
#          karst.2018 = 4,
#          karst.2019 = 5) %>%
#   group_by(ID) %>%
#   summarise(karst.2017 = sum(karst.2017)/n(),
#             karst.2018 = sum(karst.2018)/n(),
#             karst.2019 = sum(karst.2019)/n()) %>%
#   pivot_longer(cols = karst.2017:karst.2019, names_to = 'year', values_to = 'karst.percent') %>%
#   mutate(year = as.numeric(str_sub(year, 7)))
# # write.csv(tower_extract_neat, '/scratch/hgr7/analysis/tower_extract.csv')
# 
# tower_karst_sf <- tower_extract %>%
#   rename(n = ID, karst.2017 = 3, karst.2018 = 4, karst.2019 = 5) %>%
#   group_by(n) %>%
#   summarise(karst.percent.2017 = sum(karst.2017)/n(),
#             karst.percent.2018 = sum(karst.2018)/n(),
#             karst.percent.2019 = sum(karst.2019)/n()) %>%
#   full_join(wedges_sf, by = c('n'))
# st_write(tower_karst_sf, '/scratch/hgr7/analysis/tower_extract.shp')
tower_karst_sf <- st_read('/scratch/hgr7/analysis/tower_extract.shp') %>%
  rename(karst.percent.2017 = 2, karst.percent.2018 = 3, karst.percent.2019 = 4)

# ggplot(tower_extract_neat, aes(x = year, y = karst.percent, color = ID, group = ID)) +
#   geom_point() +
#   geom_line()


ggplot() + 
  geom_sf(data = tower_karst_sf,
          aes(geometry = geometry,
              color = karst.percent.2017,
              fill = karst.percent.2017)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.6)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.6))

ggplot() + 
  geom_sf(data = tower_karst_sf,
          aes(geometry = geometry,
              color = karst.percent.2018,
              fill = karst.percent.2018)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.6)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.6))

ggplot() + 
  geom_sf(data = tower_karst_sf,
          aes(geometry = geometry,
              color = karst.percent.2019,
              fill = karst.percent.2019)) + 
  coord_sf(datum = st_crs(32606)) +
  scale_color_viridis(direction = -1,
                      limits = c(0, 0.6)) +
  scale_fill_viridis(direction = -1,
                     limits = c(0, 0.6))

########################################################################################################################

### Summary Statistics From Polygons ###################################################################################
n_features <- map_df(karst_1_poly, ~ nrow(.x)) %>%
  pivot_longer(`2017`:`2019`, names_to = 'year', values_to = 'n_karst_1') %>%
  full_join(map_df(karst_3_poly, ~ nrow(.x)) %>%
              pivot_longer(`2017`:`2019`, names_to = 'year', values_to = 'n_karst_3'),
            vy = c('year'))

karst_1_depth <- map2_df(mtopo,
                         karst_1_poly,
                         ~ st_as_sf(raster::extract(.x,
                                                    as(.y, 'Spatial'),
                                                    layer = 1,
                                                    nl = 3,
                                                    sp = TRUE)) %>%
                           rename(depth.15 = 4,
                                  depth.25 = 5,
                                  depth.35 = 6) %>%
                           mutate(year = i + 2016))

write.csv(karst_1_depth, '/scratch/hgr7/output/karst_1_depth.csv', row.names = FALSE)
########################################################################################################################
