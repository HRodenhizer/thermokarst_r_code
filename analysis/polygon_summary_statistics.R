########################################################################################################################
###                           Thermokarst Summary Statistics from Polygons                                         ###
###                                         Code by HGR 6/2020                                                       ###
########################################################################################################################

### Load Libraries #####################################################################################################
library(raster)
library(tidyverse)
library(sf)
########################################################################################################################

### Load Data ##########################################################################################################
filenames <- list.files('/scratch/hgr7/output',
                        full.names = TRUE,
                        pattern = 'shp$')

karst_1_poly <- map(filenames[which(str_detect(filenames, pattern = 'karst_combined_1_poly_fill_\\d'))],
                    ~ st_read(.x))
names(karst_1_poly) <- c(2017, 2018, 2019)
for (i in 1:length(karst_1_poly)) {
  karst_1_poly[[i]] <- karst_1_poly[[i]] %>%
    rename(ID = 1) %>%
    mutate(ID = seq(1, nrow(karst_1_poly[[i]])))
}

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

### Split Data into 1x1 km Tiles #######################################################################################
split_raster_to_list <- function(list) {
  
  split_list <- list()
  
  for (i in 1:length(list)) { # list of years
    
    print(paste('I = ', i, sep = ''))
    split_list[[i]] <- list()
    
    for (j in 1:nlayers(list[[i]])) { # list of focal function radii
      
      print(paste('J = ', j, sep = ''))
      bnd <- extent(list[[i]][[j]])
      split_list[[i]][[j]] <- list()
      
      for (k in seq(1,(bnd@xmax-bnd@xmin)/1000)) { # splitting up the raster by x values (E-W)
        for (l in seq(1,(bnd@ymax-bnd@ymin)/1000)) { # splitting up the raster by y values (N-S)
          
          print(paste('K = ', k, '; L = ', l), sep = '')
          max_k <- max(seq(1,(bnd@xmax-bnd@xmin)/1000))
          split_list[[i]][[j]][[(k-1)*max_k + l]] <- crop(list[[i]][[j]],
                                                  extent(matrix(c(bnd@xmin + (k - 1)*1000,
                                                                  bnd@xmin + k*1000,
                                                                  bnd@ymin + (l - 1)*1000,
                                                                  bnd@ymin + l*1000),
                                                                2,
                                                                byrow = TRUE)))
          
        }
      }
      
    }
    
  }
  
  return(split_list)
  
}

split_mtopo <- split_raster_to_list(mtopo)

# split_sf_to_list <- function(sf_list, raster_list) {
#   
#   sf_crop_list <- list()
#   
#   for (i in 1:length(sf_list)) {
#     
#     sf_crop_list[[i]] <- list()
#     bnd <- extent(list[[i]][[1]])
#     
#     for (j in seq(1,(bnd@xmax-bnd@xmin)/1000)) { # splitting up the raster by x values (E-W)
#       for (k in seq(1,(bnd@ymax-bnd@ymin)/1000)) { # splitting up the raster by y values (N-S)
#         
#         print(paste('J = ', j, '; K = ', k), sep = '')
#         max_j <- max(seq(1,(bnd@xmax-bnd@xmin)/1000))
#         sf_crop_list[[i]][[(j-1)*max_j + k]] <- st_crop(sf_list[[i]],
#                                                         st_bbox(c(xmin = bnd@xmin + 1000*(j-1),
#                                                                   xmax = bnd@xmin + 1000*j,
#                                                                   ymin = bnd@ymin + 1000*(k-1),
#                                                                   ymax = bnd@ymin + 1000*k)))
#         
#       }
#       
#     }
#     
#   }
#   
#   return(sf_crop_list)
#   
# }
# 
# karst_1_split <- split_sf_to_list(karst_1_poly, mtopo)
########################################################################################################################

### Summary Statistics From Polygons ###################################################################################
for (i in 3:length(split_mtopo)) { # years

  karst_1_sp <- as(karst_1_poly[[i]], 'Spatial')

  for (j in 1:length(split_mtopo[[i]])) { # focal function radii

    for (k in 1:length(split_mtopo[[i]][[j]])) { # tiles

      start <- Sys.time()
      karst_1_depth <- raster::extract(split_mtopo[[i]][[j]][[k]],
                                       karst_1_sp,
                                       layer = 1,
                                       nl = 1,
                                       cellnumbers = TRUE,
                                       df = TRUE) %>%
        as.data.frame()
      end <- Sys.time()
      diff <- difftime(end, start)
      print(diff)
      save_name <- paste('/scratch/hgr7/polygon_summary_split/redo_fill/karst_1_depth_', i, '_', j, '_', k, '.csv', sep = '')
      write.csv(karst_1_depth, file = save_name, row.names = FALSE)

    }

  }

}


# karst_1_depth <- list()
# karst_1_sp <- as(karst_1_poly[['2019']], 'Spatial')
# 
# karst_1_depth[[1]] <- list()
# 
# karst_1_depth[[1]][[1]] <- list()
# for (k in 65:length(split_mtopo[[3]][[3]])) {
# 
#   start <- Sys.time()
#   karst_1_depth[[1]][[1]][[k-64]] <- raster::extract(split_mtopo[[3]][[3]][[k]],
#                                              karst_1_sp,
#                                              layer = 1,
#                                              nl = 1,
#                                              cellnumbers = TRUE,
#                                              df = TRUE) %>%
#     as.data.frame()
#   end <- Sys.time()
#   diff <- difftime(end, start)
#   print(diff)
#   save_name <- paste('/scratch/hgr7/polygon_summary_split/redo/karst_1_depth_3_3_', k, '.csv', sep = '')
#   write.csv(karst_1_depth[[1]][[1]][[k-64]], file = save_name, row.names = FALSE)
# 
# }

# karst_1_depth[[2]] <- list()
# karst_1_sp <- as(karst_1_poly[['2019']], 'Spatial')
# 
# for (j in 1:length(split_mtopo[[3]])) { # focal function radii
# 
#   karst_1_depth[[2]][[j]] <- list()
# 
#   for (k in 1:length(split_mtopo[[3]][[j]])) {
# 
#     start <- Sys.time()
#     karst_1_depth[[2]][[j]][[k]] <- raster::extract(split_mtopo[[3]][[j]][[k]],
#                                                     karst_1_sp,
#                                                     layer = 1,
#                                                     nl = 1,
#                                                     cellnumbers = TRUE,
#                                                     df = TRUE) %>%
#       as.data.frame()
#     end <- Sys.time()
#     diff <- difftime(end, start)
#     print(diff)
#     save_name <- paste('/scratch/hgr7/polygon_summary_split/redo/karst_1_depth_3_', j, '_', k, '.csv', sep = '')
#     write.csv(karst_1_depth[[2]][[j]][[k]], file = save_name, row.names = FALSE)
# 
#   }
# 
# }
########################################################################################################################
