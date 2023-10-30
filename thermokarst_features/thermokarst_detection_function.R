################################################################################
###                     Thermokarst Detection Function                       ###
###                    Code by Heidi G Rodenhizer 4/2021                     ###
################################################################################
tk.detect <- function(elev, radii = 15, cutoff = 0, n.cores = 1) {
  
  if (detectCores() < n.cores) {
    
    print('You have requested more cores than are available.')
    
  } else {
    
    # raster information
    elev.extent <- extent(elev)
    resolution <- res(elev)
    n.layers <- nlayers(elev)
    
    # calculate output extent based on extent of non-NA values after
    # calculating median elevation with the largest radius
    new.extent <- extend(elev.extent,
                         c(rep(-1*(max(radii)*resolution[[1]] + resolution[[1]]), 2),
                           rep(-1*(max(radii)*resolution[[2]] + resolution[[2]]), 2)))
    
    # Crop elevation to output extent
    elev.crop <- crop(elev,
                      new.extent)
    
    # build matrices of weights which correspond to the desired radii
    weights <- list()
    for (i in 1:length(radii)) {
      
      weights[[i]] <- focalWeight(elev, radii[i], type = 'circle')
      weights[[i]][weights[[i]] > 0] <- 1
      
    }
    
    # create reclassification matrix using the provided cut-off value
    reclass.matrix <- matrix(c(-Inf,cutoff,1, cutoff,Inf,0), ncol = 3, byrow = TRUE)
    
    
    ### Calculate median elevation, microtopography, and thermokarst
    # different methods used depending on number of cores requested
    # and number of layers in input elevation data
    if (n.cores == 1) {
      
      # create empty output lists for each radius
      med.elev <- list()
      microtopography <- list()
      thermokarst <- list()
      
      
      # iterate over all of the desire radii
      for (i in 1:length(radii)) {
        
        if (n.layers == 1) {
          
          # calculate median and crop output
          med.elev[[i]] <- crop(focal(elev, weights[[i]], fun = median),
                                new.extent)
          names(med.elev)[[i]] <- paste0('med.elev.', radii[i])
          names(med.elev[[i]]) <- paste0('med.elev.', radii[i], '.', names(elev))
          
          # calculate microtopography
          microtopography[[i]] <- elev.crop - med.elev[[i]]
          names(microtopography)[[i]] <- paste0('microtopography.', radii[i])
          names(microtopography[[i]]) <- paste0('microtopography.', radii[i], '.', names(elev))
          
          # reclassify microtopography as thermokarst
          thermokarst[[i]] <- reclassify(microtopography[[i]], reclass.matrix)
          names(thermokarst)[[i]] <- paste0('thermokarst.', radii[i])
          names(thermokarst[[i]]) <- paste0('thermokarst.', radii[i], '.', names(elev))
          
          
        } else if (n.layers > 1) {
          
          # create empty output lists for each layer nested within radius
          med.elev[[i]] <- list()
          microtopography[[i]] <- list()
          thermokarst[[i]] <- list()
          
          for (j in 1:n.layers) {
            
            # calculate median and crop output
            med.elev[[i]][[j]] <- crop(focal(elev[[j]], weights[[i]], fun = median),
                                       new.extent)
            names(med.elev[[i]])[[j]] <- paste0('med.elev.', radii[i], '.', names(elev[[j]]))
            
            # calculate microtopography
            microtopography[[i]][[j]] <- elev.crop[[j]] - med.elev[[i]][[j]]
            names(microtopography[[i]])[[j]] <- paste0('microtopography.', radii[i], '.', names(elev[[i]]))
            
            # reclassify microtopography as thermokarst
            thermokarst[[i]][[j]] <- reclassify(microtopography[[i]][[j]], reclass.matrix)
            names(thermokarst[[i]])[[j]] <- paste0('thermokarst.', radii[i], '.', names(elev[[i]]))
            
          }
          
          # convert lists of layers to rasterBrick and name list elements
          med.elev[[i]] <- brick(med.elev[[i]])
          names(med.elev)[[i]] <- paste0('med.elev.', radii[i])
          microtopography[[i]] <- brick(microtopography[[i]])
          names(microtopography)[[i]] <- paste0('microtopography.', radii[i])
          thermokarst[[i]] <- brick(thermokarst[[i]])
          names(thermokarst)[[i]] <- paste0('thermokarst.', radii[i])
          
        }
        
      }
      
      
    } else if (n.cores > 1) {
      
      # Register CoreCluster
      cl <- makeCluster(n.cores)
      registerDoParallel(cl)
      
      if (n.layers == 1) {
        
        # calculate median and crop output
        med.elev <- foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          crop(focal(elev, weights[[i]], fun = median),
               new.extent)
        }
        
        # calculate microtopography
        microtopography <- foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          elev.crop - med.elev[[i]]
        }
        
        # reclassify microtopography as thermokarst
        thermokarst <- foreach(i=1:length(radii), .packages = 'raster') %dopar% {
          reclassify(microtopography[[i]], reclass.matrix)
        }
        
      } else if (n.layers > 1) {
        
        # calculate median and crop output
        med.elev <- foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            crop(focal(elev[[j]], weights[[i]], fun = median),
                 new.extent)
          }
        
        # calculate microtopography
        microtopography <- foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            elev.crop[[j]] - med.elev[[i]][[j]]
          }
        
        # reclassify microtopography as thermokarst
        thermokarst <- foreach(i=1:length(radii), .packages = 'raster') %:%
          foreach(j=1:n.layers, .packages = 'raster', .combine = 'brick') %dopar% {
            reclassify(microtopography[[i]][[j]], reclass.matrix)
          }
        
      }
      
      # end cluster
      stopCluster(cl)
      
      # name output
      for (i in 1:length(radii)) {
        names(med.elev[[i]]) <- paste0('med.elev.', radii[i], '.', names(elev))
        names(microtopography[[i]]) <- paste0('microtopography.', radii[i], '.', names(elev))
        names(thermokarst[[i]]) <- paste0('thermokarst.', radii[i], '.', names(elev))
      }
      
      names(med.elev) <- paste0('med.elev.', radii)
      names(microtopography) <- paste0('microtopography.', radii)
      names(thermokarst) <- paste0('thermokarst.', radii)
      
    }
    
    ### Create list of output
    output <- list(cutoff,
                   radii,
                   elev.crop,
                   med.elev,
                   microtopography,
                   thermokarst)
    names(output) <- c('cutoff.value',
                       'radii',
                       'elev.crop',
                       'med.elev',
                       'microtopography',
                       'thermokarst')
    return(output)
    
  }
  
}
