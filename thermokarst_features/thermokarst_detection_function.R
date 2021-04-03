################################################################################
###                     Thermokarst Detection Function                       ###
###                    Code by Heidi G Rodenhizer 4/2021                     ###
################################################################################

### To Do
# name output for easy retrieval - currently not getting 2nd level names to the right place
# allow user input on cut-off value for thermokarst classification
# allow user input on n cores to run in parallel?


library(raster)
dtm <- crop(raster('/home/heidi/ecoss_server/Schuur Lab/2020 New_Shared_Files/DATA/Remote Sensing/NEON/DTM/NEON_DTM_2017.tif'),
            y = extent(matrix(c(390600, 390850, 7085600, 7085850), ncol = 2, byrow = TRUE)))
plot(dtm)

tk.detect <- function(elev, radii) {

  dtm.extent <- extent(dtm)
  resolution <- res(dtm)
  
  ### Calculate median elevation
  # create empty output lists
  weights <- list()
  med.elev <- list()
  elev.crop <- list()
  microtopography <- list()
  thermokarst <- list()
  
  # iterate over all of the desire radii
  for (i in 1:length(radii)) {
    
    # build matrices of weights which correspond to the desired radii
    weights[[i]] <- focalWeight(elev, radii[i], type = 'circle')
    weights[[i]][weights[[i]] > 0] <- 1
    
    # calculate output extent
    new.extent <- extend(dtm.extent,
                         c(rep(-1*(radii[[i]]*resolution[[1]] + resolution[[1]]), 2),
                           rep(-1*(radii[[i]]*resolution[[2]] + resolution[[2]]), 2)))
    
    # calculate median and crop output
    med.elev[[i]] <- crop(focal(elev, weights[[i]], fun = median),
                          new.extent)
    names(med.elev[[i]]) <- paste0('med.elev.', i)
    
    # Crop elevation to output extent
    elev.crop[[i]] <- crop(elev,
                      new.extent)
    
    # Calculate Microtopography
    microtopography[[i]] <- elev.crop[[i]] - med.elev[[i]]
    
    # Reclassify Microtopography as Thermokarst
    reclass.matrix <- matrix(c(-Inf,0,1, 0,Inf,0), ncol = 3, byrow = TRUE)
    
    thermokarst[[i]] <- reclassify(microtopography[[i]], reclass.matrix)
    
  }
  
  ### Create list of output
  output <- list(elev.crop,
                 med.elev,
                 microtopography,
                 thermokarst)
  names(output) <- c('elev.crop', 'med.elev', 'microtopography', 'thermokarst')
  return(output)
  
}

test <- tk.detect(dtm, 5)
plot(test[[2]][[1]])
plot(test[[3]][[1]])
plot(test[[4]][[1]])
