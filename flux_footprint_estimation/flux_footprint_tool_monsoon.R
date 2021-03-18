#########################################################################################
###                          Run FFP on EML Tower 2017-2019                        ###
###                                Code by HGR 2/2021                                 ###
#########################################################################################

### Libraries ###########################################################################
library(sf)
library(raster)
library(data.table)
library(lubridate)
library(doParallel)
library(tidyverse)
#########################################################################################

### Load data ###########################################################################
final.17 <- read.csv('/scratch/hgr7/flux_tower_footprint/data_input/ffp_ready_2017.csv',
                     na.strings = '-999')
final.17 <- data.table(final.17)
#########################################################################################

### Run FFP #############################################################################
### set up data needed for ffp formatting
# point for ec tower location UTM zone 6N
ec <- st_sfc(st_point(c(389389.25, 7085586.3), dim = 'XY'), crs = 32606)
ec_sf <- st_sf(geometry = ec, crs = 32606)
rm(ec)

# raster template
karst_1 <- brick(stack("/scratch/hgr7/output/karst_combined_1_raster_final_1.tif",
                       "/scratch/hgr7/output/karst_combined_1_raster_final_2.tif",
                       "/scratch/hgr7/output/karst_combined_1_raster_final_3.tif"))
crs(karst_1) <- CRS('+init=epsg:32606')
mean.karst <- calc(karst_1, mean, na.rm = TRUE)

filenames <- list.files('/scratch/hgr7/int_output',
                        full.names = TRUE,
                        pattern = '^mtopo15.+9km')

mtopo15 <- brick(stack(filenames[which(str_detect(filenames, pattern = 'mtopo15.+_1\\.tif$'))],
                     filenames[which(str_detect(filenames, pattern = 'mtopo15.+_2\\.tif$'))],
                     filenames[which(str_detect(filenames, pattern = 'mtopo15.+_3\\.tif$'))]))
mean.mtopo <- calc(mtopo15, mean, na.rm = TRUE)
karst.mtopo.brick <- brick(mean.karst, mean.mtopo)

rm(filenames, karst_1, mean.karst, mtopo15, mean.mtopo)


# # run the function on 2017
# Define how many cores you want to use
UseCores <- 28

# split up data
data.rows <- nrow(final.17)
chunk.length <- ceiling(data.rows/UseCores)
final.17.list <- list()
for (i in 1:UseCores) {
  start.row <- (i - 1)*chunk.length + 1
  if (i < UseCores) {
    end.row <- i*chunk.length
  } else {
    end.row <- nrow(final.17)
  }
  final.17.list[[i]] <- final.17[start.row:end.row]
}

cl <- makeCluster(UseCores, outfile = '/scratch/hgr7/flux_tower_footprint/ffp_raw_output/log.txt')
registerDoParallel(cl)

ffp.df.2017 <- foreach(i=1:UseCores, .combine = rbind) %dopar% {
  # load libraries
  library(sf)
  library(raster)
  library(tidyverse)
  
   # load functions
  calc_footprint_FFP <-
    function(lat, zm, z0, umean=NaN, ol, sigmav, ustar, wind_dir=NULL,
             r=NULL, nx = NULL, rslayer = NULL, crop = NULL) {
      
      # Derive a flux footprint estimate based on the simple parameterisation FFP
      # 
      # See Kljun, N., P. Calanca, M.W. Rotach, H.P. Schmid, 2015: 
      # The simple two-dimensional parameterisation for Flux Footprint Predictions FFP.
      # Geosci. Model Dev. 8, 3695-3713, doi:10.5194/gmd-8-3695-2015, for details.
      # contact: n.kljun@swansea.ac.uk
      #
      #
      # FFP Input
      #    zm        = Measurement height above displacement height (i.e. z-d) [m]
      #    z0        = Roughness length [m] - enter [NaN] if not known 
      #    umean     = Mean wind speed at measurement height zm [ms-1] - enter [NaN] if not known 
      #                Either z0 or umean is required. If both are given,
      #                umean is selected to calculate the footprint
      #    h         = Boundary layer height [m]
      #    ol        = Obukhov length [m]
      #    sigmav    = standard deviation of lateral velocity fluctuations [ms-1]
      #    ustar     = friction velocity [ms-1]
      #
      #    Optional inputs:
      #    wind_dir  = wind direction in degrees (of 360) for rotation of the footprint     
      #    r         = Percentage of source area for which to provide contours, must be between 10% and 90%.
      #                Can be either a single value (e.g., "80") or an array of percentage values (e.g., "seq(10, 80, 10)") 
      #                Expressed either in percentages ("80") or in fractions of 1 ("0.8")
      #                Default is [10:10:80]. Set to "NaN" for no output of percentages
      #    nx        = Integer scalar defining the number of grid elements of the scaled footprint.
      #                Large nx results in higher spatial resolution and higher computing time.
      #                Default is 1000, nx must be >=600.
      #    rslayer   = Calculate footprint even if zm within roughness sublayer: set rslayer = 1
      #                Note that this only gives a rough estimate of the footprint as the model is not valid within 
      #                the roughness sublayer. Default is 0 (i.e. no footprint for within RS).
      #                z0 is needed for estimation of the RS.
      #    crop      = Crop output area to size of the 80# footprint or the largest r given if crop=1
      #
      # FFP output
      #    FFP          = structure array with footprint data
      #    FFP$x_ci_max = x location of footprint peak (distance from measurement) [m]
      #    FFP$x_ci     = x values of crosswind integrated footprint [m]
      #    FFP$f_ci     = footprint function values of crosswind integrated footprint [m-1] 
      #    FFP$x_2d     = x-grid of 2-dimensional footprint [m], rotated if wind_dir is provided
      #    FFP$y_2d     = y-grid of 2-dimensional footprint [m], rotated if wind_dir is provided
      #    FFP$f_2d     = f-grid of 2-dimensional footprint [m-2]
      #    FFP$r        = percentage of footprint as in input, if provided
      #    FFP$fr       = footprint value at r, if r is provided
      #    FFP$xr       = list of x-arrays for contour line of r, if r is provided
      #    FFP$yr       = list of y-array for contour line of r, if r is provided
      #                   For array of percentage values, structure entries can be
      #                   accessed as FFP$r[1], FFP$xr[[1]], or FFP[[1]][1], etc.
      #    FFP$flag_err = 1 in case of error, 0 otherwise
      #
      # Example
      #    ffp <- calc_footprint_FFP(zm=20, z0=0.01, umean=NA, h=2000, ol=-100,
      #                   sigmav=0.9, ustar=0.5, wind_dir=30, nx=1100, crop=1)
      #
      # created in matlab: 15 April 2015 natascha kljun
      # ported to R: 25 September 2015 by sietse los
      # version: 1.4
      # last change: 08/12/2017 natascha kljun
      #
      # Copyright (C) 2015,2016,2017,2018,2019,2020 Natascha Kljun
      
      #--------------------------------------------------------------------
      # Calculate h
      #--------------------------------------------------------------------
      # use equation from appendix b of Kljun et al. 2015 for neutral and stable conditions
      # use 1500 for convective conditions
      if (is.null(ol) | is.na(ol)) {
        h <- NA
      } else if (ol < 0) {
        h <- 1500
      } else {
        omega <- 0.73*10^-4 # angular velocity of earth
        h <- ol/3.88*(-1 + (1 + 2.288*ustar/(2*omega*sin(lat)))^(1/2));
      }
      
      #--------------------------------------------------------------------
      # Check input variables
      #--------------------------------------------------------------------
      flag_err <- 0
      ind_return <- 0
      output_list <- checkinput(zm, h, z0, umean, ol, sigmav, ustar, 
                                wind_dir, nx, r, rslayer, crop, ind_return, flag_err)
      for (v in 1:length(output_list)) assign(names(output_list)[v], 
                                              output_list[[v]])
      
      #--------------------------------------------------------------------
      # Create output array
      #--------------------------------------------------------------------
      FFP <- NULL
      FFP$x_ci_max <- NaN;
      FFP$x_ci     <- NaN;
      FFP$f_ci     <- NaN;
      FFP$x_2d     <- NaN;
      FFP$y_2d     <- NaN;
      FFP$f_2d     <- NaN;
      FFP$r        <- NULL;
      FFP$fr       <- NULL;
      FFP$xr       <- NULL;
      FFP$yr       <- NULL;
      FFP$flag_err <- flag_err
      if (ind_return == 1) {
        FFP$flag_err <- 1
      }
      
      if (flag_err == 0) {
        #--------------------------------------------------------------------
        # Initialize model variables
        #--------------------------------------------------------------------
        a <- 1.4524;
        b <- -1.9914;
        c <- 1.4622;
        d <- 0.1359;
        
        ac <- 2.17; 
        bc <- 1.66;
        cc <- 20.0;
        
        xstar_end <- 30;
        
        #limit for neutral scaling
        ol_n <- 5000;
        
        #von Karman
        k <- 0.4;
        
        #--------------------------------------------------------------------
        # Create scaled X* for crosswind integrated footprint
        #--------------------------------------------------------------------
        xstar_ci_param <- seq(d,xstar_end,length=nx+2);
        xstar_ci_param <- matrix(xstar_ci_param[-1], nrow=1);
        
        #--------------------------------------------------------------------
        # Calculate crosswind integrated scaled F* 
        #--------------------------------------------------------------------
        fstar_ci_param <- a*(xstar_ci_param-d)^b * exp(-c/(xstar_ci_param-d));
        ind_notnan     <- !is.na(fstar_ci_param);
        fstar_ci_param <- fstar_ci_param[ind_notnan];
        xstar_ci_param <- xstar_ci_param[ind_notnan];
        
        #--------------------------------------------------------------------
        # Calculate scaled sig_y*
        #--------------------------------------------------------------------
        sigystar_param <- ac*(bc*(xstar_ci_param)^2 / (1+cc*(xstar_ci_param)))^0.5;
        
        #--------------------------------------------------------------------
        # Calculate real scale x and f_ci
        #--------------------------------------------------------------------
        if (!is.na(z0) & (z0 > 0)) {
          if ( (ol <=0) || (ol >=ol_n)) {
            xx  <- (1 - 19.0*zm/ol)^0.25;
            psi_f <- log((1+xx^2)/2) + 2*log((1+xx)/2) - 2*atan(xx) + pi/2;
          }
          else if (( ol > 0) && (ol < ol_n)){
            psi_f <- -5.3*zm/ol;
          }
          
          x <- matrix(xstar_ci_param*zm / (1-(zm/h)) * (log(zm/z0)-psi_f), nrow=1);
          if ((log(zm/z0)-psi_f)>0){
            x_ci <- x;
            f_ci <- fstar_ci_param/zm * (1-(zm/h)) / (log(zm/z0)-psi_f);
          }
          else{
            FFP$flag_err <- 1;
          }
        }
        else {
          x <- matrix(xstar_ci_param*zm / (1-(zm/h)) * (umean/ustar*k), nrow=1);
          
          if ((umean/ustar)>0) {
            x_ci <- x;
            f_ci <- fstar_ci_param/zm * (1-(zm/h)) / (umean/ustar*k);
          }
          else{
            FFP$flag_err <- 1;
          }
        }
        
        if (FFP$flag_err == 0){
          #--------------------------------------------------------------------
          # Calculate maximum location of influence (peak location)
          #--------------------------------------------------------------------
          xstarmax <- -c/b+d;
          if (!is.na(umean)){
            x_ci_max <- xstarmax*zm / (1-(zm/h)) * umean/ustar*k;
          }
          else{
            x_ci_max <- xstarmax*zm / (1-(zm/h)) * (log(zm/z0)-psi_f);
          }
          
          #--------------------------------------------------------------------
          # Calculate real scale sigy
          #--------------------------------------------------------------------
          if (abs(ol) >ol_n){
            ol <- -1000000;
          }
          if (ol <= 0 ) { #convective
            scale_const = 1E-5*abs(zm/ol)^(-1)+0.8;
          }
          else { #  if (ol > 0) {  #stable
            scale_const = 1E-5*abs(zm/ol)^(-1)+0.55;
          }
          if (scale_const>1){
            scale_const  <- 1.0;
          }
          sigy         <- sigystar_param/scale_const *zm *sigmav/ustar;
          sigy[sigy<0] <- NaN;
          
          #--------------------------------------------------------------------
          # Calculate real scale f(x,y)
          #--------------------------------------------------------------------
          dx    <- x_ci[3]-x_ci[2];
          y_pos <- matrix(seq(0,(length(x_ci)/2)*dx*1.5, dx), nrow=1);
          f_pos <- matrix(NaN,nrow=length(f_ci),ncol=length(y_pos));
          for (i in 1:length(f_ci)) {
            f_pos[i,] = f_ci[i] * 1/(sqrt(2*pi)*sigy[i]) * exp(-y_pos^2/(2*sigy[i]^2));
          }
          
          #--------------------------------------------------------------------
          # Complete footprint for negative y (symmetrical)
          #--------------------------------------------------------------------
          y_pos <- matrix(y_pos, nrow=1)
          nr_y  <- nrow(y_pos)
          nc_y  <- ncol(y_pos)
          y     <- matrix(NaN,nrow=nr_y, ncol=(nc_y+nc_y-1))
          f     <- matrix(NaN, nrow=nrow(f_pos), ncol=(nc_y+nc_y-1))
          y[,1:(nc_y-1)]         <- -y_pos[,(nc_y):2]
          y[,nc_y:(nc_y+nc_y-1)] <- y_pos
          f[,1:(nc_y-1)]         <- f_pos[,(nc_y):2]
          f[,nc_y:(nc_y+nc_y-1)] <- f_pos
          
          #--------------------------------------------------------------------
          # Matrices for output
          #--------------------------------------------------------------------
          x_2d <- matrix(rep(x, length(y)), nrow = length(y), ncol = length(x), byrow = T)
          y_2d <- matrix(rep(y, length(x)), nrow = length(y), ncol = length(x))
          f_2d <- f
          
          #--------------------------------------------------------------------
          # Derive footprint ellipsoid incorporating R% of the flux
          # starting at peak value, if requested
          #--------------------------------------------------------------------
          ffp_tmp <- NULL
          
          if (!is.null(r[1])) {
            rs <- r
          }
          else {
            if (crop == 1) {
              rs <- 0.8
            }
            else {
              rs <- NA
            }
          }
          dy <- dx
          if (!is.na(rs[1])){
            # Calculate integral of f_2d starting at peak value until R% are reached
            FFP$r   <- rs * NA
            FFP$fr  <- rs * NA
            f_array <- matrix(f_2d,nrow=1)
            f_sort  <- sort(f_array, decreasing=T)
            f_sort  <- f_sort[!is.na(f_sort)]
            f_cum   <- cumsum(f_sort)*dx*dy
            for (i in 1:length(rs)){
              f_diff    <- abs(f_cum - rs[i])
              ind_r     <- which.min(f_diff)
              fr        <- f_sort[ind_r]    
              contour_r <- contourLines(x,y,f_2d,levels=c(fr))
              
              # Decrease number of digits and sort/unique
              c_x   <- round(contour_r[[1]]$x*10)/10
              c_y   <- round(contour_r[[1]]$y*10)/10
              new_c <- unique(cbind(c_x, c_y))
              new_c <- rbind(new_c, new_c[1,]) 
              
              if (!is.na(r[1])) {
                # Fill output structure
                FFP$r[i]  <- rs[i]
                FFP$fr[i] <- fr;
                ffp_tmp$xr[[i]] <- c(new_c[,1])
                ffp_tmp$yr[[i]] <- c(new_c[,2])
              }
            } # end for i
            
          }
          
          #--------------------------------------------------------------------
          # Crop domain
          #--------------------------------------------------------------------
          if (!is.null(crop)) {
            if (crop == 1) {
              dminx = floor(min(ffp_tmp$xr[[i]], na.rm = T))
              dmaxx = ceiling(max(ffp_tmp$xr[[i]], na.rm = T))
              dminy = floor(min(ffp_tmp$yr[[i]], na.rm = T))
              dmaxy = ceiling(max(ffp_tmp$yr[[i]], na.rm = T))
              len_x <- length(x)
              len_y <- length(y)
              
              u_x <- x
              indx <- 1:length(u_x)
              indx <- indx[(u_x >= dminx) & (u_x <= dmaxx)]
              # extend by one row/column
              indx <- c(min(indx) - 1, indx, max(indx) + 1)
              indx <- indx[(indx > 0) & (indx <= nrow(x_2d))]
              x <- x[indx]
              len_x <- length(x)
              f_2d <- f_2d[indx,]
              
              u_y <- y
              indy <- 1:length(u_y)
              indy <- indy[(u_y >= dminy) & (u_y <= dmaxy)]
              # extend by one row/column
              indy <- c(min(indy) - 1, indy, max(indy) + 1)
              indy <- indy[(indy > 0) & (indy <= nrow(y_2d))]
              y <- y[indy]
              len_y <- length(y)
              f_2d <- f_2d[,indy]
              
              x_2d <- matrix(rep(x, length(y)), nrow = length(y), ncol = length(x), byrow = T)
              y_2d <- matrix(rep(y, length(x)), nrow = length(y), ncol = length(x))
              
            }
          }
          
          #--------------------------------------------------------------------
          # Rotate footprint if requested
          #--------------------------------------------------------------------
          if (!is.null(wind_dir)){
            wind_dir_rad <- wind_dir * pi /180;
            dist         <- (x_2d^2 + y_2d^2)^0.5
            angle        <- atan2(y_2d, x_2d)
            x_2d_rot     <- dist * sin(wind_dir_rad-angle)
            y_2d_rot     <- dist * cos(wind_dir_rad-angle)
            
            if (!is.na(r[1])){
              for (i in 1:length(r)){
                dist      <- angle <- x_tmp_rot <- y_tmp_rot <- NULL
                dist      <- (ffp_tmp$xr[[i]]^2 + ffp_tmp$yr[[i]]^2)^0.5
                angle     <- atan2(ffp_tmp$yr[[i]], ffp_tmp$xr[[i]])
                x_tmp_rot <- dist * sin(wind_dir_rad-angle)
                y_tmp_rot <- dist * cos(wind_dir_rad-angle)
                # Fill output structure
                ffp_tmp$xr[[i]] <- x_tmp_rot
                ffp_tmp$yr[[i]] <- y_tmp_rot
              }
            }
          }
          
          #--------------------------------------------------------------------
          # Fill output structure
          #--------------------------------------------------------------------
          FFP$x_ci_max <- x_ci_max
          FFP$x_ci <- x_ci
          FFP$f_ci <- f_ci
          if (is.null(wind_dir)){
            FFP$x_2d <- x_2d
            FFP$y_2d <- y_2d
          }
          else{
            FFP$x_2d <- x_2d_rot
            FFP$y_2d <- y_2d_rot
          }
          FFP$f_2d <- t(f_2d)
          FFP$xr   <- ffp_tmp$xr
          FFP$yr   <- ffp_tmp$yr
          
          
        } 
        
      }
      
      FFP
    }
  
  
  #--------------------------------------------------------------------
  # Function checkinput
  #--------------------------------------------------------------------
  checkinput <-
    function (zm, h, z0, umean, ol, sigmav, ustar, wind_dir, nx = NULL,  
              r = NULL, rslayer = NULL, crop = NULL, ind_return=0, flag_err=0) {
      flag_err <- 0
      ind_return <- 0
      
      if (any(is.null(c(zm,h,ol,sigmav,ustar)))) {
        print('wrong number of input arguments')
        ind_return <- 1;
      } else if (any(is.na(c(zm,h,ol,sigmav,ustar)))) {
        print('at least one of the input arguments is NA')
        ind_return <- 1;
      } else {
        
        if (min(zm) <= 0) {
          print("zm must be larger than 0")
          ind_return <- 1
        }
        else if (min(h) < 10) {
          print("h must be larger than 10 m")
          ind_return <- 1
        }
        else if (min(sigmav) < 0) {
          print("sig.v must be larger than 0")
          ind_return <- 1
        }
        else if (min(ustar) < 0) {
          print("ustar must be larger than 0")
          ind_return <- 1
        }
        else if (zm>h) {
          print('zm needs to be smaller than h')
          ind_return <- 1;
        }
        else if (zm/ol<= -15.5){
          print('zm/L needs to be equal or larger than -15.5')
          ind_return <- 1;
        }
        if (!is.null(wind_dir)) {
          if (max(wind_dir) > 360) {
            print("(all) wind direction(s) must be <= 360")
            ind_return <- 1
          }
          else if (min(wind_dir) < 0) {
            print("(all) wind direction(s) must be >= 0")
            ind_return <- 1
          }    
        }
      }
      if (is.null(r[1])) {
        r <- seq(10, 80, 10)
      }
      if (!is.na(r[1])) {
        if (max(r) > 1) {
          r <- r/100
        }
        if (max(r) > 0.9) {
          print("R must be ,<= 0.9 or <=90#, larger values were removed")
          r <- r[r <= 0.9]
        }
        r <- sort(r)
      }
      if (is.null(nx)) {
        nx <- 1000
      }
      else if (nx < 600) {
        print("nx must be >= 600")
        ind_return <- 1
      }
      if (is.null(rslayer)) {
        rslayer <- 0
      }
      if (is.null(crop)) {
        crop <- 0
      }
      
      if (!is.na(z0)) {
        if (z0 < 0) {
          print("z0 must be larger than 0")
          ind_return <- 1
        }
        else if ((zm < z0 * 12.5) & (rslayer != 1)) {
          #changed to lowest limit of roughness sublayer definition
          print("zm must be above roughness sublayer")
          ind_return <- 1
        } 
      }
      else if (!is.na(umean)) {
        if (umean < 0) {
          print("umean must be larger than 0")
          ind_return <- 1
        }
      }
      else if (is.na(z0) & is.na(umean)) {
        print("enter either z0 or umean")
        ind_return <- 1
      }
      
      if (ind_return) {
        flag_err <- 1
      }
      list(ind_return = ind_return, flag_err = flag_err, zm = zm, h = h, z0 = z0, 
           wind_dir = wind_dir, nx = nx, r = r, crop = crop)
    }
  
  
  calc.ffp.loop <- function(df, tower.loc, raster.brick, contour.range) {
    
    print(paste('Function prep starting at', Sys.time()))
    
    # extract tower location information as numeric 
    tower.x.utm <- st_coordinates(tower.loc)[,1]
    tower.y.utm <- st_coordinates(tower.loc)[,2]
    latitude.wgs84 <- st_coordinates(st_transform(tower.loc, crs = 4326))[,1]
    
    # create raster template onto which output should be projected
    crop.extent <- extent(tower.x.utm - 500,
                          tower.x.utm + 500,
                          tower.y.utm - 500,
                          tower.y.utm + 500)
    raster.brick.crop <- crop(raster.brick, crop.extent)
    
    # create vector of ffp output names
    matrices <- c('x_2d', 'y_2d', 'f_2d')
    
    # create output objects
    karst.pc <- vector()
    sd.mtopo <- vector()
    
    # run ffp model on each row of data in the input (each row is a half hour period)
    for (i in 1:nrow(df)) { 
      
      print(paste('Iteration', i, 'footprint model starting at', Sys.time()))
      
      # run ffp model
      ffp <- calc_footprint_FFP(lat = latitude.wgs84,
                                zm = as.numeric(df[i, "zm"]),
                                z0 = as.numeric(df[i, "z0"]),
                                umean = as.numeric(df[i, "u_mean"]),
                                ol = as.numeric(df[i, "L"]),
                                sigmav = as.numeric(df[i, "sigma_v"]),
                                ustar = as.numeric(df[i, "u_star"]),
                                wind_dir = as.numeric(df[i, "wind_dir"]),
                                r = contour.range)
      
      if (ffp$flag_err == 0) {
        
        print(paste('Iteration', i, 'footprint model done. Reformating starting at', Sys.time()))
        
        # format as data frame (this allows easy alignment of points to ec tower)
        ffp.sf <- map_dfc(matrices,
                          ~ ffp[[.x]] %>% # select x, y, or f matrix
                            as.data.frame() %>%
                            pivot_longer(cols = 1:ncol(.),
                                         names_to = 'remove',
                                         values_to = .x) %>% # make it into a 'tidy' data frame (long format with single column for x, y, or f)
                            select(-matches('remove'))) %>% # we don't need the names column which just contains original column information
          mutate(x_geo = x_2d + tower.x.utm, # add ec tower latitude location to x distance values
                 y_geo = y_2d + tower.y.utm, # add ec tower longitude location to y distance values
                 f_2d_scaled = f_2d/sum(f_2d, na.rm = TRUE)) %>% # scale the flux measurement for each cell to a percentage of total flux
          st_as_sf(coords = c('x_geo', 'y_geo'), crs = 32606, remove = FALSE)
        
        # reformat contours as sf
        ffp.sa.polygon <- list()
        for (j in 1:length(ffp$xr)) {
          # extract vectors with contour location information
          xr <- ffp$xr[[j]] + tower.x.utm
          yr <- ffp$yr[[j]] + tower.y.utm
          # convert contour information to an sf polygon object
          ffp.sa.polygon[[j]] <- st_polygon(list(matrix(c(xr, yr), ncol = 2, byrow = FALSE)))
        }
        ffp.sa.sfc <- st_as_sfc(ffp.sa.polygon, crs = 32606)
        ffp.sa.sf <- st_sf(geometry = ffp.sa.sfc) %>%
          mutate(interval = contour.range)
        
        # use rasterize to convert points to a raster aligned with thermokarst model
        ffp.raster <- rasterize(as(ffp.sf, 'Spatial'),
                                raster.brick.crop,
                                field = 'f_2d_scaled',
                                fun = 'sum',
                                na.rm = TRUE)
        
        # mask to maximum requested contour polygon
        ffp.raster.mask <- mask(ffp.raster, ffp.sa.sf)
        raster.brick.mask <- mask(raster.brick.crop, ffp.sa.sf)
        # plot(ffp.raster.mask)
        
        # calculate thermokarst percent cover
        karst.pc[i] <- cellStats(raster.brick.mask[[1]]*ffp.raster.mask, sum, na.rm = TRUE)/cellStats(ffp.raster.mask, sum, na.rm = TRUE)
        
        # calculate mean microtopography
        sd.mtopo[i] <- sqrt(cellStats(ffp.raster.mask*(raster.brick.mask[[2]] - cellStats(raster.brick.mask[[2]], mean, na.rm = TRUE))^2, sum, na.rm = TRUE)/((length(ffp.raster.mask[!is.na(ffp.raster.mask)]) - 1)/length(ffp.raster.mask[!is.na(ffp.raster.mask)])*cellStats(ffp.raster.mask, sum, na.rm = TRUE)))
        
      } else { # if the model didn't run for the current time period
        
        print(paste('Iteration', i, 'footprint model failed at', Sys.time()))
        
        # fill in karst.pc and sd.mtopo with NA
        karst.pc[i] <- NA
        sd.mtopo[i] <- NA
        
      }
      
    }
    
    print(paste('Loop completed at', Sys.time()))
    
    output <- df %>%
      mutate(karst.pc = karst.pc,
             sd.mtopo = sd.mtopo)
    
    print(paste('Output being returned at', Sys.time()))
    
    
    return(output)
    
  }
  
  # calculate ffp
  calc.ffp.loop(final.17.list[[i]], ec_sf, karst.mtopo.brick, seq(10, 90, 10))
  
}

stopCluster(cl)

write.csv(ffp.df.2017,
          '/scratch/hgr7/flux_tower_footprint/ffp_2017.csv',
          row.names = FALSE)
#########################################################################################