### landfClass updated by HR 4/2021 from GmAMisc version 1.1.0

#' R function for landform classification on the basis od Topographic Position Index
#'
#' The function allows to perform landform classification on the basis of the Topographic Position Index calculated from an input Digital Terrain Model (RasterLayer class).
#'
#' The TPI is the difference between the elevation of a given cell and the average elevation of the surrounding cells in a user defined moving window.
#' For landform classification, the TPI is first standardized and then thresholded; to isolate certain classes, a slope raster (which is internally worked out) is also needed.\cr
#' For details about the implemented classification, see: http://www.jennessent.com/downloads/tpi_documentation_online.pdf.\cr
#'
#' Two methods are available:\cr
#' -the first (devised by Weiss) produces a 6-class landform classification comprising
#' -- valley\cr
#' -- lower slope\cr
#' -- flat slope\cr
#' -- middle slope\cr
#' -- upper clope\cr
#' -- ridge\cr
#' -the second (devised by Jennes) produces a 10-class classification comprising
#' -- canyons, deeply incised streams\cr
#' -- midslope drainages, shallow valleys\cr
#' -- upland drainages, headwaters\cr
#' -- u-shaped valleys\cr
#' -- plains\cr
#' -- open slopes\cr
#' -- upper slopes, mesas\cr
#' -- local ridges, hills in valleys\cr
#' -- midslope ridges, small hills\cr
#' -- mountain tops, high ridges\cr
#' The second classification is based on two TPI that make use of two neighborhoods (moving windows) of different size: a s(mall) n(eighborhood) and a l(arge) n(eighborhood),
#' defined by the parameters sn and ln.\cr
#'
#' Besides rasters representing the different landform classes, the function optionally returns the TPI raster, either un- or standarized.
#'
#' @param x: input DTM (RasterLayer class).
#' @param scale: size (in terms of cells per side) of the neighborhood (moving window) to be used; it must be an odd integer.
#' @param sn: if the 10-class classification is selected, this paramenter sets the s(mall) n(eighborhood) to be used.
#' @param ln: if the 10-class classification is selected, this paramenter sets the l(arge) n(eighborhood) to be used.
#' @param n.classes: "six" or "ten" for a six- or ten-class landform classification.
#' @param add.tpi: set to TRUE will return a TPI raster (FALSE is default).
#' @param stand.tpi: specifies whether the returned TPI raster will be un- or standardized (FALSE is default).
#' @keywords landform
#' @export
#' @examples
#' data(elev) #load the 'elev' raster from the 'raster' package
#' landfClass(elev, scale=5, add.tpi=TRUE, stand.tpi=TRUE) #perform the 6-class landform analysis (which is default), and also produce the standardized TPI; a moving window of dimension 5 (in terms of cells per side) is used
#' landfClass(elev, sn=5, ln=11, n.classes="ten") #perform the 10-class landform analysis, with a s(mall) n(eighborhood) of size 5 and a l(arge) n(eighborhood) of size 11
#'
landfClass <- function (x, scale = 3, sn=3, ln=7, add.tpi=FALSE, stand.tpi = FALSE) {
  #define the shape of the moving window used by spatialEco::tpi
  win = "rectangle"
  
  #calculate the slope from the input DTM, to be used for either the six or ten class slope position
  slp <- raster::terrain(x, opt="slope", unit="degrees", neighbors=8)
  
  #calculate the tpi using spatialEco::tpi function
  tp <- spatialEco::tpi(x, scale=scale, win=win, normalize=TRUE)
  classes <- tp
  
  #define the six classes on the basis of thresholds of tp and slope
  valley <- (tp <= -0.8)
  classes[classes <= -0.8] <- 1
  #valley[na.omit(valley)] <- 1
  
  lower.slp <- (tp > -0.8 & tp <= -0.5) 
  classes[tp > -0.8 & tp <= -0.5] <- 2
  #lower.slp[na.omit(lower.slp)] <- 2
  
  flat.slp <- (tp > -0.5 & tp < 0.3) & (slp <= 5)
  classes[(tp > -0.5 & tp < 0.5) & (slp <= 5)] <- 3
  #flat.slp[na.omit(flat.slp)] <- 3
  
  middle.slp <- (tp > -0.5 & tp < 0.3) & (slp > 5)
  classes[(tp > -0.5 & tp < 0.5) & (slp > 5)] <- 4
  #middle.slp[na.omit(middle.slp)] <- 4
  
  upper.slp <- (tp > 0.3 & tp <= 0.8)
  classes[tp > 0.5 & tp <= 0.8] <- 5
  #upper.slp[na.omit(upper.slp)] <- 5
  
  ridge <- (tp > 0.8)
  classes[tp > 0.8] <- 6
  #ridge[na.omit(ridge)] <- 6
  
  # combine output
  names(classes) <- 'classification'
  classes <- ratify(classes)
  rat <- data.frame(ID = seq(1, 6),
                    landclass = c('Valley',
                                  'Lower Slope',
                                  'Flat Slope',
                                  'Mid Slope',
                                  'Upper Slope',
                                  'Ridge'))
  rat <- rat[which(rat$ID %in% levels(classes)[[1]][,1]),]
  levels(classes) <- rat
  output <- list(tp, classes)
  
  plot(valley, main="Valley", sub="TPI <= -1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  plot(lower.slp, main="Lower Slope", sub="-1 < TPI <= -0.5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  plot(flat.slp, main="Flat Slope", sub="-0.5 < TPI < 0.5 \nslope <= 5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  plot(middle.slp, main="Middle Slope", sub="-0.5 < TPI < 0.5 \nslope > 5", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  plot(upper.slp, main="Upper Slope", sub="0.5 < TPI <= 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  plot(ridge, main="Ridge", sub="TPI > 1", cex.main=0.9, cex.sub=0.7, legend=FALSE)
  
  if (add.tpi == TRUE) {
    if (stand.tpi == FALSE) {
      tp <-  spatialEco::tpi(x, scale=scale, win=win, normalize=FALSE)
    } else {
      tp <- tp
    }
    plot(tp, main=paste0(ifelse(stand.tpi==TRUE, "Standardized", "Unstandardized"), " Topographic Position Index"), cex.main=0.8)
  }
  return(output)
}