################################################################################
# Code for extracting AGO data for GWW fire modelling
#
# By Bart Huntley

wkdir <- "Z:\\DEC\\GreatWesternWoodland\\Working\\Fire_mapping_2016\\R_analysis"
imdir <- "Z:\\DEC\\GreatWesternWoodland\\Working\\Fire_mapping_2016\\AGO_mosaics"
shpdir <- "Z:\\DEC\\GreatWesternWoodland\\Working\\Fire_mapping_2016\\field_pts"

shp <- "GWW_fire_72to16_test_pts_att"
shp.id <- "UID"
opt <- "b4"

AGO_extractR <- function(wkdir, imdir, shpdir, shp, shp.id, opt){
  ##Libraries required for spatial work
  library(raster)
  library(rgdal)
  library(maptools)
  library(sp)
  
  setwd(imdir)
  #get files
  files <- list.files(pattern = "*.ers")
  filesToDo <- files[c(-1, -21)]#1972 missing scenes & don't need 2014 16bit
  
  #get shapefile
  shpf <- readOGR(dsn = shpdir, layer = shp)
  shpf <- subset(shpf, YEAR1 <= 2014)#don't want years that we don't have up to for imagery
  rnames <- as.character(shpf@data[, shp.id])
  rownames(shpf@data) <- rnames
  namesSHP <- rownames(shpf@data)
  ynames <- paste0("y_", substr(filesToDo, 1, 4), "_", opt)
  
  #empty df for results
  results.a <- as.data.frame(matrix(ncol=length(namesSHP), 
                                    dimnames=list(NULL, namesSHP)))
  
  #extract
  for (a in 1:length(filesToDo)){
    cat(a, " of ", length(filesToDo), '\n')
    data <- filesToDo[a]
    
    bandRaster <- raster(data, band = substr(opt, 2,2))
    shpfT <- spTransform(shpf, crs(bandRaster))
    extVals <- extract(bandRaster, shpfT)
    
    results.a <- rbind(results.a, extVals)
  }
  
  #tidy and write out
  results.b <- as.data.frame(results.a[-1, ])
  results.c <- cbind(ynames, results.b)
  setwd(wkdir)
  write.csv(file = paste0("GWW_fire_modelling_", opt, ".csv"), x = results.c)
}
opt <- "b6"
AGO_extractR(wkdir, imdir, shpdir, shp, shp.id, opt)
