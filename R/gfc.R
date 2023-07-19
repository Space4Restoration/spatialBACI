# Author: Jasper Van doninck
# Date: July 2023

#To do: Add functions for processing/summarizing GFC layers?

gfc_tiles <- function(x){
  library(terra)
  #To do: check possible issues when lower/upper bound is on edge of tile
  
  #Convert to SpatExtent
  x<- terra::ext(x)
  
  roundDown <- function(y) 10*floor(y/10)
  roundUp <- function(y) 10*ceiling(y/10)
  EW2txt <- function(z){
    if (z<0) EWtxt <- paste0(abs(z),"W") else EWtxt <- paste0(z,"E")
    if (abs(z)>=100) return(EWtxt) else if (abs(z)>=10) return(paste0(0,EWtxt)) else return(paste0(0,0,EWtxt))
  }
  NS2txt <- function(z){
    if (z<0) NStxt <- paste0(abs(z),"S") else NStxt <- paste0(z,"N")
    if (z>=10) return(NStxt) else return(paste0(0,NStxt))
  }
  NS_range <- sapply(seq(roundUp(x$ymin), roundUp(x$ymax), 10),
                     NS2txt)
  EW_range <- sapply(seq(roundDown(x$xmin), roundDown(x$xmax), 10),
                     EW2txt)
  tiles <- as.character(outer(NS_range, EW_range, paste, sep="_"))
  return(tiles)
}

gfc2rast <- function(extent, layers=c("treecover2000", "gain", "lossyear", "datamask"), version="2022-v1.10"){
  
  library(terra)
  toP <- terraOptions(print=FALSE)$progress
  terraOptions(progress=0)
  
  tiles <- gfc_tiles(extent)
  
  cropLayer <- function(layer){
    filesList <- paste0("/vsicurl/",
                        "https://storage.googleapis.com/earthenginepartners-hansen/GFC-",version,"/Hansen_GFC-",version,"_",
                        layer, "_", 
                        tiles, ".tif")
    
    layer_vrt <- vrt(filesList)
    layer_rast <- crop(layer_vrt, extent)
    return(layer_rast)
  }
  
  gfc_out <- rast(lapply(layers, cropLayer))
  names(gfc_out) <- layers
  
  terraOptions(progress=toP)
  return(gfc_out)
}

setGeneric("gfc", function(x, ...) standardGeneric("gfc"))

setMethod("gfc", signature="SpatRaster",
          function(x, layers=c("treecover2000", "gain", "lossyear", "datamask"), version="2022-v1.10", method="near", ...){
            library(terra)
            extent <- terra::project(ext(x), crs(x), crs("epsg:4326"))
            gfc_layers <- gfc2rast(extent, layers=layers, version=version)
            gfc_layers <- terra::project(gfc_layers, x, method=method, ...)
            return(gfc_layers)
          }
)

setMethod("gfc", signature="SpatVector",
          function(x, layers=c("treecover2000", "gain", "lossyear", "datamask"), version="2022-v1.10"){
            library(terra)
            extent <- terra::project(ext(x), crs(x), crs("epsg:4326"))
            gfc_layers <- gfc2rast(extent, layers=layers, version=version)
            return(gfc_layers)
          }
)

setMethod("gfc", signature="SpatExtent",
          function(x, layers=c("treecover2000", "gain", "lossyear", "datamask"), version="2022-v1.10", CRS){
            library(terra)
            extent <- x
            if(!missing(CRS)) extent <- terra:project(extent, CRS, crs("epsg:4326"))
            gfc_layers <- gfc2rast(extent, layers=layers, version=version)
            return(gfc_layers)
          }
)

setMethod("gfc", signature="numeric",
          function(x, ..., xy=FALSE, layers=c("treecover2000", "gain", "lossyear", "datamask"), version="2022-v1.10", CRS){
            library(terra)
            extent <- ext(x, ..., xy=xy)
            if(!missing(CRS)) extent <- terra:project(extent, CRS, crs("epsg:4326"))
            gfc_layers <- gfc2rast(extent, layers=layers, version=version)
            return(gfc_layers)
          }
)


