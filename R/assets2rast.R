#' STAC assets to SpatRaster
#' 
#' Read selected assets from a STAC feature as a SpatRaster object
#' 
#' Assets to read must be of same extent and dimensions if read into mulitlayer SpatRaster. Set 'as_list=TRUE' to deal with assets with different extents or dimensions.
#' Assets must be readable, which may require prior authentication.
#' 
#' @seealso [\code{\link{assets2vrt}}]
#' @export
#' @import terra
#' 
#' @param feature a STAC feature: list element of STACItemCollection$features
#' @param assets character vector of assets names to extract
#' @param as_list boolean defining whether assets should be read as list of SpatRaster objects or as a single multilayer SpatRaster object
#' 
#' @returns a (list of) SpatRaster object(s)
#' 
assets2rast <- function(feature, assets, as_list=FALSE){
  fAssets <- feature$assets[which(names(feature$assets) %in% assets)]
  fURLs <- lapply(fAssets, function(x){x$href})
  #read all assets into list, remove possible scale/offset
  ras <- lapply(fURLs, function(x){r <- rast(x, vsi=TRUE); scoff(r) <- NULL; return(r)})
  ras <- ras[assets]
  
  #convert list to multilayers SpatRaster
  if(!isTRUE(as_list)) ras <- rast(ras)
  return(ras)
}

#' STAC assets to virtual raster
#' 
#' Read selected assets from a STAC featureCollection as a virtual SpatRaster object
#' 
#' Can be used when the different features in the featureCollection are tiles that must be combined.
#' 
#' @seealso [\code{\link{assets2rast}}]
#' @export
#' @import terra
#' @importFrom terra names
#' 
#' @param feature a STAC feature: list element of STACItemCollection$features
#' @param assets character vector of assets names to extract
#' @param as_list boolean defining whether assets should be read as list of SpatRaster objects or as a single multilayer SpatRaster object
#' 
#' @returns a (list of) SpatRaster object(s)
#' 
assets2vrt <- function(featureCollection, assets, as_list=FALSE){
  
  #TODO can this be combined with assets2rast, with different methods for feature and featureCollection?
  library(terra)
  
  aFun <- function(asset){
    URLs <- sapply(featureCollection$features, function(x){x$assets[[asset]]$href})
    URLs <- paste0("/vsicurl/", URLs)
    asset_vrt <- vrt(URLs)
    terra::names(asset_vrt) <- asset
    scoff(asset_vrt) <- NULL
    return(asset_vrt)
  }
  ras <- lapply(assets,aFun)
  
  if(!isTRUE(as_list)) ras <- rast(ras)
  
  return(ras)
}



