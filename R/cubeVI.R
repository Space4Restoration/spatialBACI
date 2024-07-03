#' Calculate Vegatation Index
#' 
#' Calculates Vegetation Index for a data cube of EO (surface reflectance) images
#' 
#' Implemented vegetation indices:
#' *NDVI = (nir-red)/(nir+red)
#' *EVI
#' *SAVI
#' *MSAVI
#' *NDMI
#' *NBR
#' *NBR2 = (swir1-swir2)/(swir1+swir2)
#' *NDSI = (green-swir1)/(green+swir1)
#' 
#' Scale factor has to be applied when reflectance saved as integer instead of range 0-1. To be checked for different endpoints
#' 
#' @importFrom gdalcubes select_bands apply_pixel rename_bands
#' @export
#' 
#' @param cube data cube with spectral bands
#' @param VI vegetation index
#' @param endpoint STAC endpoint
#' @param collection STAC collection
#' @param scale scale of source surface reflectance data
#' 
#' @returns proxy data cube object
#' 
#' @seealso [reqBands()]
#' 
cubeVI <- function(cube, VI, endpoint, collection, scale=0.00001){
  
  req <- reqBands(VI) 
  req_map <- mapBand(req, endpoint=endpoint, collection=collection)
  rename_args <- as.list(req)
  names(rename_args) <- req_map
  
  rename_args$cube <- select_bands(cube, req_map)
  
  cube_sub <- do.call(rename_bands, rename_args)
  
  # #These seems to be something wrong here when using apply_pixel(x,FUN), using apply_pixel(x, expr) instead.
  # #Should be debugged
  # FUN <- switch(tolower(VI),
  #               ndvi = function(x){(x["nir"]-x["red"])/(x["nir"]+x["red"])},
  #               evi = function(x){2.5*(x[nir]*scale-x[red]*scale/((x[nir]*scale+6*x[red]*scale-7.5*x[blue]*scale)+1))},
  #               savi = function(x){((x[nir]*scale-x[red]*scale)/(x[nir]*scale+x[red]*scale+0.5))*(1.5)},
  #               msavi = function(x){(2*x[nir]*scale+1-sqrt((2*x[nir]*scale+1)^2-8*(x[nir]*scale-x[red]*scale)))/2},
  #               ndmi = function(x){(x[nir]-x[swir1])/(x[nir]+x[swir1])},
  #               nbr = function(x){(x[nir]-x[swir2])/(x[nir]+x[swir2])},
  #               nbr2 = function(x){(x[swir1]-x[swir2])/(x[swir1]+x[swir2])},
  #               ndsi = function(x){(x[green]-x[swir1])/(x[green]+x[swir1])},
  #               stop(paste0("Invalid VI")))
  # cube_vi <- apply_pixel(cube_sub, names=VI, FUN=FUN)

  expr <- switch(tolower(VI),
                 ndvi = "(nir-red)/(nir+red)",
                 evi =  paste("2.5*(nir*scale-red*","/((nir*","+6*red*","-7.5*blue*",")+1))", sep=format(scale,scientific=FALSE)),
                 savi = paste("((nir*","-red*",")/(nir*","+red*","+0.5))*1.5", sep=format(scale,scientific=FALSE)),
                 msavi = paste("(2*nir*","+1-sqrt((2*nir*","+1)^2-8*(nir*","-red*",")))/2", sep=format(scale,scientific=FALSE)),
                 ndmi = "(nir-swir1)/(nir+swir1)",
                 nbr = "(nir-swir2)/(nir+swir2)",
                 nbr2 = "(swir1-swir2)/(swir1+swir2)",
                 ndsi = "(green-swir1)/(green+swir1)",
                 stop(paste0("Invalid VI")))
  cube_vi <- apply_pixel(cube_sub, expr=expr, names=VI)
  return(cube_vi)
}
  