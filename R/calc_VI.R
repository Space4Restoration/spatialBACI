# #overarching function
# #to be added, also make function for spatRaster
# 
# calc_VI <- function(x, ...){
#   if ("cube" %in% class(x)){
#     vi <- calc_VI.cube(x, ...)
#   # } else if (class(x)=="SpatRaster"){
#   #   vi <- calc_VI.rast(x, ...)
#   } else {
#     stop('unrecognized class of "X"')
#   }
#   return(vi)
# }

#' Calculate Vegatation Index
#' 
#' Calculates Vegetation Index for a data cube of EO (surface reflectance) images
#' 
#' Implemented vegetation indices:
#' \itemize{
#'   \item NDVI = (nir-red)/(nir+red)
#'   \item NDMI = (nir-swir1)/(nir+swir1)
#'   \item NBR = (nir-swir2)/(nir+swir2)
#'   \item NBR2 = (swir1-swir2)/(swir1+swir2)
#'   \item NDSI = (green-swir1)/(green+swir1)
#'   \item EVI = 2.5*(nir-red)/(nir+6*red-7.5*blue+1)
#'   \item SAVI = 1.5*(nir-red)/(nir+red+0.5)
#'   \item MSAVI = (2*nir+1-sqrt((2*nir+1)^2-8*(nir-red)))/2
#' }
#' 
#' Scale factor has to be applied when reflectance saved as integer instead of range [0-1]. To be checked for different endpoints
#' 
#' @importFrom gdalcubes select_bands apply_pixel rename_bands
#' @export
#' 
#' @param cube data cube with spectral bands
#' @param VI vegetation index
#' @param scale scale of source surface reflectance data
#' 
#' @returns proxy data cube object
#' 
calc_VI.cube <- function(cube, VI, 
                         scale=0.00001){
  FUN <- switch(tolower(VI),
                ndvi = function(x){(x["nir"]-x["red"]) / 
                                   (x["nir"]+x["red"])},
                ndmi = function(x){(x["nir"]-x["swir1"])/
                                   (x["nir"]+x["swir1"])},
                nbr  = function(x){(x["nir"]-x["swir2"])/
                                   (x["nir"]+x["swir2"])},
                nbr2 = function(x){(x["swir1"]-x["swir2"])/
                                   (x["swir1"]+x["swir2"])},
                ndsi = function(x){(x["green"]-x["swir1"])/
                                   (x["green"]+x["swir1"])},
                
                # Functions requiring correct scaling of the bands are calculated using "expr", since I don't manage to pass the scaling constant to the functions
                # savi = function(x){1.5*
                #                   (x["nir"]-x["red"])/
                #                   (x["nir"]+x["red"]+0.5*scale)},
                # msavi= function(x){(2*x["nir"]+1-sqrt((2*x["nir"]+1)^2-8*(x["nir"]-x["red"])))/2},
                # evi  = function(x){2.5*(x["nir"]*scale-x["red"]*scale/((x["nir"]*scale+6*x["red"]*scale-7.5*x["blue"]*scale)+1))},
                evi =  paste0("2.5*(nir*",format(scale,scientific=FALSE),"-red*",format(scale,scientific=FALSE),")/(nir*",format(scale,scientific=FALSE),"+6*red*",format(scale,scientific=FALSE),"-7.5*blue*",format(scale,scientific=FALSE),"+1)"),
                savi = paste0("1.5*(nir-red)/(nir+red+0.5/",format(scale,scientific=FALSE),")"),
                msavi = paste0("(2*nir*",format(scale,scientific=FALSE),"+1-sqrt((2*nir*",format(scale,scientific=FALSE),"*+1)^2-8*(nir*",format(scale,scientific=FALSE),"-red*",format(scale,scientific=FALSE),")))/2"),
                stop(paste0("Unkonwn VI")))

  if(VI %in% c("evi", "savi", "msavi")){
    cube_vi <- apply_pixel(cube, expr=FUN, names=VI)
  } else {
    cube_vi <- apply_pixel(cube, names=VI, FUN=FUN)
  }
  return(cube_vi)
}

#' Required bands for selected index
#' 
#' Return (generic) band names required for calculating specified vegetation index
#' 
#' Possible values of \code{index} are "ndvi", "evi", "savi", "msavi", "ndmi", "nbr", "nbr2", "ndsi"
#' 
#' @export
#' @param index character. Vegetation Index.
#' @returns character vector
#' 
#' @noRd
#' 
reqBands <- function(index){
  bands <- switch(tolower(index),
                  #including the individual bands so that generic functions for an index also work with single band
                  blue = "blue",
                  green = "green",
                  red = "red",
                  nir = "nir",
                  swir1 = "swir",
                  swir2 = "swir2",
                  vre1 = "vre1",
                  vre2 = "vre2",
                  vre3 = "vre3",
                  vre4 = "vre4",
                  ndvi=c("red", "nir"),
                  evi=c("blue", "red", "nir"),
                  savi=c("red", "nir"),
                  msavi=c("red", "nir"),
                  ndmi=c("nir", "swir1"),
                  nbr=c("nir", "swir2"),
                  nbr2=c("swir1", "swir2"),
                  ndsi=c("green", "swir1")
  )
  return(bands)
}
