#' Vegetation Index composites
#' 
#' Create composite image time series of a selected vegetation index
#' 
#' Limited functionalities for now
#' Implemented vegetation indices:
#' *NDVI = (nir-red)/(nir+red)
#' *EVI
#' *SAVI
#' *MSAVI
#' *NDMI
#' *NBR
#' *NBR2 = (swir1-swir2)/(swir1+swir2)
#' *NDSI = (green-swir1)/(green+swir1)
#' @md
#' 
#' @export
#' 
#' @returns SpatRaster
#' 
#' @param endpoint description
#' @param collection description
#' @param VI Vegetation Index (see details)
#' @param years numeric vector of years for which composite image must be created
#' @param months numeric vector of months for which composite image must be created for each years
#' @param maxCloud numeric, maximum cloud cover percentage
#' 
VI_composite <- function(endpoint, collection, VI, 
                         spatRef, 
                         years, months, maxCloud){
  out <- eo_stac_yearly_composites(endpoint=endpoint,
                                   collection=stac_collection,
                                   spatRef = spatRef,
                                   years = years,
                                   months = months,
                                   maxCloud=maxCloud) |>
    cubeVI(VI=VI, endpoint=stac_endpoint, collection=stac_collection) |>
    gdalcube_as_terra()
  return(out)
}


