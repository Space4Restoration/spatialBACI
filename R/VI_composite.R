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






#' Before/After Vegetation Index time series
#' 
#' Create yearly composite time series for the before and after period for an intervention
#' 
#' 
#' 
#' @export
#' 
#' @returns List of SpatRaster objects. The two list elements correspond to the before and after period, respectively 
#' 
#' @param spatRef SpatRaster defining spatial reference for output layers
#' @param index Character. Vegetation Index to be calculated
#' @param collection description
#' @param endpoint STAC endpoint
#' @param name description
#' @param intervention_year Numeric. Year of intervention
#' @param intervention_lag Numeric. Lag (in years) of response to intervention
#' @param nYears Numeric. Length of the before and after time series in years
#' @param nYears_before Numeric. Length of the before time series in years. Ignored if argument nYears is provided
#' @param nYears_after Numeric. Length of the after time series in years. Ignored if argument nYears is provided
#' @param months Numeric vector. Month(s) for which the yearly composites should be created
#' @param maxCloud Numeric. Maximum cloud cover of images used in compositing
#' 
#' 

BA_VI_ts <- function(spatRef,
                     index,
                     collection,
                     endpoint,
                     intervention_year,
                     intervention_lag=0,
                     nYears,
                     nYears_before,
                     nYears_after,
                     months=1:12,
                     maxCloud=60){
  
  if(!missing(nYears)){
    nYears_before <- nYears
    nYears_after <- nYears
  } else {
    if (missing(nYears_before) | missing(nYears_after)) stop("Argument nYears missing.")
  }
  
  years_before <- seq(intervention_year-nYears_before, intervention_year-1, 1)
  years_after <- seq(intervention_year+intervention_lag+1, intervention_year+intervention_lag+nYears_after, 1)
  
  out <- vector(mode="list", length=2)
  names(out) <- c("before", "after")
  
  out$before <- VI_composite(spatRef=spatRef,
                             VI=index,
                             collection=collection, endpoint=endpoint,
                             years=years_before, months=months,
                             maxCloud=maxCloud)
  
  out$after <- VI_composite(spatRef=spatRef,
                             VI=index,
                             collection=collection,endpoint=endpoint,
                             years=years_after, months=months,
                             maxCloud=maxCloud)
  
  return(out)

}    

