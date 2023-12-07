#' EO assets
#' 
#' Default assets for L/S2
#' 
#' To be updated for correct naming under PC/LPDAAC/..., For now only PC Landsat and Sentinel-2 implemented
#' 
#' @export
#' @param feature description
#' @param collection description
#' @param meta description
#' 
#' @returns character vector
#' 
eo_assets <- function(feature, collection, meta=TRUE){

  if(missing(collection)) collection <- feature$collection
  if(collection=='landsat-c2-l2'){
    assets <- c("blue" , "green", "red", "nir08", "swir16", "swir22")
    if(meta) assets <- c(assets,"qa_pixel")
  } else if(collection=='sentinel-2-l2a'){
    assets <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")
    if(meta) assets <- c(assets, "SCL")
  } else {
    stop("Only default assets for landsat-c2-l2/sentinel-2-l2a implemented")
    assets <- NULL
  }
  return(assets)
}

#' Map band
#' 
#' Map generic band name to collection/endpoint-specific band name
#' 
#' Limited number of collections/endpoints implemented, more to be added
#' 
#' @export
#' @param bands description
#' @param collection description
#' @param endpoint description
#' 
#' @returns character vector
#' 
mapBand <- function(bands, collection, endpoint){
  #Re-assign band names to generically address Landsat/Sentinel-2 bands for calculation of VIs
  mapFun <- function(band, collection){
    
    if(collection=="landsat-c2-l2"){
      #Landsat (for planetary computer, check for other catalogs)
      name <- switch(band,
                     blue = "blue",
                     green = "green",
                     red = "red",
                     nir = "nir08",
                     swir1 = "swir16",
                     swir2 = "swir22")
    } else if(collection=="HLSL30.v2.0"){
      #HLS Landsat (for LPDAAC, check for other catalogs)
      name <- switch(band,
                     blue = "B02", 
                     green = "B03",
                     red = "B04", 
                     nir = "B05",
                     swir1 = "B06",
                     swir2 = "B07")
    } else if(collection=="sentinel-2-l2a" | collection=="HLSS30.v2.0"){
      #Sentinel (PC), HLS Sentinel (LPDAAC)  
      name <- switch(band,
                     "blue" = "B02", 
                     "green" = "B03",
                     "red" = "B04", 
                     "red-edge 1" = "B05",
                     "red-edge 2" = "B06",
                     "red-edge 3" = "B07",
                     "nir" = "B08",
                     "swir1" = "B11", 
                     "swir2" = "B12")
    } else {
      stop(paste0("Collection ", collection, " not recognized"))
    }
    return(name)
  }
  return(unname(sapply(bands, mapFun, collection)))
}

#' Required bands for selected index
#' 
#' Return (generic) band names required for calculating specified vegetation index
#' 
#' Possible values of \code{index} are "ndvi", "evi", "savi", "msavi", "ndmi", "nbr", "nbr2", "ndsi"
#' 
#' @export
#' @param index description
#' @returns character vector
#' @seealso [cubeVI()]
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

