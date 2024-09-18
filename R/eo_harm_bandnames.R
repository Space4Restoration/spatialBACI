#' Harmonize STAC endpoint/collection specific band names
#' 
#' Shorthand for eo_harm_bandnames.cube
#' 
#' 
#' @export
#' 
#' @param x data cube
#' @param ... additional arguments passed to \code{eo_harm_bandnames.cube}
#' 
#' 
eo_harm_bandnames <- function(x, ...){
  if ("cube" %in% class(x)){
    y <- eo_harm_bandnames.cube(x, ...)
    # } else if (class(x)=="SpatRaster"){
    #   y <- eo_harm_bandnames.rast(x, ...)
  } else {
    stop('unrecognized class of "X"')
  }
  return(y)
}

#' Harmonize STAC endpoint/collection-specific band names of data cubes
#' 
#' Replaces the STAC endpoint/collection-specific band names of a data cube by generic band names (i.e., "blue", "green", "red", "nir", ...).
#' This simplifies the calculation of Vegetation Indices, 
#' 
#' @importFrom gdalcubes rename_bands
#' 
#' @param cube a data cube
#' @param endpoint character. STAC endpoint
#' @param collection character. STAC collection
#' 
#' @export
#' 
eo_harm_bandnames.cube <- function(cube, endpoint, collection){
  
  bandnames <- names(cube)
  rename_args <- eo_mapBand(bandnames, endpoint=endpoint, collection=collection, invert=FALSE) |>
    as.list()
  rename_args$cube <- cube

  cube <- do.call(gdalcubes::rename_bands, rename_args)
  return(cube)
}

#eo_harm_bandnames.rast: to do?


#' Map band
#' 
#' Switch between generic and endpoint/collection-specific band name
#' 
#' Limited number of endpoints/collections implemented, more to be added
#' 
#' @export
#' @param bandnames character vector of bandnames
#' @param endpoint character. STAC endpoint
#' @param collection character. STAC collection
#' @param invert logical. Set to TRUE to obtain endpoint-specific bandnames from generic names
#' 
#' @returns character vector
#' 
eo_mapBand <- function(bandnames, endpoint, collection, invert=FALSE){
  
  if(endpoint==as.endpoint("PlanetaryComputer")){
    #Planetary Computer
    if(collection=="landsat-c2-l2"){
      dat <- c("blue", "blue",
               "green", "green",
               "red", "red",
               "nir", "nir08",
               "swir1", "swir16",
               "swir2", "swir22")
    } else if(collection=="sentinel-2-l2a"){
      dat <- c("blue", "B02",
               "green", "B03",
               "red" = "B04", 
               "red-edge 1", "B05",
               "red-edge 2", "B06",
               "red-edge 3", "B07",
               "nir", "B08",
               "swir1", "B11", 
               "swir2", "B12")
    } else {
      stop(paste0("Collection ", collection, " not implemented"))
    }
    
  } else if(endpoint==as.endpoint("lpcloud")){
    #LPDAAC cloud
    if(collection=="HLSL30.v2.0"){
            dat <- c("blue", "B02", 
               "green", "B03",
               "red", "B04", 
               "nir", "B05",
               "swir1", "B06",
               "swir2", "B07")
    } else if (collection=="HLSS30.v2.0"){
      dat <- c("blue", "B02",
               "green", "B03",
               "red" = "B04", 
               "red-edge 1", "B05",
               "red-edge 2", "B06",
               "red-edge 3", "B07",
               "nir", "B08",
               "swir1", "B11", 
               "swir2", "B12")
    } else {
      stop(paste0("Collection ", collection, " not implemented"))
    }
    
  } else {
    stop(paste0("Collection ", endpoint, " not implemented"))
  }
    
  bandsmap <- matrix(data=dat, ncol=2, byrow=TRUE)
  
  if(invert){
    bandnames_mapped <- bandsmap[match(bandnames, bandsmap[,1]),2]
  } else {
    bandnames_mapped <- bandsmap[match(bandnames, bandsmap[,2]),1]
  }
  names(bandnames_mapped) <- bandnames
  bandnames_mapped <- na.omit(bandnames_mapped)
  return(bandnames_mapped)
}
  
