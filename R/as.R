##  ***********
##  Various functions to coerce to specific class/format



#' Coerce data cube to SpatRaster
#' 
#' Writes a data cube as a temporal file and reads that as SpatRaster object
#' 
#' Data cubes with n>1 in both the "time" and "bands" dimension will be transformed to a list of SpatRaster objects with the list elements corresponding to different time steps. Otherwise the function will return a single (multi-layer) SpatRaster.
#' 
#' @export
#' 
#' @importFrom gdalcubes write_tif nbands nt dimension_values
#' @importFrom terra rast
#' 
#' @param x data cube
#' 
#' @returns (list of) SpatRaster object(s)
#' 
as.SpatRaster <- function(x){
  
  stopifnot(is.cube(x))
  
  #write gdalcube as .tif file(s)
  x_files <- gdalcubes::write_tif(x)
  
  if(gdalcubes::nbands(x)==1 | gdalcubes::nt(x)==1){
    #Max three dimensions -> read as single SpatRaster
    if(gdalcubes::nt(x)>1){
      #Single band, multiple time steps
      r <- lapply(x_files, rast) |> rast()
      time(r) <- dimension_values(x)$t |> as.Date() 
    } else {
      #Multiple (or single) bands, single time
      r <- rast(x_files)
      time(r) <- dimension_values(x)$t |> as.Date() |> rep(times=nlyr(r))
    }
  } else {
    #Four dimensions -> load as list of SpatRasters
    r <- lapply(x_files, rast)
    for(i in 1:length(r)) time(r[[i]]) <- dimension_values(x)$t[i] |> as.Date() 
  }
  return(r)  
} 

#' As bounding box
#' 
#' Create a Bounding Box from a SpatRaster, SpatVector, or SpatExtent object in target CRS. Bounding box is a 4-element vector in the order c(xmin, ymin, xmax, ymax)
#' 
#' @import terra
#' 
#' @export
#' @param x a SpatRaster, SpatVector or SpatExtent object
#' @param crs_to character. Description of the Coordinate Reference System of the created bounding box, in PROJ.4, WKT or authority:code notation. Defaults to longitude/latitude.
#' @param crs_from If x has no CRS attached (i.e., if x is a SpatExtend or numeric), a character description of the Coordinate Reference System corresponding to the calues of x. Defaults to longitude/latitude.
#' @param xy logical. If x is a numeric vector of length 4 (or 4 numeric values), set this parameter to TRUE if coordinates are in order (xmin, ymin, xmax, ymax) or FALSE if in order (xmin, xmax, ymin, ymax).
#' @returns numeric vector of length four
#' 
#' 
setGeneric("as.bbox", function(x, ...){
  standardGeneric("as.bbox")
})

setMethod("as.bbox", signature="SpatRaster",
          function(x, crs_to=crs("epsg:4326")){
            x <- project(ext(x), crs(x), crs_to) 
            bbox <- x[c(1,3,2,4)]
            return(bbox)
          }
)

setMethod("as.bbox", signature="SpatVector",
          function(x, crs_to=crs("epsg:4326")){
            x <- project(ext(x), crs(x), crs_to) 
            bbox <- x[c(1,3,2,4)]
            return(bbox)
          }
)

setMethod("as.bbox", signature="SpatExtent",
          function(x, crs_from=crs("epsg:4326"), crs_to=crs("epsg:4326")){
            x <- project(x, crs_from, crs_to)
            bbox <- x[c(1,3,2,4)]
            return(bbox)
          }
)

setMethod("as.bbox", signature="numeric",
          function(x, ..., crs_from=crs("epsg:4326"), crs_to=crs("epsg:4326"), xy=TRUE){
            dots <- as.vector(unlist(list(...)))
            x <- c(x, dots)
            n <- length(x)
            if (n != 4) {
              stop("as.bbox: expected four numbers")
            }
            names(x) <- NULL
            x <- ext(x, xy=xy)
            x <- project(x, crs_from, crs_to)
            bbox <- x[c(1,3,2,4)]
            return(bbox)
          }
)

#' @noRd
setMethod("as.bbox", signature="NULL",
          function(x){
            return(NULL)
          }
)

#' As datetime
#' 
#' Format a range of dates as STAC datetime format 
#' 
#' If x is a single numeric or character formatted as YYYYMMDD, the output will be "YYYY-MM-DD", if x is a numeric or character vector of length 2, formatted as YYYYMMDD, output wil be "YYYY-MM-DD/YYYY-MM-DD"
#' If x is a list the list elements should be named "years" (required), and "months" and "days" (optional). E.g., list(years=2011:2012, months=5:7) returns 2011-05-01/2012-07-31". List element named "days" will be ignored if "months" is not provided.
#' 
#' @export
#' @param x a numeric, character or list (see Details)
#' @returns a character string in STAC datetime format
#' 
#' 
setGeneric("as.datetime", function(x, ...){
  standardGeneric("as.datetime")
})

setMethod("as.datetime", signature="NULL",
          function(x){
            return(NULL)
          }
)

setMethod("as.datetime", signature="numeric",
          function(x, ...){
            dots <- as.vector(unlist(list(...)))
            x <- c(x, dots)
            n <- length(x)
            if (n == 2) {
              datetime <- as.Date(as.character(x), format="%Y%m%d") |>
                paste(collapse="/")
            } else {
              datetime <- as.character(as.Date(as.character(x), format="%Y%m%d"))
            }
            return(datetime)
          }
)

setMethod("as.datetime", signature="list",
          function(x, ...){
            if(!"years" %in% names(x)) stop('as.datetime: list element "years" missing')
            if(!"months" %in% names(x)) return(paste(c(min(x$years), max(x$years)), c("01", "12"), c("01", "31"), sep="-", collapse=("/")))
            if(!"days" %in% names(x)){
              datetimeMin <- as.Date(paste(min(x$years), min(x$months), "01", sep="-"))
              if(max(x$months)==12){
                datetimeMax <- as.Date(paste(max(x$years), max(x$months), "31", sep="-"), format="%Y-%m-%d") 
              } else{
                datetimeMax <- as.Date(paste(max(x$years), max(x$months)+1, "01", sep="-"), format="%Y-%m-%d")-1
              }
              return(paste(c(datetimeMin, datetimeMax), collapse="/"))
            }
            return(paste(c(min(x$years), max(x$years)), c(min(x$months), max(x$months)), c(min(x$days), max(x$days)), sep="-", collapse=("/")))
          }
)

setMethod("as.datetime", signature="character",
          function(x, ...){
            dots <- as.vector(unlist(list(...)))
            x <- c(x, dots)
            n <- length(x)
            if (n == 2) {
              datetime <- as.Date(x, format="%Y%m%d") |>
                paste(collapse="/")
            } else {
              datetime <- as.Date(x, format="%Y%m%d")
            }
            return(datetime)
          }
)


#' As collection
#' 
#' Format to STAC collection name for some commonly used collections and STAC endpoints
#' 
#' Possible values:
#' 
#'   For endpoint=as.endpoint("PlanetaryComputer"):
#'   * For collection "landsat-c2-l2": "landsat", "landsat-c2-l2"
#'   * For collection "sentinel-2-l2a": "s2", "sentinel2", "sentinel-2-l2a"
#' 
#' @md
#' 
#' @export
#' @param x character. Simplified collection name (see Details)
#' @param endpoint character. STAC endpoint (see as.endpoint)
#' @returns character 
#' 
#' 
as.collection <- function(x, endpoint=as.endpoint("PlanetaryComputer")){
  if(endpoint==as.endpoint("PlanetaryComputer")){
    collection <- switch(tolower(x),
                         landsat="landsat-c2-l2",
                         'landsat-c2-l2'="landsat-c2-l2",
                         s2="sentinel-2-l2a",
                         sentinel2="sentinel-2-l2a",
                         'sentinel-2-l2a'="sentinel-2-l2a",
                         x)
  } else {
    stop("STAC endpoint and/or collection not implemented")
  }
  return(collection)
}


#' As endpoint
#' 
#' Simplified representation of some STAC API endpoints 
#' 
#' Defaults to "https://planetarycomputer.microsoft.com/api/stac/v1" (Planetary Computer). 
#' 
#' Possible values:
#'  * "lpdaac" = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
#'  * "lpcloud" = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
#'  * "planetarycomputer" = "https://planetarycomputer.microsoft.com/api/stac/v1",
#'  * "planetary computer" = "https://planetarycomputer.microsoft.com/api/stac/v1"
#' 
#' @export
#' @param name character. Simplified endpoint name
#' @returns a character
#' 
#' 
as.endpoint <- function(name){
  if(missing(name)) {
    endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  } else {
    endpoint <- switch(tolower(name),
                       "lpdaac" = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
                       "lpcloud" = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
                       "planetarycomputer" = "https://planetarycomputer.microsoft.com/api/stac/v1",
                       "planetary computer" = "https://planetarycomputer.microsoft.com/api/stac/v1",
                       stop("Endpoint not implemented")
    )
  }
  return(endpoint)
}



