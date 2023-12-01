#' As bounding box
#' 
#' Create a Bounding Box from a SpatRaster or SpatExtent object in target CRS. Bounding box is a 4-element vector in the order c(xmin, ymin, xmax, ymax)
#' 
#' @import terra
#' 
#' @export
#' @param x a SpatRaster, SpatVector or SpatExtent object
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
#' Format to datetime 
#' 
#' 
#' @export
#' @param x a numeric (or character?)
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
            if (n != 2) {
              stop("as.datetime: expected two numbers")
            }
            datetime <- as.character(x) |> 
              as.Date(format="%Y%m%d") |>
              paste(collapse="/")
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
              return(datetime)
            } else {
              datetime <- x
              return(x)
            }
          }
)


#' As collection
#' 
#' Format to STAC collection name for some commonly used collections and STAC endpoints
#' 
#' 
#' @param x character
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
#' Some endpoints of STAC APIs because I'm too lazy to remember/search 
#' 
#' Defaults to Planetary Computer
#' 
#' @export
#' @param name character
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



