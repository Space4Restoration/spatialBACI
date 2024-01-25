#' Geographic distance to feature of interest
#'
#' Calculates (minimum) geographic distance in meters from the cells of a reference SpatRaster or the feature in reference SpatVector to the features in the target SpatVector 
#' 
#' Basically rast::distance but with reprojection CRS if needed, and in case of SpatVector-SpatVector returning the minimum distance rather than a matrix. Perhaps also allow different types of vector formats (e.g. sf)?
#' 
#' @export
#' @import methods
#' @importFrom terra distance project crs
#' 
#' @param x SpatRaster or Spatvector for which distances must calculated
#' @param y a terra::SpatVector or sf::sf vector layer (points, lines or polygons) to which distances must be calculated
#' @param ... Additional arguments to terra::distance
#' 
#' @returns SpatRaster or numerical vector


setGeneric("distanceToVector", function(x, y, ...){
  standardGeneric("distanceToVector")
})

setMethod("distanceToVector", signature("SpatRaster", "SpatVector"),
          function(x, y, ...){
            if(crs(x) != crs(y)) y <- project(y,x)
            d <- distance(x, y, ...)
            return(d)
})

setMethod("distanceToVector", signature("SpatRaster", "sf"),
          function(x, y, ...){
            y <- vect(y)
            if(crs(x) != crs(y)) y <- project(y,x)
            d <- distance(x, y, ...)
            return(d)
})

setMethod("distanceToVector", signature("SpatVector", "SpatVector"),
          function(x, y, ...){
            if(crs(x) != crs(y)) y <- project(y,x)
            d <- distance(x,y, pairwise=FALSE)
            d <- apply(d, 1, min)
            return(d)
})

setMethod("distanceToVector", signature("SpatVector", "sf"),
          function(x, y, ...){
            y <- vect(y)
            if(crs(x) != crs(y)) y <- project(y,x)
            d <- distance(x,y, pairwise=FALSE)
            d <- apply(d, 1, min)
            return(d)
})
