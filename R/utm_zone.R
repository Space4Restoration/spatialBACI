#' Given a spatial object, calculate the UTM zone of the centroid
#'
#' For a line or polygon, the UTM zone of the centroid is given, after 
#' reprojecting the object into WGS-84.
#'
#' @export utm_zone
#' @import methods
#' @param x a longitude (with western hemisphere longitudes negative), a 
#' \code{Spatial} object, or a \code{SpatVector} object
#' @param y a latitude (with southern hemisphere latitudes negative), or 
#' missing (if x is a \code{Spatial} or \code{SpatVector} object)
#' @param proj4string if FALSE (default) return the UTM zone as a string (for 
#' example "34S" for UTM Zone 34 South). If TRUE, return a proj4string using 
#' the EPSG code as an initialization string.
#' @returns character
#' @examples
#' utm_zone(45, 10)
#' utm_zone(45, -10)
#' utm_zone(45, 10, proj4string=TRUE)
setGeneric("utm_zone", function(x, y, proj4string=FALSE) {
  standardGeneric("utm_zone")
})

utm_zone_calc <- function(x, y, proj4string) {
  if (x < -180 || x > 180) {
    stop("longitude must be between -180 and 180")
  }
  if (y < -90 || y > 90) {
    stop("latitude must be between -90 and 90")
  }
  if (y < -80 || y > 84) {
    warning("latitude is not between -80 and 84, consider using a different coordinate reference system")
  }

  # Adjustment for Norway zone 31/32
  zone_num <- floor((x + 180)/6) + 1
  if (y >= 56.0 && y < 64.0 && x >= 3.0 && x < 12.0) {
    zone_num <- 32
  }
  
  # Special zone_nums for Svalbard
  if (y >= 72.0 && y < 84.0) {
    if (x >= 0.0 && x < 9.0) {
      zone_num <- 31
    } else if (x >= 9.0 && x < 21.0) {
      zone_num <- 33
    } else if (x >= 21.0 && x < 33.0) {
      zone_num <- 35
    } else if (x >= 33.0 && x < 42.0) {
      zone_num <- 37
    }
  }
  
  if (y >= 0) {
    ns <- 'N'
  } else {
    ns <- 'S'
  }
  
  if (proj4string) {
    if (ns == 'N') {
      return(paste0('+init=epsg:326', sprintf('%02i', zone_num)))
    } else {
      return(paste0('+init=epsg:327', sprintf('%02i', zone_num)))
    }
  } else {
    return(paste0(zone_num, ns))
  }
}

#' @rdname utm_zone
#' @aliases utm_zone,numeric,numeric,logical-method
setMethod("utm_zone", signature("numeric", "numeric"),
          function(x, y, proj4string) {
            return(utm_zone_calc(x, y, proj4string))
          }
)

# #' @rdname utm_zone
# #' @importFrom sf st_centroid st_coordinates st_transform
# #' @aliases utm_zone,Spatial,missing,logical-method
# setMethod("utm_zone", signature(x='Spatial', y='missing'),
#           function(x, proj4string) {
#             x <- st_transform(x, st_crs('+init=epsg:4236'))
#             
#             centroid <- st_coordinates(st_centroid(x))
#             return(utm_zone_calc(centroid[1], centroid[2], proj4string))
#           }
# )

#' @rdname utm_zone
#' @importFrom terra centroids geom project
#' @aliases utm_zone,SpatVector,missing,logical-method
setMethod("utm_zone", signature(x='SpatVector', y='missing'),
          function(x, proj4string) {
            x <- project(x, "epsg:4326")
            centroid <- geom(centroids (x))
            return(utm_zone_calc(centroid[3], centroid[4], proj4string))
          }
)

