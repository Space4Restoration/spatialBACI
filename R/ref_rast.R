
#' Create a reference SpatRaster object
#' 
#' Define a spatial reference spatRaster object based on a polygon of pair of coordinates, and additional spatial arguments.
#' 
#' @export ref_rast
#' 
#' @import methods
#' @importFrom terra ext project extend rast
#' 
#' @param x longitude (with western hemisphere longitudes negative) or a \code{SpatVector} object
#' @param y latitude (with southern hemisphere latitudes negative), or missing (if x is a \code{SpatVector} object)
#' @param crs description of the Coordinate Reference System in PROJ.4, WKT or authority:code notation.
#' Defaults to the UTM zone of center coordinates
#' @param buffer numeric vector of 1, 2, or 4 elements. Indicates by how much (in units of crs) the spatial extent of x/y should be enlarged on each side. 
#' A single positive integer when the distance in x and y to be added is equal; or 2 number (x, y), or four (lower x, upper x, lower y, upper y).
#' @param round_coords logical or integer. Should the coordinates of the reference SpatRaster be rounded.
#' If TRUE, coordinates are rounded to the nearest integer; if a positive numeric rounded to the corresponding decimal; 
#' if a negative integer rounded to the corresponding power of 10 (e.g., \code{round_corrs=2} round to the nearest 100).
#' @param ... additional arguments passed on to \code{terra::rast}

#' @returns SpatRaster

#' @examples
#' ref_rast(30, 45, resolution=10, buffer=200, round_coords=-1)
setGeneric("ref_rast", function(x, y, crs, resolution, buffer=0, round_coords=FALSE, ...) {
  standardGeneric("ref_rast")
})

#' @rdname ref_rast
#' @aliases ref_rast,SpatVector,missing-method
setMethod("ref_rast", signature(x='SpatVector', y='missing'),
          function(x, crs, resolution, buffer=0, round_coords=FALSE, ...) {
            
            if(missing(crs)) crs <- utm_zone(x, proj4string = TRUE)
            
            extent <- terra::ext(x) |>
              terra::project(crs(x), crs) |> 
              terra::extend(buffer)
            
            if(!isFALSE(round_coords)){
              if(isTRUE(round_coords)) round_coords <- 0
              extent <- base::round(extent, round_coords)
            }
            
            return(terra::rast(extent=extent, crs=crs, resolution=resolution, ...))
            
          }
)

#' @rdname ref_rast
#' @aliases ref_rast,numeric,numeric-method
setMethod("ref_rast", signature(x='numeric', y='numeric'),
          function(x, y, crs, resolution, buffer=0, round_coords=FALSE, ...) {
            
            if(missing(crs)) crs <- utm_zone(x, y, proj4string = TRUE)
            
            extent <- ext(x, x, y, y) |>
              project('epsg:4236', crs) |> 
              extend(buffer)
            
            if(!isFALSE(round_coords)){
              if(isTRUE(round_coords)) round_coords <- 0
              extent <- terra::round(extent, round_coords)
            }
            
            return(rast(extent=extent, crs=crs, resolution=resolution, ...))
            
          }
)