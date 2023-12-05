#' Data cube to SpatRaster
#' 
#' Convert a proxy data cube object to a terra SpatRaster object
#' 
#' Probably only works with a 3-dimensional data cube, to be checked
#' 
#' @importFrom terra rast
#' @param cube data cube object
#' @returns a SpatRaster object
#' 
#' 
gdalcube_as_terra <- function(cube){
  r <- st_as_stars.cube(cube) |>
    terra::rast()
  return(r)
}
