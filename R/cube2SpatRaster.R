#' Data cube to SpatRaster
#' 
#' Convert a proxy data cube object to 
#' 
#' Probably only works with a 3-dimensional data cube, to be checked
#' 
#' @importFrom stars st_as_stars
#' @importFrom terra rast
#' @param cube data cube object
#' @returns a SpatRaster object
#' 
#' 
cube2SpatRaster <- function(cube){
  r <- stars::st_as_stars(cube) |>
    terra::rast()
  return(r)
}
