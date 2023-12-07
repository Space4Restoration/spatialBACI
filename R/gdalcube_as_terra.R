#' Data cube to SpatRaster
#' 
#' Convert a proxy data cube object to a terra SpatRaster object
#' 
#' @importFrom terra rast
#' @importFrom gdalcubes st_as_stars.cube
#' @export
#' @param cube data cube object
#' @returns a SpatRaster object
#' 
#' 
gdalcube_as_terra <- function(cube){
  r <- st_as_stars.cube(cube) |>
    terra::rast()
  return(r)
}
