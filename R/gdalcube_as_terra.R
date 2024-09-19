#' Data cube to SpatRaster
#' 
#' Convert a proxy data cube object to a terra SpatRaster object
#' 
#' Currently only works for data cubes with a single time dimension or a single band. To be improved.
#' 
#' @importFrom terra rast
#' @importFrom gdalcubes st_as_stars.cube
#' 
#' @export
#' 
#' @param cube data cube object
#' 
#' @returns a SpatRaster object
#' 
#' 
gdalcube_as_terra <- function(cube){
  
  cube_names <- names(cube)
  r <- gdalcubes::st_as_stars.cube(cube) |>
    terra::rast()
  #names(r) <- cube_names
  
  return(r)
}



is.cube <- function(obj) {
  if(!("cube" %in% class(obj))) {
    return(FALSE)
  }
  return(TRUE)
}

is.spatRaster <- function(obj){
  if("SpatRaster" %in% class(obj)){
    return(TRUE)
  }
  return(FALSE)
}

