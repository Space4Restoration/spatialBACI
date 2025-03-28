##  Various functions to check if an object belongs to a certain class
## ******


is.sf <- function(obj) {
  if(!("sf" %in% class(obj))) {
    return(FALSE)
  }
  return(TRUE)
}



is.cube <- function(obj) {
  if(!("cube" %in% class(obj))) {
    return(FALSE)
  }
  return(TRUE)
}

#should be replaced by is.SpatRaster (capital S) but keeping it in now to avoid bugs with typos
is.spatRaster <- function(obj){
  if("SpatRaster" %in% class(obj)){
    return(TRUE)
  }
  return(FALSE)
}


is.SpatRaster <- function(obj){
  if("SpatRaster" %in% class(obj)){
    return(TRUE)
  }
  return(FALSE)
}

is.SpatVector <- function(obj){
  if("SpatVector" %in% class(obj)){
    return(TRUE)
  }
  return(FALSE)
}


