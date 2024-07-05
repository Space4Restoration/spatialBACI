#' Create median value composite image
#' 
#' Create a median value composite image from a data cube
#' 
#' @export
#' @importFrom gdalcubes bands reduce_time rename_bands
#' @param cube source data cube
#' @returns output data cube
#' 

cube_composite.median <- function(cube){
  names <- gdalcubes::bands(cube)$name
  comp_args <- list(x=cube)
  comp_args <- c(comp_args, lapply(names, function(x){paste0("median","(",x,")")}))
  composite <- do.call(gdalcubes::reduce_time, comp_args)
  
  rename_args <- as.list(names)
  names(rename_args) <- gdalcubes::bands(composite)$name
  rename_args$cube <- composite
  composite <- do.call(gdalcubes::rename_bands, rename_args)
  
  return(composite)
}

#' Create medoid composite image
#' 
#' Create a pseudo-medoid composite image from a data cube
#' 
#' This compositing method uses aan approximating of the medoid, selecting the observation with the smallest euclidean distance to the band-wise median position.
#' 
#' @export
#' @importFrom gdalcubes bands reduce_time rename_bands
#' @param cube source data cube
#' @returns output data cube
#' 

cube_composite.medoid <- function(cube){
  names <- gdalcubes::bands(cube)$name
  
  composite <- gdalcubes::reduce_time(cube, names=names, FUN=function(x){
    #calculate median over time
    x_median <- apply(x, 1, median, na.rm=TRUE)
    #calculate euclidean distance for each time step to median
    y <- (x-x_median)^2 |>
      apply(2,sum) |>
      sqrt()
    
    #select time step with minimum y
    if (all(is.na(y))) return(rep(NA,nrow(x)))
    return(x[, which.min(y)])
  })
  
  return(composite)
}




#' Composite cube
#'
#' Create a pixel-based composite image from a multitemporal data cube
#' 
#' See cube_composite.median, cube_composite.medoid
#'
#' @export
#' @param cube a multitemporal data cube
#' @param method compositing method: "median" or "medoid"
#' @param ... optional arguments for specific methods
#'
#' @returns data cube
#'  

cube_composite <- function(cube, method="median", ...){
  if(method=="median"){
    composite <- cube_composite.median(cube)
  } else if(method=="medoid"){
    composite <- cube_composite.medoid(cube)
  # } else if(method=maxNDVI){
  #   composite <- cube_composite.maxNDVI(cube,...)
  } else {
    stop("unknown method")
  }
  return(composite)
}



# cube_composite.maxNDVI <- function(cube, collection, endpoint){
# 
# }
# 
#   } else if(method=="maxNDVI"){
#     names <- gdalcubes::bands(cube)$name
# 
#     #Calculate and add NDVI band
#     cube_NDVI <- gdalcubes::apply_pixel(cube, keep_bands=TRUE, names="NDVI", FUN=cubeVI, VI="NDVI", collection=collection, endpoint=endpoint)
#     #Use NDVI band to reduce over time
#     composite <- gdalcubes::reduce_time(cube_NDVI, names=names, FUN=function(x){
#       if (all(is.na(x["NDVI",]))) return(rep(NA,length(names)))
#       return (x[names, which.max(x["NDVI",])])
#     })



# cube_composite.median2 <- function(cube){ #Could try a test of performance expr versus fun.
#   names <- gdalcubes::bands(cube)$name
#   composite <- gdalcubes::reduce_time(cube, names=names, FUN=function(x){
#     x_median <- apply(x, 1, median, na.rm=TRUE)
#     return(x_median)
#   })
#   return(composite)
# }
