#' EO cube from STAC
#' 
#' Create a gdalcubes data cube for EO images retrieved through STAC
#' 
#' Warning: masking only works now when asset_names are explicitely defined
#' 
#' @export
#' @import gdalcubes
#' 
#' @param x spatRaster. Spatial reference
#' @param endpoint STAC endpoint
#' @param collection STAC collection
#' @param t0 t0
#' @param t1 t1
#' @param dt dt
#' @param asset_names description
#' @param aggregations description
#' @param resampling description
#' @param maxCloud description
#' @param maskOptions description
#' @param stacOptions Optional arguments to be passed to eo_stac_search, e.g. authentication options
#' 
#' 
#' 
eo_cube.stac <- function(x,
                         endpoint, collection,
                         t0, t1, dt="P1D",
                         asset_names=NULL,
                         maxCloud=NULL,
                         maskOptions=list(NULL),
                         stacOptions=list(limit=1000, authOpt=list()),
                         aggregation="first", resampling="bilinear"){
  
  ##  Format dates
  if(is.numeric(t0)) t0 <- as.character(t0) |> as.Date(format="%Y%m%d") |> as.character()
  if(is.numeric(t1)) t1 <- as.character(t1) |> as.Date(format="%Y%m%d") |> as.character()
  
  ##  Create STAC ItemCollection
  s_args <- list(endpoint=endpoint, collection=collection, bbox=as.bbox(x), datetime=paste(t0,t1, sep="/"))
  s_args <- c(s_args, stacOptions)
  itemCollection <- do.call(eo_stac_search, s_args)

  ##  Create gdalcubes image collection from STAC ItemCollection
  f_args <- list(s=itemCollection$features)
  if(!is.null(asset_names)) f_args$asset_names <- c(asset_names,maskOptions$maskLyr)
  if(!is.null(maxCloud)) f_args$property_filter <- function(x) {x[["eo:cloud_cover"]] < maxCloud}
  suppressWarnings(imgCollection <- do.call(gdalcubes::stac_image_collection, f_args))
  
  ##  Masking (only if mask layer is provided in "maskOptions" argument)
  if(!is.null(maskOptions$maskLyr)) msk <- eo_image_mask(maskOptions) else msk <- NULL

  ##  Define Cube View
  v.ref <- gdalcubes::cube_view(srs=crs(x, proj=TRUE), 
                                extent=list(t0=t0, t1=t1,
                                            left= ext(x)$xmin, right= ext(x)$xmax,
                                            top=ext(x)$ymax, bottom=ext(x)$ymin),
                                dx=res(x)[1], dy=res(x)[2], dt=dt,
                                aggregation=aggregation, resampling=resampling)
  
  imgCube <- gdalcubes::raster_cube(imgCollection, v.ref, msk)
  if(!is.null(asset_names)) imgCube <- gdalcubes::select_bands(imgCube, asset_names)
  
  return(imgCube)
}
  

#' Create a median value composite image
#' 
#' Create a median value composite image from a data cube
#' 
#' @export
#' 
#' @importFrom gdalcubes bands reduce_time rename_bands
#' 
#' @param cube source data cube
#' 
#' 
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

# cube_composite.median2 <- function(cube){ #Could try a test of performance expr versus fun.
#   names <- gdalcubes::bands(cube)$name
#   composite <- gdalcubes::reduce_time(cube, names=names, FUN=function(x){
#     x_median <- apply(x, 1, median, na.rm=TRUE)
#     return(x_median)
#   })
#   return(composite)
# }


cube_composite.pseudoMedoid <- function(cube){
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
#'
#'
#'
#'
#'


cube_composite <- function(cube, method="median", collection, endpoint){



  if(method=="median"){
    composite <- cube_composite.median(cube)
  }
  return(composite)
}


    
    
    
#'   } else if(method=="maxNDVI"){
#'     names <- gdalcubes::bands(cube)$name
#' 
#'     #Calculate and add NDVI band
#'     cube_NDVI <- gdalcubes::apply_pixel(cube, keep_bands=TRUE, names="NDVI", FUN=cubeVI, VI="NDVI", collection=collection, endpoint=endpoint)
#'     #Use NDVI band to reduce over time
#'     composite <- gdalcubes::reduce_time(cube_NDVI, names=names, FUN=function(x){
#'       if (all(is.na(x["NDVI",]))) return(rep(NA,length(names)))
#'       return (x[names, which.max(x["NDVI",])])
#'     })
#' 
#'     return(composite)
#' 
#'   } else if(method=="pseudo-medoid"){
#'     names <- gdalcubes::bands(cube)$name
#' 
#'     # cube_dist <- gdalcubes::apply_time(cube, keep_bands=FALSE, FUN=function(x){
#'     #   x-rowMeans(x, na.rm=TRUE)
#'     # })
#'     #
#'     #
#'     #
#'     # cube_dist <- gdalcubes::apply_time(cube, names="dist", keep_bands=TRUE, FUN=function(x){
#'     #   return(sqrt(colSums((x-rowMeans(x, na.rm=TRUE))^2)))
#'     # })
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#'     # #Calculate band-wise median composite
#'     # median_composite <- cube_composite.median(cube)
#'     #
#'     # dist_to_median <-
#'     #
#'     # cube - median_composite
#' 
#' 
#' 
#' 
#' 
#' 
#'     #nothing yet
#'     return(NULL)
#' 
#'   }else {
#'     return(NULL)
#' 
#'   }
#' 
#' 
#' }
#' 
#' 
