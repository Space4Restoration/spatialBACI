#' Vegetation Index composites
#' 
#' Create composite image time series of a selected vegetation index
#' 
#' Limited functionalities for now
#' Implemented vegetation indices:
#' *NDVI = (nir-red)/(nir+red)
#' *EVI
#' *SAVI
#' *MSAVI
#' *NDMI
#' *NBR
#' *NBR2 = (swir1-swir2)/(swir1+swir2)
#' *NDSI = (green-swir1)/(green+swir1)
#' @md
#' 
#' @export
#' 
#' @returns SpatRaster
#' 
#' @param x description
#' @param VI Vegetation Index (see details)
#' @param endpoint description
#' @param collection description#' 
#' @param years numeric vector of years for which composite image must be created
#' @param months numeric vector of months for which composite image must be created for each years
#' @param maxCloud numeric, maximum cloud cover percentage
#' @param maskOptions description
#' 
eo_VI_yearly.stac <- function(x, VI,
                              endpoint, collection, stacOptions=list(limit=1000, authOpt=list()),
                              years, 
                              months,
                              maxCloud=NULL,
                              maskOptions=list(maskLyr=maskLyrName.stac(endpoint, collection), maskSnow=FALSE, maskWater=FALSE)){
  
  items <- NULL
  for(year in years){
    if(missing(months)) {
      t0 <- as.Date(paste(year, "01", "01", sep="-"))
      t1 <- as.Date(paste(year, "12", "31", sep="-"))
    } else {
      t0 <- as.Date(paste(year, min(months), "01", sep="-"))
      if(max(months)==12){
        t1 <- as.Date(paste(year, max(months), "31", sep="-"), format="%Y-%m-%d") 
      } else{
        t1 <- as.Date(paste(year, max(months)+1, "01", sep="-"), format="%Y-%m-%d")-1
      }
    }
    
    s_args <- list(endpoint=endpoint, collection=collection, bbox=as.bbox(x), datetime=paste(t0,t1, sep="/"))
    s_args <- c(s_args, stacOptions)
    items_year <- do.call(eo_stac_search, s_args)
    
    if(is.null(items)){
      items <- items_year
    } else {
      items$features <- c(items$features, items_year$features)
    }
  }
  
  ##  Create gdalcubes image collection from STAC ItemCollection
  f_args <- list(s=items$features)
  asset_names <- reqBands(VI) |> mapBand(collection, endpoint)
  f_args$asset_names <- c(asset_names,maskOptions$maskLyr)
  if(!is.null(maxCloud)) f_args$property_filter <- function(x) {x[["eo:cloud_cover"]] < maxCloud}
  suppressWarnings(imgCollection <- do.call(gdalcubes::stac_image_collection, f_args))
  
  ##  Masking (if mask layer is provided in "maskOptions" argument)
  if(!is.null(maskOptions$maskLyr)) msk <- eo_mask.cube(maskOptions) else msk <- NULL
  
  v.ref <- gdalcubes::cube_view(srs=crs(x, proj=TRUE), 
                                extent=list(t0=paste(min(years), "01", "01", sep="-"), t1=paste(max(years), "12", "31", sep="-"),
                                            left= ext(x)$xmin, right= ext(x)$xmax,
                                            top=ext(x)$ymax, bottom=ext(x)$ymin),
                                dx=res(x)[1], dy=res(x)[2], dt="P1Y",
                                aggregation="median", resampling="bilinear")
  composite <- raster_cube(imgCollection, v.ref, msk) |>
    select_bands(asset_names)
  
  
  out <- cubeVI(composite, VI=VI, endpoint=endpoint, collection=collection) |>
    gdalcube_as_terra()
  return(out)
  
}




##TO CHECK
#' 
#' 
#'
#' 
#' #' Before/After Vegetation Index time series
#' #' 
#' #' Create yearly composite time series for the before and after period for an intervention
#' #' 
#' #' 
#' #' 
#' #' @export
#' #' 
#' #' @returns List of SpatRaster objects. The two list elements correspond to the before and after period, respectively 
#' #' 
#' #' @param spatRef SpatRaster defining spatial reference for output layers
#' #' @param index Character. Vegetation Index to be calculated
#' #' @param collection description
#' #' @param endpoint STAC endpoint
#' #' @param name description
#' #' @param intervention_year Numeric. Year of intervention
#' #' @param intervention_lag Numeric. Lag (in years) of response to intervention
#' #' @param nYears Numeric. Length of the before and after time series in years
#' #' @param nYears_before Numeric. Length of the before time series in years. Ignored if argument nYears is provided
#' #' @param nYears_after Numeric. Length of the after time series in years. Ignored if argument nYears is provided
#' #' @param months Numeric vector. Month(s) for which the yearly composites should be created
#' #' @param maxCloud Numeric. Maximum cloud cover of images used in compositing
#' #' 
#' #' 
#' 
#' BA_VI_ts <- function(spatRef,
#'                      index,
#'                      collection,
#'                      endpoint,
#'                      intervention_year,
#'                      intervention_lag=0,
#'                      nYears,
#'                      nYears_before,
#'                      nYears_after,
#'                      months=1:12,
#'                      maxCloud=60){
#'   
#'   if(!missing(nYears)){
#'     nYears_before <- nYears
#'     nYears_after <- nYears
#'   } else {
#'     if (missing(nYears_before) | missing(nYears_after)) stop("Argument nYears missing.")
#'   }
#'   
#'   years_before <- seq(intervention_year-nYears_before, intervention_year-1, 1)
#'   years_after <- seq(intervention_year+intervention_lag+1, intervention_year+intervention_lag+nYears_after, 1)
#'   
#'   out <- vector(mode="list", length=2)
#'   names(out) <- c("before", "after")
#'   
#'   out$before <- VI_composite(spatRef=spatRef,
#'                              VI=index,
#'                              collection=collection, endpoint=endpoint,
#'                              years=years_before, months=months,
#'                              maxCloud=maxCloud)
#'   
#'   out$after <- VI_composite(spatRef=spatRef,
#'                              VI=index,
#'                              collection=collection,endpoint=endpoint,
#'                              years=years_after, months=months,
#'                              maxCloud=maxCloud)
#'   
#'   return(out)
#' 
#' }    
#' 
