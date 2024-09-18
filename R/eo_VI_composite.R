
#' Vegetation Index composites
#' 
#' Create yearly composite image time series of a selected vegetation index
#' 
#' Limited functionalities for now, see \code{calc_VI.cube} for implemented vegetation indices:
#' 
#' @export
#' 
#' @importFrom gdalcubes cube_view raster_cube select_bands
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
                              maskOptions=list(maskLyr=eo_maskLyrName.stac(endpoint, collection), maskSnow=FALSE, maskWater=FALSE)){
  
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
  asset_names <- reqBands(VI) |> eo_mapBand(endpoint, collection, invert=TRUE)
  f_args$asset_names <- c(asset_names,maskOptions$maskLyr)
  if(!is.null(maxCloud)) f_args$property_filter <- function(x) {x[["eo:cloud_cover"]] < maxCloud}
  suppressWarnings(imgCollection <- do.call(gdalcubes::stac_image_collection, f_args))
  
  ##  Masking (if mask layer is provided in "maskOptions" argument)
  if(!is.null(maskOptions$maskLyr)) msk <- do.call(eo_mask.cube, maskOptions) else msk <- NULL
  
  v.ref <- gdalcubes::cube_view(srs=crs(x, proj=TRUE), 
                                extent=list(t0=paste(min(years), "01", "01", sep="-"), t1=paste(max(years), "12", "31", sep="-"),
                                            left= ext(x)$xmin, right= ext(x)$xmax,
                                            top=ext(x)$ymax, bottom=ext(x)$ymin),
                                dx=res(x)[1], dy=res(x)[2], dt="P1Y",
                                aggregation="median", resampling="bilinear")
  composite <- gdalcubes::raster_cube(imgCollection, v.ref, msk) |>
    gdalcubes::select_bands(asset_names) |>
    eo_harm_bandnames.cube(endpoint, collection)

  out <- calc_VI.cube(composite, VI=VI)
  return(out)
  
}


