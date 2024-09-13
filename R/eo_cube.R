#' EO cube from STAC
#' 
#' Create a data cube for EO images retrieved from a STAC catalog.
#' 
#' t0 and t1 can be provided as numeric in format yyyymmdd, as a character in the format "yyyymmdd" or "yyyy-mm-dd", or as a Date object. 
#' For now, only date is implemented, not time.
#' 
#' @export
#' @importFrom gdalcubes stac_image_collection cube_view raster_cube select_bands
#' 
#' @param x spatRaster. Defines spatial reference of the output data cube
#' @param endpoint character. STAC endpoint
#' @param collection character. STAC collection
#' @param stacOptions list. Optional arguments to be passed to eo_stac_search, e.g. authentication options
#' @param t0 start date as numeric, character or Date
#' @param t1 end date as numeric, character or Date
#' @param dt size of pixels in time-direction, expressed as ISO8601 period string
#' @param asset_names character vector of asset (band) names to be used, or NULL for all asset names with "eo:bands" attributes
#' @param joint_assets logical indicating if only the assets (bands) present in all features should be used, if argument asset_names is NULL
#' @param aggregations see gdalcubes::cube_view
#' @param resampling see gdalcubes::cube_view
#' @param maxCloud maximum cloud cover [0-100] of images to be included in data cube, only relevant for collections with "eo:cloud_cover" attributes 
#' @param maskOptions list specifying layer used to create mask and masking options (see eo_mask.cube)
#' @param harm_bandnames logical indicating whether endpoint/collection-specific band names should be changed to generic band names
#' 
#' @returns proxy data cube object

eo_cube.stac <- function(x,
                         endpoint, collection, stacOptions=list(),
                         t0, t1, dt="P1D",
                         asset_names=NULL, joint_assets=TRUE,
                         maxCloud=NULL,
                         maskOptions=list(NULL),
                         aggregation="first", resampling="bilinear",
                         harm_bandnames=TRUE){
  
  ##  Format dates
  formatDate <- function(d){
    switch(class(d),
           Date = as.character(d),
           numeric = as.character(d) |> as.Date(format="%Y%m%d") |> as.character(),
           character = if(nchar(d)==8) as.character(as.Date(d, format="%Y%m%d")) else d
    )}
  t0 <- formatDate(t0)
  t1 <- formatDate(t1)
  
  ##  Create STAC ItemCollection
  s_args <- list(endpoint=endpoint, collection=collection, bbox=as.bbox(x), datetime=paste(t0,t1, sep="/"))
  s_args <- c(s_args, stacOptions)
  itemCollection <- do.call(eo_stac_search, s_args)
  
  ##  Create gdalcubes image collection from STAC ItemCollection
  f_args <- list(s=itemCollection$features)
  if(is.null(asset_names)){
    all_asset_names <- lapply(f_args$s, function(s){
      names(s$assets)[sapply(s$assets, function(a){
        "eo:bands" %in% names(a)})]
    })
    if(isTRUE(joint_assets)){
      tab <- table(unlist(all_asset_names))
      tab <- tab[unlist(all_asset_names)|> unique()]
      asset_names <- names(tab[tab==length(all_asset_names)])
    } else {
      asset_names <- unlist(all_asset_names)|> unique()
    }
  }
  f_args$asset_names <- c(asset_names,maskOptions$maskLyr)
  if(!is.null(maxCloud)) f_args$property_filter <- function(x) {x[["eo:cloud_cover"]] < maxCloud}
  suppressWarnings(imgCollection <- do.call(gdalcubes::stac_image_collection, f_args))
  
  ##  Masking (if mask layer is provided in "maskOptions" argument)
  if(!is.null(maskOptions$maskLyr)) msk <- do.call(eo_mask.cube, maskOptions) else msk <- NULL
  
  ##  Define Cube View
  v.ref <- gdalcubes::cube_view(srs=crs(x, proj=TRUE), 
                                extent=list(t0=t0, t1=t1,
                                            left= ext(x)$xmin, right= ext(x)$xmax,
                                            top=ext(x)$ymax, bottom=ext(x)$ymin),
                                dx=res(x)[1], dy=res(x)[2], dt=dt,
                                aggregation=aggregation, resampling=resampling)
  
  imgCube <- gdalcubes::raster_cube(imgCollection, v.ref, msk)
  imgCube <- gdalcubes::select_bands(imgCube, asset_names)
  
  if(isTRUE(harm_bandnames)) imgCube <- eo_harm_bandnames.cube(imgCube, endpoint, collection)
  
  return(imgCube)
}
  

#' Yearly EO cube from STAC
#' 
#' Create a data cube with yearly time intervals for EO images retrieved from a STAC catalog.
#' 
#' This function bypasses limits on the length of the queried STAC itemCollection by posting subsequent queries for each year
#' 
#' @export
#' @importFrom gdalcubes stac_image_collection cube_view raster_cube select_bands
#' 
#' @param x spatRaster. Defines spatial reference of the output data cube
#' @param endpoint character. STAC endpoint
#' @param collection character. STAC collection
#' @param stacOptions list. Optional arguments to be passed to eo_stac_search, e.g. authentication options
#' @param years description
#' @param months description
#' @param asset_names character vector of asset (band) names to be used, or NULL for all asset names with "eo:bands" attributes
#' @param joint_assets logical indicating if only the assets (bands) present in all features should be used, if argument asset_names is NULL
#' @param maxCloud numeric. Maximum cloud cover [0-100] of images to be included in data cube, only relevant for collections with "eo:cloud_cover" attributes
#' @param maskOptions list specifying layer used to create mask and masking options (see eo_mask.cube)
#' @param aggregation see gdalcubes::cube_view
#' @param resampling see gdalcubes::cube_view
#' @param harm_bandnames logical indicating whether endpoint/collection-specific band names should be changed to generic band names
#' 
#' @returns proxy data cube object
#' 
eo_cube_yearly.stac <- function(x,
                                endpoint, collection, stacOptions=list(),
                                years,
                                months,
                                asset_names=NULL, joint_assets=TRUE,
                                maxCloud=NULL,
                                maskOptions=list(NULL),
                                aggregation="median", resampling="bilinear",
                                harm_bandnames=TRUE){
  
  itemCollection <- NULL
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
    
    ##  Create STAC ItemCollection
    s_args <- list(endpoint=endpoint, collection=collection, bbox=as.bbox(x), datetime=paste(t0,t1, sep="/"))
    s_args <- c(s_args, stacOptions)  ##!!! add maxCloud to stacOptions? 
    itemCollection_year <- do.call(eo_stac_search, s_args)
    
    if(is.null(itemCollection)){
      itemCollection <- itemCollection_year
    } else {
      itemCollection$features <- c(itemCollection$features, itemCollection_year$features)
    }
  }
  
  ##  Create gdalcubes image collection from STAC ItemCollection
  f_args <- list(s=itemCollection$features)
  if(is.null(asset_names)){
    all_asset_names <- lapply(f_args$s, function(s){
      names(s$assets)[sapply(s$assets, function(a){
        "eo:bands" %in% names(a)})]
    })
    if(isTRUE(joint_assets)){
      tab <- table(unlist(all_asset_names))
      tab <- tab[unlist(all_asset_names)|> unique()]
      asset_names <- names(tab[tab==length(all_asset_names)])
    } else {
      asset_names <- unlist(all_asset_names)|> unique()
    }
  }
  f_args$asset_names <- c(asset_names,maskOptions$maskLyr)
  if(!is.null(maxCloud)) f_args$property_filter <- function(x) {x[["eo:cloud_cover"]] < maxCloud}
  suppressWarnings(imgCollection <- do.call(gdalcubes::stac_image_collection, f_args))
  
  ##  Masking (if mask layer is provided in "maskOptions" argument)
  if(!is.null(maskOptions$maskLyr)) msk <- do.call(eo_mask.cube, maskOptions) else msk <- NULL
  
  ##  Define Cube View
  v.ref <- gdalcubes::cube_view(srs=crs(x, proj=TRUE), 
                                extent=list(t0=paste(min(years), "01", "01", sep="-"), t1=paste(max(years), "12", "31", sep="-"),
                                            left= ext(x)$xmin, right= ext(x)$xmax,
                                            top=ext(x)$ymax, bottom=ext(x)$ymin),
                                dx=res(x)[1], dy=res(x)[2], dt="P1Y",
                                aggregation=aggregation, resampling=resampling)
  
  imgCube <- gdalcubes::raster_cube(imgCollection, v.ref, msk)
  imgCube <- gdalcubes::select_bands(imgCube, asset_names)
  
  if(isTRUE(harm_bandnames)) imgCube <- eo_harm_bandnames.cube(imgCube, endpoint, collection)
  
  return(imgCube)
}



