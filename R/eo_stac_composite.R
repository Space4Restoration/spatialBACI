#' EO STAC composite
#' 
#' Create EO image composite from STAC collection
#' 
#' Automatically applies masking if masking layer is provided
#' 
#' @export
#' @importFrom gdalcubes stac_image_collection image_mask cube_view raster_cube
#' @importFrom terra rast ext res
#' 
#' @param endpoint STAC endpoint
#' @param collection STAC collection
#' @param spatRef SpatRaster object from which spatial reference is extracted
#' @param t0 composite start time, character or Date formatted as "yyyy-mm-dd",  or numeric formatted as yyyymmdd
#' @param t1 composite end time, character or Date formatted as "yyyy-mm-dd", or numeric formatted as yyyymmdd
#' @param limit STAC item limit
#' @param maxcloud maximum cloud cover
#' @param assets STAC assets to be selected
#' @param maskSnow should snow be masked if mask layer is available
#' @param maskWater should water be masked if mask layer is available
#' @param aggregation aggregation method, see \code{gdalcubes::cubes_view}
#' @param resampling resampling method, see \code{gdalcubes::cubes_view}
#' 
#' @returns proxy data cube object
#' 
eo_stac_composite <- function(spatRef, t0, t1, dt, endpoint, collection, 
                              limit=1000, maxCloud=100,
                              assets=eo_assets(collection=collection),
                              maskSnow=FALSE, maskWater=FALSE,
                              aggregation="median", resampling="bilinear",
                              authOpt=list()){
  
  if(is.numeric(t0)) t0 <- as.character(t0) |> as.Date(format="%Y%m%d")
  if(is.numeric(t1)) t1 <- as.character(t1) |> as.Date(format="%Y%m%d")
  
  items <- eo_stac_search(endpoint=endpoint,
                          collection=collection,
                          bbox=as.bbox(spatRef),
                          datetime=paste(t0,t1, sep="/"),
                          limit=limit,
                          authOpt=authOpt)
  
  suppressWarnings(img_collection <- gdalcubes::stac_image_collection(items$features, 
                                                                      asset_names = assets, 
                                                                      property_filter = function(x) {x[["eo:cloud_cover"]] < maxCloud}))
  maskNames  <- c("qa_pixel", "SCL")
  if(any(maskNames %in% assets)){
    if("qa_pixel" %in% assets){
      #Landsat QA bitmask
      maskLyr <- "qa_pixel"
      bits <- 0:7
      mskValues <- bitmask_values(8, snow=maskSnow, water=maskWater)
    } else if ("SCL" %in% assets){
      #Sentinel-2 Scene Classification
      maskLyr <- "SCL"
      bits <- NULL
      mskValues <- c(0,1,2,3,8,9,10)
      if(maskWater) mskValues <- c(mskValues,6)
      if(maskSnow) mskValues <- c(mskValues,11)
    }
    msk <- gdalcubes::image_mask(maskLyr, bits=bits, values=mskValues)
    assets <- assets[! assets %in% maskNames]
  } else {
    #no mask layer detected
    msk <- NULL
  }
  if(missing(dt)) dt <- paste0("P",as.numeric(as.Date(t1)-as.Date(t0))+1, "D")
  
  v.ref <- cube_view(srs=crs(spatRef), 
                     extent=list(t0=t0, t1=t1,
                                 left= ext(spatRef)$xmin, right= ext(spatRef)$xmax,
                                 top=ext(spatRef)$ymax, bottom=ext(spatRef)$ymin),
                     dx=res(spatRef)[1], dy=res(spatRef)[1], dt=dt,
                     aggregation=aggregation, resampling=resampling)
  
  composite <- raster_cube(img_collection, v.ref, msk) |>
    select_bands(assets)

  return(composite)
}


#' Yearly STAC composite
#' 
#' Create yearly composite images from STAC collection
#' 
#' If the \code{months} argument is provided, the lowest and highest number if used to define the compositing period.
#' 
#' @export
#' @importFrom gdalcubes stac_image_collection image_mask cube_view raster_cube
#' @importFrom terra rast ext res
#' 
#' @param endpoint STAC endpoint
#' @param collection STAC collection
#' @param spatRef SpatRaster object from which spatial reference is extracted
#' @param years years for which a composite is created
#' @param months months that are used for creating composite for each year
#' @param limit STAC item limit
#' @param maxcloud maximum cloud cover
#' @param assets STAC assets to be selected
#' @param maskSnow should snow be masked if mask layer is available
#' @param maskWater should water be masked if mask layer is available
#' @param aggregation aggregation method, see \code{gdalcubes::cubes_view}
#' @param resampling resampling method, see \code{gdalcubes::cubes_view}
#' 
#' @returns proxy data cube object
#' 
eo_stac_yearly_composites <- function(spatRef, years, months,
                                      endpoint, collection, 
                                      limit=1000, maxCloud=100,
                                      assets=eo_assets(collection=collection),
                                      maskSnow=FALSE, maskWater=FALSE,
                                      aggregation="median", resampling="bilinear",
                                      authOpt=list()){

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
    items_year <- eo_stac_search(endpoint=endpoint,
                                 collection=collection,
                                 bbox=as.bbox(spatRef),
                                 datetime=paste(t0,t1, sep="/"),
                                 limit=limit, 
                                 authOpt=authOpt)
    if(is.null(items)){
      items <- items_year
    } else {
      items$features <- c(items$features, items_year$features)
    }
  }

  suppressWarnings(img_collection <- gdalcubes::stac_image_collection(items$features, 
                                                                      asset_names = assets, 
                                                                      property_filter = function(x) {x[["eo:cloud_cover"]] < maxCloud}))
  maskNames  <- c("qa_pixel", "SCL")
  if(any(maskNames %in% assets)){
    if("qa_pixel" %in% assets){
      #Landsat QA bitmask
      maskLyr <- "qa_pixel"
      bits <- 0:7
      mskValues <- bitmask_values(8, snow=maskSnow, water=maskWater)
    } else if ("SCL" %in% assets){
      #Sentinel-2 Scene Classification
      maskLyr <- "SCL"
      bits <- NULL
      mskValues <- c(0,1,2,3,8,9,10)
      if(maskWater) mskValues <- c(mskValues,6)
      if(maskSnow) mskValues <- c(mskValues,11)
    }
    msk <- gdalcubes::image_mask(maskLyr, bits=bits, values=mskValues)
    assets <- assets[! assets %in% maskNames]
  } else {
    #no mask layer detected
    msk <- NULL
  }

  v.ref <- cube_view(srs=crs(spatRef), 
                     extent=list(t0=paste(min(years), "01", "01", sep="-"), t1=paste(max(years), "12", "31", sep="-"),
                                 left= ext(spatRef)$xmin, right= ext(spatRef)$xmax,
                                 top=ext(spatRef)$ymax, bottom=ext(spatRef)$ymin),
                     dx=res(spatRef)[1], dy=res(spatRef)[1], dt="P1Y",
                     aggregation=aggregation, resampling=resampling)
  
  composite <- raster_cube(img_collection, v.ref, msk) |>
    select_bands(assets)
  
  return(composite)
}


