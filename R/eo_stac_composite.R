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
eo_stac_composite <- function(spatRef, t0, t1, endpoint, collection, 
                              limit=1000, maxCloud=100,
                              assets=eo_assets(collection=collection),
                              maskSnow=FALSE, maskWater=FALSE,
                              aggregation="median", resampling="bilinear"){
  
  if(is.numeric(t0)) t0 <- as.character(t0) |> as.Date(format="%Y%m%d")
  if(is.numeric(t1)) t1 <- as.character(t1) |> as.Date(format="%Y%m%d")
  
  items <- eo_stac_search(endpoint=endpoint,
                          collection=collection,
                          bbox=as.bbox(spatRef),
                          datetime=paste(t0,t1, sep="/"),
                          limit=limit)
  
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
                     extent=list(t0=t0, t1=t1,
                                 left= ext(spatRef)$xmin, right= ext(spatRef)$xmax,
                                 top=ext(spatRef)$ymax, bottom=ext(spatRef)$ymin),
                     dx=res(spatRef)[1], dy=res(spatRef)[1], dt=paste0("P",as.numeric(as.Date(t1)-as.Date(t0))+1, "D"),
                     aggregation=aggregation, resampling=resampling)
  
  composite <- raster_cube(img_collection, v.ref, msk) |>
    select_bands(assets)

  return(composite)
}
