#' Mask layer name
#' 
#' Function that defines metadata layer name from which mask can be derived for a collection at a STAC endpoint
#' 
#' Currently only implemented for collections "landsat-c2-l2" and "sentinel-2-l2a" at Planetary Computer. Returns NULL for all other values 
#' 
#' @param endpoint STAC endpoint
#' @param collection STAC collection
#' 
#' @returns character or NULL
#' 

maskLyrName.stac <- function(endpoint, collection){
  metaLyr <- NULL
  if(endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
    if(collection=="landsat-c2-l2") metaLyr <- "qa_pixel"
    if(collection=="sentinel-2-l2a") metaLyr <- "SCL"
  }
  return(metaLyr)
}


#' Image composites from STAC
#' 
#' Create EO image composite from STAC collection.
#' 
#' Compositing period can be specified as a two-element vector t0-t1 pair (see eo_cube), as a list of t0-t1 pairs, or a list with named elements "years" and, optionally, "months".
#' If a list of t0-t1 pairs is provided, the function will return a list of proxy data cube objects, where each list element is the image composite for the corresponding t0-t1 pair.
#' If a named list with element "years" is provided, a composite image for each year provided in this list argument is created; the compositing period within each year can be specified by including the named list element "months", in which case the provided months will be used to refine the compositing period. 
#' 
#' @export
#' 
#' @param x spatRaster. Defines spatial reference of the output data cube
#' @param endpoint character. STAC endpoint
#' @param collection character. STAC collection
#' @param stacOptions list. Optional arguments to be passed to eo_stac_search, e.g. authentication options
#' @param compositing_period compositing period. See Details
#' @param method compositing method, see cube_composite
#' @param asset_names character vector of asset (band) names to be used, or NULL for all asset names with "eo:bands" attributes
#' @param joint_assets logical indicating if only the assets (bands) present in all features should be used, if argument asset_names is NULL
#' @param maxCloud maximum cloud cover [0-100] of images to be included in data cube, only relevant for collections with "eo:cloud_cover" attributes 
#' @param maskOptions list specifying layer used to create mask and masking options (see eo_mask.cube)
#' @param ... additional arguments passed to to eo_cube
#' 
#' @returns (list of) proxy data cube object
#' 
eo_composites.stac <- function(x, 
                               endpoint, collection, stacOptions=list(limit=1000, authOpt=list()),
                               compositing_period, 
                               method="median",
                               asset_names=NULL, joint_assets=TRUE,
                               maxCloud=NULL,
                               maskOptions=list(maskLyr=maskLyrName.stac(endpoint, collection), maskSnow=FALSE, maskWater=FALSE),
                               ...){
  
  #If compositing is provided as $years/$months, convert to list of t0-t1-pairs; if only a single t0-t1 pair, convert to 1-element list
  if(is.list(compositing_period)){
    if(!is.null(compositing_period$years)){
      if(is.null(compositing_period$months)){
        compositing_period <- lapply(compositing_period$years, function(year){
          t0 <- as.Date(paste(year, "01", "01", sep="-"))
          t1 <- as.Date(paste(year, "12", "31", sep="-"))
          return(c(t0, t1))
        })
      } else {
        compositing_period <- lapply(compositing_period$years, function(year){
          t0 <- as.Date(paste(year, min(compositing_period$months), "01", sep="-"))
          if(max(compositing_period$months)==12){
            t1 <- as.Date(paste(year, max(compositing_period$months), "31", sep="-"), format="%Y-%m-%d") 
          } else{
            t1 <- as.Date(paste(year, max(compositing_period$months)+1, "01", sep="-"), format="%Y-%m-%d")-1
          }
          return(c(t0,t1))
        })
      }
    } 
  } else {
    compositing_period <- list(compositing_period)
  }
  
  #create multitemporal data cube for each compositing period
  cubes <- lapply(compositing_period, function(period){
    period_cube <- eo_cube.stac(x,
                                endpoint=endpoint, collection=collection, stacOptions=stacOptions,
                                t0=period[1], t1=period[2], dt="P1D",
                                asset_names=asset_names, joint_assets=joint_assets,
                                maxCloud=maxCloud,
                                maskOptions=maskOptions,
                                ...)
    return(period_cube)
  })
  
  #Create composite from each multitemporal data cube
  cubes <- lapply(cubes, cube_composite, method=method)
  
  return(cubes)
}
