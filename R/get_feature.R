#' Extract STAC feature for specified spatial reference
#' 
#' Download STAC feature from collection and transform based on provided spatial reference
#' 
#' @export
#' @import terra
#' 
#' @param feature description
#' @param x description
#' @param assets description
#' @param as_list description
#' @param add_datetime description
#' @param ... description
#' 
#' @returns (list of) SpatRaster objects
setGeneric("get_feature", function(feature, x, assets, as_list=FALSE, add_datetime=TRUE, ...){
  standardGeneric("get_feature")}
)

#' @rdname get_feature
#' @import terra
setMethod("get_feature", signature(x="SpatRaster"),
          function(feature, x, assets, as_list=FALSE, add_datetime=TRUE, ...){
            library(terra)
            featBands <- assets2rast(feature, assets, as_list=TRUE)
            stac_x <- lapply(featBands, terra::project, x, ...)
            if(isTRUE(add_datetime)){
              ftime <- as.POSIXlt(feature$properties$datetime, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
              for (l in 1:length(stac_x)) time(stac_x[[l]]) <- rep(ftime, nlyr(stac_x[[l]]))
            }
            if(!isTRUE(as_list)) stac_x <- rast(stac_x)
            return(stac_x)
          }
)

#' @rdname get_feature
#' @import terra
setMethod("get_feature", signature(x="SpatExtent"),
          function(feature, x, assets, as_list=FALSE, add_datetime=TRUE, CRS, snap="near", mask=FALSE, touches=TRUE, extend=FALSE, filename="", ...){
            library(terra)
            
            featBands <- assets2rast(feature, assets, as_list=TRUE)
            
            ##  Project extent to crs of feature
            if(missing(CRS)) CRS <- crs("epsg:4326")
            cropExt <- terra::project(x, CRS, crs(featBands[[1]]))
            
            ##  Adjust crop extent to match asset with coarsest resolution
            coarseAsset <- which.max(sapply(featBands, function(x) res(x)[1]))
            cropExt <- align(cropExt, featBands[[coarseAsset]], snap=snap)
            
            stac_x <- lapply(featBands, crop, cropExt, snap=snap, mask=mask, touches=touches, extend=extend, filename=filename, ...)
            
            if(isTRUE(add_datetime)){
              ftime <- as.POSIXlt(feature$properties$datetime, format="%Y-%m-%dT%H:%M:%S", tz="GMT")
              for (l in 1:length(stac_x)) time(stac_x[[l]]) <- rep(ftime, nlyr(stac_x[[l]]))
            }
            
            if(!isTRUE(as_list)) stac_x <- rast(stac_x)
            return(stac_x)
          }
)

#TODO: setMethod("get_feature", signature(x="SpatVector"), function(){})
#TODO: setMethod("get_feature", signature(x="numeric"), function(){})


#' Extract STAC FeatureCollection for specified spatial reference
#' 
#' Download STAC FeatureCollection from collection and transform based on provided spatial reference
#' 
#' @export
#' @import terra
#' 
#' @param featureCollection STAC FeatureCollection (list)
#' @param x Spatial reference (see get_feature())
#' @param assets character vector of assets names to extract
#' @param progress should progress bar be shown
#' @param ... additional arguments passed to get_feature
#' 
#' @returns list of (list of) SpatRaster objects
get_FeatureCollection <- function(FeatureCollection, x, assets, progress=FALSE, ...){

  #TODO Should this be combined as method in get_feature?

  ##  Check input parameters
  if(missing(FeatureCollection)) stop("FeatureCollection must be provided")
  if(!"list" %in% class(FeatureCollection)) stop("FeatureCollection must be list of type FeatureCollection")
  if(FeatureCollection$type!="FeatureCollection") stop("feature must be list of type FeatureCollection")
  
  nFeat <- length(FeatureCollection$features)
  rastFeatures <- vector(mode='list', length=nFeat)
  if(isTRUE(progress)) pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                                            max = nFeat,  # Maximum value of the progress bar
                                            style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                            width = 50,   # Progress bar width. Defaults to getOption("width")
                                            char = "=")   # Character used to create the bar
  for(f in 1:nFeat){  # Iteratively get the features
    rastFeatures[[f]] <- tryCatch(get_feature(FeatureCollection$features[[f]], x, assets, ...), 
                                  error=function(e) NULL)
    if(progress) setTxtProgressBar(pb, f)
  }
  if(progress) close(pb)
  
  failed <- sapply(rastFeatures, is.null) #check for failed downloads
  if(sum(failed) > 0) {                   #remove from list
    warning(paste0(sum(failed)," failed download(s), feature(s) ignored"))
    rastFeatures <- rastFeatures[!failed]
  }
  return(rastFeatures)
}
