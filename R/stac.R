##  Author: Jasper Van doninck
##  Date: July 2023
##  


stac_endpoint <- function(name){
  #Some endpoints of STAC APIs because I'm too lazy to remember/search
  #Defaults to Planetary Computer
  if(missing(name)) {
    endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  } else {
    endpoint <- switch(tolower(name),
                       "lpdaac" = "https://cmr.earthdata.nasa.gov/stac/LPCLOUD",
                       "planetarycomputer" = "https://planetarycomputer.microsoft.com/api/stac/v1",
                       "planetary computer" = "https://planetarycomputer.microsoft.com/api/stac/v1",
                       stop("Endpoint not implemented")
    )
  }
  return(endpoint)
}


.stac_auth.planetarycomputer <- function(x, key=""){
  library(httr)
  library(rstac)
  
  endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  
  #Determine collection
  if(x$type=="Feature"){
    collection <- x$collection
  } else if (x$type=="FeatureCollection"){
    collection <- sapply(x$features, function(z){z$collection}) %>%
      as.factor() %>%
      levels()
    if(length(collection)>1) stop('Multiple collection IDs in FeatureCollection')
  }
  
  #Get PC token
  if(isTRUE(nchar(key)>0)) keystr <- paste0('?subscription-key=', key) else keystr <- ''
  tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/',collection,keystr))
  token <- httr::content(tok_ret)$token
  
  signFeature <- function(feat){
    assets <- feat$assets
    assets_signed <- lapply(assets, function(x){
      href <- x$href
      href_signed <-  paste0(href,'?',token)
      x$href <- href_signed
      return(x)
    })
    feat$assets <- assets_signed
    return(feat)
  }
  if(x$type=="Feature") x <- signFeature(x) else x$features <- lapply(x$features, signFeature)
  
  return(x)
}

stac_auth <- function(x, endpoint, ...){
  if(endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
    x <- do.call(.stac_auth.planetarycomputer, 
                 c(list(x=x), key=list(...)$key))
  } else {
    #Signing for other endpoints not implemented
  }
  return(x)
}

assets2rast <- function(feature, assets, as_list=FALSE){
  #   Read STAC assets as spatRaster
  #
  #   Description
  #    Reads selected assets from a STAC features as a spatRaster object
  #
  #   Usage
  #     assets2rast(feature, assets)
  #
  #   Arguments
  #     feature       a STAC feature: list element of STACItemCollection$features
  #     assets        character vector of assets names to extract
  #     as_list       boolean defining whether assets should be read as list of spatRasters or as multilayer spatRaster 
  #     sign_feature  function used to sign feature (e.g., sign_feature_pc), or NULL
  #     ...           Additional arguments to function in sign_feature
  #   
  #   Details
  #     Assets to read must be of same extent and dimensions if read into mulitlayer SpatRaster.
  #     Set 'as_list=TRUE' to deal with assets with different extents or dimensions
  #
  #   Value
  #     a (list of) spatRaster object(s)
  
  library(terra)
  fAssets <- feature$assets[which(names(feature$assets) %in% assets)]
  fURLs <- lapply(fAssets, function(x){x$href})
  #read all assets into list, remove possible scale/offset
  ras <- lapply(fURLs, function(x){r <- rast(x, vsi=TRUE); scoff(r) <- NULL; return(r)})
  ras <- ras[assets]
  
  #convert list to multilayers SpatRaster
  if(!isTRUE(as_list)) ras <- rast(ras)
  return(ras)
}

assets2vrt <- function(featureCollection, assets, as_list=FALSE){
  
  library(terra)

  aFun <- function(asset){
    URLs <- sapply(featureCollection$features, function(x){x$assets[[asset]]$href})
    URLs <- paste0("/vsicurl/", URLs)
    asset_vrt <- vrt(URLs)
    names(asset_vrt) <- asset
    scoff(asset_vrt) <- NULL
    return(asset_vrt)
  }
  ras <- lapply(assets,aFun)
  
  if(!isTRUE(as_list)) ras <- rast(ras)
  
  return(ras)
}

setGeneric("get_feature", function(feature, x, assets, as_list=FALSE, add_datetime=TRUE, ...) standardGeneric("get_feature"))

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

get_FeatureCollection <- function(FeatureCollection, x, assets, progress=FALSE, ...){
  # Description
  #   Read STAC FeatureCollection as list a list of SpatRaster
  # Arguments
  #   featureCollection STAC FeatureCollection (list)
  #   x                 Spatial reference (see get_feature())
  #   assets            character vector of assets names to extract
  #   ...               additional arguments passed to get_feature
  # Value
  #   list of (list of) SpatRaster objects

  library(terra)
  #library(httr)
  
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












##  Deprecated:
###############

sign_feature <- function(feature, endpoint, key='', usr=NULL, pwd=NULL){
  #Deprecated, use stac_auth
  library(httr)
  if(missing(endpoint)){
    warning("no endpoint provided")
    return(feature)
  }
  
  if(endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
    #Sign assets href for Planetary Computer
    if(isTRUE(nchar(key)>0)) {
      tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/',feature$collection,
                                  '?subscription-key=', key))
    } else {
      tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/',feature$collection))
    }
    token <- httr::content(tok_ret)$token
    
    feature_assets <- feature$assets
    feature_assets_signed <- lapply(feature_assets, function(x){
      href <- x$href
      href_signed <-  paste0(href,'?',token)
      x$href <- href_signed
      return(x)
    })
    feature$assets <- feature_assets_signed
  } else {
    #Signing for other endpoints not implemented
  }
  return(feature)
  
}

sign_featureCollection <- function(featureCollection, endpoint, key='', usr=NULL, pwd=NULL){
  #Deprecated, use stac_auth
  library(httr)
  if(missing(endpoint)){
    warning("no endpoint provided")
    return(featureCollection)
  }
  
  if(endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
    
    collection <- sapply(featureCollection$features, function(x){x$collection}) %>%
      as.factor() %>%
      levels()
    if(length(collection)>1) stop('Multiple collection IDs in item')
    if(nchar(key)==0) {
      tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/', collection)) %>%         
        httr::content() 
    } else {
      tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/', collection,
                                  '?subscription-key=', key)) %>%         
        httr::content() 
    }
    token <- tok_ret$token
    
    feats <- featureCollection$features
    signFeature <- function(feat){
      assets <- feat$assets
      assets_signed <- lapply(assets, function(x){
        href <- x$href
        href_signed <-  paste0(href,'?',token)
        x$href <- href_signed
        return(x)
      })
      feat$assets <- assets_signed
      return(feat)
    }
    feats_signed <- lapply(feats, signFeature)
    featureCollection$features <- feats_signed
  } else {
    #not implemented
  }
  
  return(featureCollection)
}




cropSTACFeature <- function(feature, 
                            assets,
                            bbox,
                            extend=TRUE,
                            commonRes="high",
                            as_list=FALSE,
                            ...
){
  
  #   Crop SpatRaster from STAC feature
  #
  #   Description
  #    Extract spatial subset (crop) from a STAC feature assets
  #
  #   Usage
  #     cropSTACFeature(feature, assets=NULL, bbox, collection, commonRes="low")
  #
  #   Arguments
  #     feature     a STAC feature: list element of STACItemCollection$features
  #     assets      character vector of assets names to extract
  #     bbox        numeric vector representing bounding box (xmin, ymin, xmax, ymax) in geographic coordinates
  #     extend      boolean. Should the cropped area be extended if the bounding box goes beyond the STAC asset's geometry    
  #     commonRes   numeric or character. Defines the common resolution in case speciefied assets have multiple resolutions and as.list=FALSE. 
  #                   numeric: spatial resolution in x and y, in units of feature
  #                   character: one of c("low", "high"); "low" aggregating to coarsest resolution, "high" disaggregating to finest resolution 
  #     as_list     boolean. If multiple assets specified should they be returned as a list of SpatRaster instead of a multilayer SpatRaster
  #     ...         Additional arguments passed to assets2rast (for signing feature)
  #
  #   Details
  #     Warning! If spatial resolutions of assets are not all multiples of each others, returning as multilayer spatRaster will result in error
  #
  #   Value
  #     a spatRaster object
  #
  #   To do
  #     handling error when no overlap between assets and bbox
  #
  
  library(terra)
  library(magrittr)
  terraOptions(progress=0)
  
  ##  Check input parameters, define default values
  # feature
  if(missing(feature)) stop("feature must be provided")
  # if(!"list" %in% class(feature)) stop("feature must be list of type Feature")
  # if(feature$type!="Feature") stop("feature must be list of type Feature")
  #assets
  #If argument assets is missing, use all assets of type "image/tiff; application=geotiff; profile=cloud-optimized"
  if(missing(assets)){
    warning("assets not provided, all available assets selected")
    assets <- names(feature$assets)[lapply(feature$assets, function(x) x$type)=="image/tiff; application=geotiff; profile=cloud-optimized"]
  }
  #bbox
  if(missing(bbox)) bbox <- feature$bbox
  if(class(bbox)=="SpatExtent") bbox <- bbox[c(1,3,2,4)]  #Check if bbox is already a SpatExtent object
  
  ##  Read assets as spatRaster object
  featBands <- assets2rast(feature, assets, as_list=TRUE, ...)
  
  ##  Reproject bounding box to crs of feature
  cropExt <- terra::project(ext(bbox, xy=TRUE), crs("+proj=longlat +datum=WGS84"), crs(featBands[[1]]))
  
  ##  Adjust crop extent to match asset with coarsest resolution
  coarseRast <- sapply(featBands, function(x) res(x)[1]) %>% 
    which.max()
  cropExt <- align(cropExt, featBands[[coarseRast]], snap="out")
  
  ##  Crop assets
  cropFun <- function(b, cropExt, extend){
    #TO DO: Add extra check if extents of b and cropExt overlap # If no overlap, this will throw an error in the current version of terra. This should be replaced by a warning and returning an empty raster (with correct properties) 
    cr <- crop(b,cropExt, snap="near")
    if(isTRUE(extend)) cr <- extend(cr, cropExt) #Crop with extend=TRUE is bugged in older versions of terra, so using this approach for now. Should be updated
    
    #set "time" field
    if(!is.null(feature$properties$datetime)){
      time(cr) <- rep(as.POSIXlt(feature$properties$datetime, format="%Y-%m-%dT%H:%M:%S", tz="GMT"), nlyr(cr))
    }
    return(cr)
  }
  croppedBands <- lapply(featBands, cropFun, cropExt, extend)
  
  if(!isTRUE(as_list)){
    # Check spatial resolutions of assets
    resolutions <- sapply(featBands, function(x) res(x)[1])
    resLevels <- as.numeric(levels(as.factor(resolutions)))
    if(length(resLevels)>1){
      #Assets with different resolution: harmonize assets to common resolution (assuming multiples)
      # Target resolution: either specified or highest/lowest
      if(is.numeric(commonRes)){
        targetRes <- commonRes
      } else {
        targetRes <- switch(commonRes,
                            low=max(resLevels),
                            high=min(resLevels))
      }
      fun <- function(x){
        if(res(x)[1]==targetRes){
          return(x)
        } else if(res(x)[1]>targetRes){
          #disaggregate
          return(disagg(x, fact=res(x)[1]/targetRes, method="near"))
        } else if(res(x)[1]<targetRes){
          #aggregate
          return(aggregate(x, fact=targetRes/res(x)[1], fun="mean"))
        }
      }
      croppedBands <- lapply(croppedBands, fun)
    }
    
    #Extents may differ in case of different resolutions, add check to avoid errors  
    if(length(croppedBands)>1){
      if(!isTRUE(do.call(compareGeom, c(unname(croppedBands),stopOnError=FALSE)))){
        croppedBands <- lapply(croppedBands, crop, cropExt)
      } 
    }
    croppedBands <- rast(croppedBands)
  }
  return(croppedBands)
}

cropSTACFeatureCollection <- function(FeatureCollection,
                                      assets,
                                      bbox,
                                      progress=TRUE,
                                      sign_featureCollection=NULL,
                                      key='',
                                      ...){
  # Read STAC Feature Collection
  #
  # Description
  #   Read STAC FeatureCollection as list STAC features
  #
  # Arguments
  #   stacFeatures    list. STAC FeatureCollection
  #   assets      character vector of assets names to extract
  #   bbox        numeric vector representing bounding box (xmin, ymin, xmax, ymax) in geographic coordinates
  #   
  #   ...             additional arguments passed to cropSTACFeature (assets, bbox, extend, as.list, commonRes)
  #
  # Value
  #   list of SpatRaster objects
  #
  
  library(terra)
  library(httr)
  
  ##  Check input parameters
  if(missing(FeatureCollection)) stop("FeatureCollection must be provided")
  if(!"list" %in% class(FeatureCollection)) stop("FeatureCollection must be list of type FeatureCollection")
  if(FeatureCollection$type!="FeatureCollection") stop("feature must be list of type FeatureCollection")
  
  if(!is.null(sign_featureCollection)) FeatureCollection <- sign_featureCollection(FeatureCollection, key)
  
  nFeat <- length(FeatureCollection$features)
  rastFeatures <- vector(mode='list', length=nFeat)
  if(progress) pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                                    max = nFeat,  # Maximum value of the progress bar
                                    style = 3,    # Progress bar style (also available style = 1 and style = 2)
                                    width = 50,   # Progress bar width. Defaults to getOption("width")
                                    char = "=")   # Character used to create the bar
  for(f in 1:nFeat){  # ITeratively get the features
    rastFeatures[[f]] <- tryCatch(cropSTACFeature(FeatureCollection$features[[f]], assets, bbox, ...), 
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

sign_feature_pc <- function(feature, key=''){
  #DEPRECATED, USE sign_feature()
  #   Sign a Microsoft Planetary Computer STAC feature to enable download
  
  library(httr)
  
  if(nchar(key)==0) {
    tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/', feature$collection))
  } else {
    tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/', feature$collection,
                                '?subscription-key=', key))
  }
  token <- httr::content(tok_ret)$token
  
  feature_assets <- feature$assets
  feature_assets_signed <- lapply(feature_assets, function(x){
    href <- x$href
    href_signed <-  paste0(href,'?',token)
    x$href <- href_signed
    return(x)
  })
  feature$assets <- feature_assets_signed
  return(feature)
}

sign_featureCollection_pc  <- function(item, key=''){
  ##DEPRECATED, USE sign_featureCollection()
  
  #   Sign STACItemCollection 
  #
  #   Description
  #     Alternative method for singing a STACItemCollection in Planetary Computer
  #     Can be used if the functions 'items_sign' and 'sign_planetary_computer' from the 'rstac' package throw an error
  #
  #   Usage
  #     sign_backup(item, key=NULL)
  #
  #   Arguments
  #     item        a STACItemCollection object representing the results of /stac/search
  #     key         a character representing a personal Planetary Computer key. Not required
  #
  #   Value
  #     a STACItemCollection with downloadable urls
  library(httr)
  
  collection <- sapply(item$features, function(x){x$collection}) %>%
    as.factor() %>%
    levels()
  if(length(collection)>1) stop('Multiple collection IDs in item')
  if(nchar(key)==0) {
    tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/', collection)) %>%         
      httr::content() 
  } else {
    tok_ret <- httr::GET(paste0('https://planetarycomputer.microsoft.com/api/sas/v1/token/', collection,
                                '?subscription-key=', key)) %>%         
      httr::content() 
  }
  token <- tok_ret$token
  
  feats <- item$features
  signFeature <- function(feat){
    assets <- feat$assets
    assets_signed <- lapply(assets, function(x){
      href <- x$href
      href_signed <-  paste0(href,'?',token)
      x$href <- href_signed
      return(x)
    })
    feat$assets <- assets_signed
    return(feat)
  }
  feats_signed <- lapply(feats, signFeature)
  item$features <- feats_signed
  return(item)
}

