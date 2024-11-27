#' STAC authentication
#' 
#' Authenticate STAC items for specified STAC endpoint
#' 
#' Implemented endpoints: 
#' * Microsoft Planetary Computer
#' 
#' @export
#' @import rstac
#' 
#' @param x a STAC Feature or FeatureCollection
#' @param endpoint a STAC endpoint
#' @param authOpt additional arguments for authentication options specific to STAC endpoint (e.g., list(key="abc"))
#' 
#' @returns description
#' 
stac_auth <- function(x, endpoint, authOpt=list()){
  if(endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
    x.out <- items_sign_planetary_computer(items=x, subscription_key=authOpt$key)
  } else {
    #Signing for other endpoints not implemented
  }
  return(x.out)
}


#deprecated, replaced by rstac::items_sign_planetary_computer
stac_auth.planetarycomputer <- function(x, key=""){

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

