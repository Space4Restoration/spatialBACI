#' Search STAC EO data
#' 
#' Easier searching for EO data in STAC collections
#' 
#' For now only tested/specified for Planetary Computer, LPCLOUD
#' 
#' @import rstac
#' @export
#' 
#' @param endpoint see rstac::search
#' @param ... see rstac::search
#' @param eocc maximum cloud cover (for "eo:cloud_cover" field)
#' @param authOpt list specifying STAC authentication options (e.g. list(key="abc"))
#' 
#' @returns STACItemCollection
#' 
#' 
eo_stac_search <- function(endpoint, collection=NULL, ids=NULL, bbox=NULL, datetime=NULL, intersects=NULL, limit=NULL,
                           eocc=NULL, authOpt=list()){
  
  searchArgs <- list()
  searchArgs$q <- stac(endpoint)
  searchArgs$collection <- collection
  searchArgs$ids <- ids
  searchArgs$bbox <- bbox
  searchArgs$datetime <- datetime
  searchArgs$intersects <- intersects
  
  if(endpoint==as.endpoint("PlanetaryComputer")){
    #Set limit for planetary computer (max=1000)
    searchArgs$limit <- min(limit, 1000)
    if(isTRUE(limit>1000)) warning("Limit set to 1000 (maximum for Planetary Computer)")
  }
  
  query <- do.call(stac_search, searchArgs)
  
  if(endpoint==as.endpoint("PlanetaryComputer")){
    if(!is.null(eocc)) query <- ext_query(query, "eo:cloud_cover" < eocc)
  }
  
  items <- post_request(query)
  
  if(endpoint==as.endpoint("lpdaac")){
    #searchArgs$limit <- NULL
    items <- items_fetch(items)
    if(!is.null(eocc)) items <- items_filter(items, properties$`eo:cloud_cover` < eocc)
  }
  
  if(isTRUE(length(items$features)==limit)) warning(paste0("Limit of ",limit," items reached."))
  
  #Authenticate 
  items <- stac_auth(items, endpoint = endpoint, authOpt=authOpt)
  
  return(items)
}
