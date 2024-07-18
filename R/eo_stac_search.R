#' Search STAC EO data
#' 
#' Easier searching for EO data in STAC collections
#' 
#' For now only implemented for Planetary Computer
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
eo_stac_search <- function(endpoint, ...){
  if(endpoint==as.endpoint("PlanetaryComputer")){
    return(eo_stac_search.PlanetaryComputer(...))
  } else {
    stop("eo_stac_search only omplemented for Planetary Computer")
  }
}



#' Search STAC EO data for Planetary Computer
#' 
#' 
#' @import rstac
#' @export
#' 
#' @param collection STAC collection
#' @param ids description
#' @param bbox description
#' @param datetime description
#' @param intersects description
#' @param limit description
#' @param eocc maximum cloud cover (for "eo:cloud_cover" field)
#' @param authOpt list specifying STAC authentication options (e.g. list(key="abc"))
#' 
#' @returns STACItemCollection
#' 
#' 

eo_stac_search.PlanetaryComputer <- function(collection=NULL, ids=NULL, bbox=NULL, datetime=NULL, intersects=NULL, limit=NULL,
                                             eocc=NULL, sat.orbit.state=NULL, sat.orbit=NULL, authOpt=list()){
    
  endpoint <- as.endpoint("PlanetaryComputer")
  
  searchArgs <- list()
  searchArgs$q <- stac(endpoint)
  searchArgs$collection <- collection
  searchArgs$ids <- ids
  searchArgs$bbox <- bbox
  searchArgs$datetime <- datetime
  searchArgs$intersects <- intersects
  # #Set limit for planetary computer (max=1000)    #Disable due to rare error.
  # searchArgs$limit <- min(limit, 1000)
  # if(isTRUE(limit>1000)) warning("Limit set to 1000 (maximum for Planetary Computer)")
  
  #Extend query
  query <- do.call(stac_search, searchArgs)
  if(!is.null(eocc)) query <- ext_query(query, "eo:cloud_cover" < eocc)
  if(!is.null(sat.orbit.state)) query <- ext_query(query, "sat:orbit_state" == sat.orbit.state)
  
  #post query
  items <- post_request(query)
  
  # #check if search limit reached
  # if(isTRUE(length(items$features)==limit)) warning(paste0("Limit of ",limit," items reached."))
  
  #Authenticate 
  items <- stac_auth(items, endpoint = endpoint, authOpt=authOpt)
  
  #Optional: Filter on orbit. This can be used to eliminate angular effects.
  if(length(items$features)>0){
    if(!is.null(sat.orbit)){
      if(sat.orbit=="majority"){ #find orbit with majority of observations
        sat.orbit <- lapply(items$features, function(x) x$properties$`sat:relative_orbit`) %>% unlist() %>% table() %>% which.max() %>% names() %>% as.numeric()
      }
      items$features <- items$features[unlist(lapply(items$features, function(x) x$properties$`sat:relative_orbit`))==sat.orbit]
    }
  }
  
  return(items)
}






## For LPDAAC: To be fixed   
#
# eo_stac_search.LPDAAC <- function(collection=NULL, ids=NULL, bbox=NULL, datetime=NULL, intersects=NULL, limit=NULL,
#                                              eocc=NULL, authOpt=list()){
#   
#   searchArgs <- list()
#   searchArgs$q <- stac(endpoint)
#   searchArgs$collection <- collection
#   searchArgs$ids <- ids
#   searchArgs$bbox <- bbox
#   searchArgs$datetime <- datetime
#   searchArgs$intersects <- intersects
#   
#   if(endpoint==as.endpoint("PlanetaryComputer")){
#     #Set limit for planetary computer (max=1000)
#     searchArgs$limit <- min(limit, 1000)
#     if(isTRUE(limit>1000)) warning("Limit set to 1000 (maximum for Planetary Computer)")
#   }
#   
#   query <- do.call(stac_search, searchArgs)
#   
#   if(endpoint==as.endpoint("PlanetaryComputer")){
#     if(!is.null(eocc)) query <- ext_query(query, "eo:cloud_cover" < eocc)
#   }
#   
#   items <- post_request(query)
#   
#   if(endpoint==as.endpoint("lpdaac")){
#     #searchArgs$limit <- NULL
#     items <- items_fetch(items)
#     if(!is.null(eocc)) items <- items_filter(items, properties$`eo:cloud_cover` < eocc)
#   }
#   
#   if(isTRUE(length(items$features)==limit)) warning(paste0("Limit of ",limit," items reached."))
#   
#   #Authenticate 
#   items <- stac_auth(items, endpoint = endpoint, authOpt=authOpt)
#   
#   return(items)
# }
# 
# 
