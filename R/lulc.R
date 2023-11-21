#' Extract Land USe / Land Cover data
#' 
#' Read LULC as SpatRaster 
#' 
#' Designed to be flexible with regard to STAC endpoint and collection, but for now only tested for io-lulc-9-class on PlanetaryComputer 
#' io-lulc-9-class is only generated for the period 2017-2022, for years outside this range a warning will be returned and the neares year returned
#' 
#' @export
#' @import terra
#' @import rstac
#' 
#' @param x a SpatRaster or SpatExtent object, defining the area for which the LULC should be extracted
#' @param year description
#' @param endpoint description, defaults to Microsoft Planetary Computer
#' @param collection STAC collection, defaults to "io-lulc-9-class"
#' @param assets character defining STAC assets name, defaults to "data"
#' @param signOpt list with STAC authentication options passed to stac_auth (e.g. \code{list(key="abc")}), endpoint-specific
#' @param ... additional parameters passed to \code{terra:project}
#' 
#' @returns SpatRaster object
#'
#' 
setGeneric("lulc", function(x, year,
                            endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                            signOpt=list(), ...){
  standardGeneric("lulc")
})

lulc2rast <- function(extent, year,
                        endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="io-lulc-9-class", assets="data",
                        signOpt=list()){
  
  if(source=='io-lulc-9-class'){
    if(year<2017){
      warning("Land cover product currently has data 2017-2022. 2017 selected.")
      year <- 2017 
    }
    if(year>2022){
      warning("Land cover product currently has data 2017-2022. 2022 selected.")
      year <- 2022 
    }
    items <- stac(endpoint) |>
      stac_search(collections=collection, datetime=paste0(year,"-07-01"), bbox=extent[c(1,3,2,4)]) |>
      get_request()
    items <- do.call(stac_auth, c(list(x=items, endpoint=endpoint), signOpt))
    
    if(length(items$features)>1) r <- assets2vrt(items, assets) else r <- assets2rast(items$features[[1]], assets)
    names(r) <- source
    
    cls <- as.data.frame(do.call(rbind, lapply(feature$assets$data$`file:values`, unlist)))
    cls[,1] <- as.numeric(cls[,1])
    names(cls) <- c("value", "landcover")
    levels(r) <- cls
    
    return(r)
  }
}

setMethod("lulc", signature="SpatRaster",
          function(x, year,
                   endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="io-lulc-9-class", assets="data",
                   signOpt=list(), 
                   ...){
            extent <- terra::project(ext(x), crs(x), crs("epsg:4326"))
            lulc_rast <- lulc2rast(extent, year, ndpoint=endpoint, collection=collection, assets=assets, signOpt=signOpt)
            lulc_x <- terra::project(lulc_rast, x, method="near", ...)
            return(lulc_x)
          }
)

