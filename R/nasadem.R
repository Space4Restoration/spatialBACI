#' Extract Digital Elevation Model
#' 
#' Read NASA (SRTM) Digital Elevation Model as SpatRaster and calculate derivatives
#' 
#' @export
#' @import terra
#' @import rstac
#' 
#' @param x a SpatRaster or SpatExtent object, defining the area for which the DEM should be extracted
#' @param v description
#' @param unit description
#' @param aspectTransform description
#' @param endpoint description
#' @param collection STAC collection, defaults to "nasadem"
#' @param assets character defining STAC assets name, defaults to "elevation"
#' @param signOpt list with STAC authentication options passed to stac_auth (e.g. \code{list(key="abc")}), endpoint-specific
#' @param ... additional parameters passed to ???
#' 
#' @returns SpatRaster object
#' 
#' 
setGeneric("nasadem", function(x, v=c("slope", "aspect"), unit="degrees", aspectTransform=TRUE, 
                               endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                               authOpt=list(), ...){
  standardGeneric("nasadem")
})

nasadem2rast <- function(extent, 
                         endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="nasadem", assets="elevation",
                         authOpt=list()){

  items <- stac(endpoint) |>
    stac_search(collections=collection, bbox=extent[c(1,3,2,4)]) |>
    get_request()
  items <- do.call(stac_auth, c(list(x=items, endpoint=endpoint), authOpt))
  
  #Changed this from ==1 to >1, think this was a bug, check if correct
  if(length(items$features)>1) dem <- assets2vrt(items, assets) else dem <- assets2rast(items$features[[1]], assets)
  names(dem) <- assets
  
  dem <- terra::crop(dem, extent)
  return(dem)
}

transform_aspect <- function(r, unit="degrees"){
  if(unit=="degrees") d2r <- pi/180 else d2r <- 1
  asp <- rast(list(cos_aspect=cos(d2r*r["aspect"]),
                   sin_aspect=sin(d2r*r["aspect"])))
  r <- rast(list(subset(r, "aspect", negate=TRUE), asp))
  return(r)
}

setMethod("nasadem", signature="SpatRaster",
          function(x, v=c("slope", "aspect"), unit="degrees", aspectTransform=TRUE,
                   endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                   authOpt=list()){

            extent <- terra::project(ext(x), crs(x), crs("epsg:4326"))
            dem <- nasadem2rast(extent, endpoint=endpoint, collection=collection, assets=assets, authOpt=authOpt)
            
            dem <- terra::project(dem, x, method="bilinear")
            
            if(!is.null(v)) {
              dem <- rast(list(dem=dem, terrain(dem, v=v, unit=unit)))
              if(("aspect" %in% v) & isTRUE(aspectTransform)) dem <- transform_aspect(dem)
            }
            return(dem)
          })

setMethod("nasadem", signature="SpatExtent",
          function(x,  v=c("slope", "aspect"), unit="degrees", aspectTransform=TRUE,
                   endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                   signOpt=list(), CRS=NULL){

            extent <- x
            #if(!missing(CRS)) CRS <- crs("epsg:4326")
            if(is.null(CRS)) CRS <- crs("epsg:4326")
            extent <- terra::project(extent, CRS, crs("epsg:4326"))
            dem <- nasadem2rast(extent, endpoint=endpoint, collection=collection, assets=assets, authOpt=authOpt)
            if(!is.null(v)) {
              dem <- rast(list(dem=dem, terrain(dem, v=v, unit=unit)))
              if(("aspect" %in% v) & isTRUE(aspectTransform)) dem <- transform_aspect(dem)
            }
            return(dem)
          }
)
