


nasadem2rast <- function(extent, 
                         endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="nasadem", assets="elevation",
                         signOpt=list()){
  
  library(terra)
  library(rstac)

  items <- stac(endpoint) %>%
    stac_search(collections=collection, bbox=extent[c(1,3,2,4)]) %>%
    get_request()
  items <- do.call(stac_auth, c(list(x=items, endpoint=endpoint), signOpt))
  
  if(length(items$features)==1) dem <- assets2vrt(items, assets) else dem <- assets2rast(items$features[[1]], assets)
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

setGeneric("nasadem", function(x, v=c("slope", "aspect"), unit="degrees", aspectTransform=TRUE, ...,
                               endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                               signOpt=list()) standardGeneric("nasadem"))

setMethod("nasadem", signature="SpatRaster",
          function(x, v=c("slope", "aspect"), unit="degrees", aspectTransform=TRUE, ...,
                   endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                   signOpt=list()){
            library(terra)
            
            extent <- terra::project(ext(x), crs(x), crs("epsg:4326"))
            dem <- nasadem2rast(extent, endpoint=endpoint, collection=collection, assets=assets, key=key, usr=usr, pwd=pwd)
            
            dem <- terra::project(dem, x, method="bilinear")
            
            if(!is.null(v)) {
              dem <- rast(list(dem=dem, terrain(dem, v=v, unit=unit)))
              if(("aspect" %in% v) & isTRUE(aspectTransform)) dem <- transform_aspect(dem)
            }
            return(dem)
          })

setMethod("nasadem", signature="SpatExtent",
          function(x, CRS, v=c("slope", "aspect"), unit="degrees", aspectTransform=TRUE, ...,
                   endpoint="https://planetarycomputer.microsoft.com/api/stac/v1",  collection="nasadem", assets="elevation",
                   signOpt=list()){
            library(terra)
            extent <- x
            if(!missing(CRS)) CRS <- crs("epsg:4326")
            extent <- terra:project(extent, CRS, crs("epsg:4326"))
            dem <- nasadem2rast(extent, endpoint=endpoint, collection=collection, assets=assets, key=key, usr=usr, pwd=pwd)
            
            if(!is.null(v)) {
              dem <- rast(list(dem=dem, terrain(dem, v=v, unit=unit)))
              if(("aspect" %in% v) & isTRUE(aspectTransform)) dem <- transform_aspect(dem)
            }
            
            return(dem)
          }
)
