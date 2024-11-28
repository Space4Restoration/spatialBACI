#' Extract Digital Elevation Model
#' 
#' @description
#' Read Digital Elevation Model and calculate derivatives.
#' 
#' @details
#' 
#' This function returns a data cube object when only the digital terrain model is required (i.e. \code{v="elevation"}), and a SpatRaster when the terrain derivatived are required.
#' The reason for this is that calculating terrain parameters using the terra::terrain function outperforms the same calculation on data cube, with regard to time.  
#' 
#' \code{dem_source} must be a list with STAC specifications (named list with list elements "endpoint", "collection", "assets" and (optionally) "authOpt". 
#' If no DEM source it provided, this argument defaults to \code{list(endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="nasadem", assets="elevation", authOpt=list())}
#' 
#' Examples of available global DEMs with STAC endpoint/collection/assets specification and whether authentication is required to access the DEM:
#' 
#' | name | endpoint | collection | assets | auth. | 
#' | --- | --- | ---| --- | --- |
#' | | | | |
#' | \strong{NASA DEM (default)} | https://planetarycomputer.microsoft.com/api/stac/v1 | "nasadem" | "elevation" | no |
#' | ALOS World DEM 30m | https://planetarycomputer.microsoft.com/api/stac/v1 | "alos-dem" | "data" | no |
#' | Copernicus DEM 30m | https://planetarycomputer.microsoft.com/api/stac/v1 | "cop-dem-glo-30" | "data" | yes |
#' | Copernicus DEM 90m | https://planetarycomputer.microsoft.com/api/stac/v1 | "cop-dem-glo-90" | "data" | yes |
#' 
#' @md
#' 
#' @export
#' @importFrom terra ext res crs
#' 
#' @param x a SpatRaster, spatVector, or SpatExtent object, defining the area for which the DEM should be extracted
#' @param dx numeric. output resolution in x dimension
#' @param dy numeric. output resolution in y dimension
#' @param srs character. output spatial reference system
#' @param dem_source Source of the Digital Elevation Model, see Details
#' @param v character vector. Terrain parameters to be calculated
#' @param neighbors integer. Either 8 (queen case) or 4 (rook case), indicating which neighbors to use to compute slope or aspect. 
#' @param unit character. "degrees" or "radians" for the output of "slope" and "aspect"
#' @param transformAspect logical. Should aspect be transformed to "northness" and "eastness" 
#' 
#' @returns a data cube or SpatRaster object (see Details)
#' 
setGeneric("dem", function(x, ...){
  standardGeneric("dem")
})

setMethod("dem", signature="SpatRaster",
          function(x, 
                   dem_source,
                   v=c("elevation", "slope", "aspect"), neighbors=8, unit="radians", transformAspect=TRUE){
            
            dem_args <- list()
            extent <- terra::ext(x)
            dem_args$xmin <- extent$xmin
            dem_args$xmax <- extent$xmax
            dem_args$ymin <- extent$ymin
            dem_args$ymax <- extent$ymax
            dem_args$dx <- terra::res(x)[1]
            dem_args$dy <- terra::res(x)[2]
            dem_args$srs <- terra::crs(x)
            dem_args$v <- v
            dem_args$neighbors <- neighbors
            dem_args$unit <- unit
            dem_args$transformAspect <- transformAspect
            if(!missing(dem_source)) dem_args$dem_source <- dem_source
            
            out <- do.call(get_dem, dem_args) 
            return(out)
})

setMethod("dem", signature="SpatVector",
          function(x, 
                   dx, dy,
                   dem_source,
                   v=c("elevation", "slope", "aspect"), neighbors=8, unit="radians", transformAspect=TRUE){
            
            dem_args <- list()
            extent <- terra::ext(x)
            dem_args$xmin <- extent$xmin
            dem_args$xmax <- extent$xmax
            dem_args$ymin <- extent$ymin
            dem_args$ymax <- extent$ymax
            dem_args$srs <- terra::crs(x)
            dem_args$v <- v
            dem_args$neighbors <- neighbors
            dem_args$unit <- unit
            dem_args$transformAspect <- transformAspect
            if(!missing(dx)) dem_args$dx <- dx
            if(!missing(dy)) dem_args$dy <- dy
            if(!missing(dem_source)) dem_args$dem_source <- dem_source
            
            out <- do.call(get_dem, dem_args) 
            return(out)
})

setMethod("dem", signature="SpatExtent",
          function(x,
                   dx, dy, srs,
                   dem_source, 
                   v=c("elevation", "slope", "aspect"), neighbors=8, unit="radians", transformAspect=TRUE){
            
            dem_args <- list()
            dem_args$xmin <- x$xmin
            dem_args$xmax <- x$xmax
            dem_args$ymin <- x$ymin
            dem_args$ymax <- x$ymax
            dem_args$v <- v
            dem_args$neighbors <- neighbors
            dem_args$unit <- unit
            dem_args$transformAspect <- transformAspect
            dem_args$srs <- srs
            if(!missing(dx)) dem_args$dx <- dx
            if(!missing(dy)) dem_args$dy <- dy
            if(!missing(dem_source)) dem_args$dem_source <- dem_source
            
            out <- do.call(get_dem, dem_args) 
            return(out)
})

#' Wrapper function for DEM and terrain derivatives calculation
#'
#' @importFrom terra rast ext res crs is.lonlat terrain
#'
#' @noRd
#' 
get_dem <- function(xmin, xmax, ymin, ymax,
                    dx, dy, srs,
                    dem_source,
                    v, neighbors, unit, transformAspect){
  
  if(missing(dem_source)) dem_source <- list(endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="nasadem", assets="elevation", authOpt=list())

  #Default values for missing srs? (assume epsg:4326) - To be added
  
  #Default values for missing dx, dy
  if(missing(dx) | missing(dy)){
    if(!(missing(dx) & missing(dy))){
      warning("Missing output resolution for one dimension missing, same resolution for both dimensions used")
      if(missing(dx)) dx <- dy else dy <- dx
    } else {
      warning("Missing output resolution in x and y dimension, applying collection default")
      
      if(dem_source$endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
        if(dem_source$collection=="nasadem") if(terra::is.lonlat(srs)) dx <- dy <- 0.0002777778 else dx <- dy <- 30
        if(dem_source$collection=="cop-dem-glo-30") if(terra::is.lonlat(srs)) dx <- dy <- 0.0002777778 else dx <- dy <- 30
        if(dem_source$collection=="cop-dem-glo-90") if(terra::is.lonlat(srs)) dx <- dy <- 0.0008333333 else dx <- dy <- 90
      } else {
        stop("Defaults for endpoint not implemented")
      }
    }
  }
    
  dem_args <- dem_source
  dem_args$xmin <- xmin
  dem_args$xmax <- xmax
  dem_args$ymin <- ymin
  dem_args$ymax <- ymax
  dem_args$dx <- dx
  dem_args$dy <- dy
  dem_args$srs <- srs

  #load DEM cube 
  elev <- do.call(dem2cube, dem_args)
  
  d <- v[v!="elevation"]
  if(length(d) == 0){
    #If only elevation is requested, return DEM as gdalcube
    return(elev)
    
  } else {
    #If terrain derivatives are to be calculated, transform to SpatRaster and use terra::terrain for efficiency
    
    elev_r <- as.SpatRaster(elev)
    ter_r <- terra::terrain(elev_r, v=d, neighbors=neighbors, unit=unit)
    
    if(("aspect" %in% d) & isTRUE(transformAspect)){
      if(unit=="degrees") d2r <- pi/180 else d2r <- 1
      aspect_trans <- terra::rast(list(northness=cos(d2r*subset(ter_r, "aspect")),
                                       eastness= sin(d2r*subset(ter_r, "aspect"))))
      ter_r <- terra::rast(list(subset(ter_r, "aspect", negate=TRUE), 
                                aspect_trans))
    }
    
    if("elevation" %in% v)
      return(terra::rast(list(elev_r, ter_r)))
    else
      return(ter_r)

  }
}


#' Read DEM as data cube
#' 
#' Read a DEM from a STAC collection or from file as a gdalcubes data cube
#' 
#' 
#' @export
#' 
#' @importFrom gdalcubes stac_image_collection cube_view extent raster_cube rename_bands
#' @import rstac
#' 
#' @param xmin numeric. Minimum x-coordinate of output DEM
#' @param xmax numeric. Maximum x-coordinate of output DEM
#' @param ymin numeric. Minimum y-coordinate of output DEM
#' @param ymax numeric. Maximum y-coordinate of output DEM
#' @param dx numeric. Pixels size of output DEM in x-dimension
#' @param dy numeric. Pixels size of output DEM in y-dimension
#' @param srs character. Description of spatial reference system
#' @param endpoint character. STAC endpoint
#' @param collection character. STAC collection
#' @param assets character. STAC asset name
#' @param authOpt STAC authentication options (endpoint-dependent)
#' @param filename character vector of input filenames
#' @param ... additional arguments (not used)
#' 
#' @returns a gdalcubes data cube
#' 
dem2cube <- function(xmin, xmax, ymin, ymax,
                     dx, dy, srs,
                     endpoint,  collection, assets, authOpt=list(),
                     filename=NULL, ...
                     ){
   
  #to do: 
  # * implement reading data cube from file (using mock date_time="2000-01-01")
  # if(!is.null(filename)){
  #   
  # } else {
  #   
  #   
  # }

  ##  STAC items from query
  searchArgs <- list()
  searchArgs$q <- stac(endpoint)
  searchArgs$collection <- collection
  searchArgs$bbox <- as.bbox(xmin, xmax, ymin, ymax, crs_from=srs, xy=FALSE)
  
  query <- do.call(stac_search, searchArgs)
  items <- post_request(query)
  items <- stac_auth(items, endpoint = endpoint, authOpt=authOpt)
  
  ##  gdalcubes image collection from STAC ItemCollection
  f_args <- list(s=items$features)
  f_args$asset_names <- assets
  imgCollection <- do.call(gdalcubes::stac_image_collection, f_args)
  
  # Creating cube view (may display messages related to extending size cube, should provide param to suppress these)
  cv <- gdalcubes::cube_view(extent=list(t0=substr(gdalcubes::extent(imgCollection)$t0,1,10), t1=substr(gdalcubes::extent(imgCollection)$t1,1,10),
                              left= xmin, right= xmax, bottom=ymin, top=ymax),
                             srs=srs,
                             dx=dx, dy=dy, dt="P1D",
                             aggregation="median", resampling="bilinear")
  
  cube <- gdalcubes::raster_cube(imgCollection, cv)
  
  # Rename dem band name to "elevation"
  rename_args <- list("elevation")
  names(rename_args) <- names(cube)
  rename_args$cube <- cube
  cube <- do.call(gdalcubes::rename_bands, rename_args)
  
  return(cube)
  
}


##Calculating terrain characteristics unefficient for large data cubes, function to be archived

#' Terrain characteristics
#' 
#' Compute terrain characteristics from digital elevation model data cube.
#' 
#' Currently, only "slope" and "aspect" are implemented, and only projected DEM layers with equal resolution in x and y are supported.
#' Only 8 ngb case implemented. Aspect is provided in the range [-pi,pi], rather than the conventional [0,2*pi].
#' 
#' @importFrom gdalcubes dimensions window_space rename_bands apply_pixel
#' 
#' @param cube data cube. Digital Elevation Model
#' @param v character vector. Terrain parameters to be calculated
#' @param neighbors integer. Either 8 (queen case) or 4 (rook case), indicating which neighbors to use to compute slope or aspect. 
#' @param unit character. "degrees" or "radians" for the output of "slope" and "aspect"
#' 
#' @returns a gdalcubes data cube
#' 
#' @noRd
#' 
terrain.cube <- function(cube, v=c("slope"), neighbors=8, unit="degrees"){
  
  stopifnot(is.cube(cube))
  stopifnot(neighbors==8 | neighbors==4)
  
  #Kernels in x and y dimension
  dx <- gdalcubes::dimensions(cube)$x$pixel_size
  dy <- gdalcubes::dimensions(cube)$y$pixel_size
  if (neighbors==8){
    x_kernel <- matrix(data=c(1, 0, -1,
                              2, 0, -2,
                              1, 0, -1)/(8*dx),
                       nrow=3, ncol=3, byrow=TRUE)
    y_kernel <- matrix(data=c(1, 2, 1,
                              0, 0, 0,
                              -1,-2,-1)/(8*dy),
                       nrow=3, ncol=3, byrow=TRUE)
  } else if (neighbors==4){
    x_kernel <- matrix(data=c(1, 0, -1)/(2*dx),
                       nrow=1, ncol=3, byrow=TRUE)
    y_kernel <- matrix(data=c(1, 0, -1)/(2*dy),
                       nrow=3, ncol=1, byrow=TRUE)
  } else{
    stop('"neighbors" must be either 8 or 4')
  }

  #Slopes in x and y
  dz_dx <- gdalcubes::window_space(cube, kernel=x_kernel) 
  dz_dy <- gdalcubes::window_space(cube, kernel=y_kernel)
  slopes <- gdalcubes::join_bands(list(dz_dx, dz_dy), cube_names = c("X1", "X2"))
  #Assign correct names to bands
  rename_args <- list("dz_dx", "dz_dy"); names(rename_args) <- names(slopes); rename_args$cube <- slopes
  slopes <- do.call(gdalcubes::rename_bands, rename_args)
  
  out.list <- list()
  if("slope" %in% v){
    slope <- gdalcubes::apply_pixel(slopes, expr="atan(sqrt(dz_dx^2 + dz_dy^2))", names="slope")
    if(unit=="degrees") slope <- gdalcubes::apply_pixel(slope, expr="slope*180/pi", names="slope")
    out.list$slope <- slope
  }                               
  if("aspect" %in% v){
    aspect <- gdalcubes::apply_pixel(slopes, expr="atan2(dz_dy, -dz_dx)", names="aspect")
    if(unit=="degrees") aspect <- gdalcubes::apply_pixel(aspect, expr="aspect*180/pi", names="aspect")
    out.list$aspect <- aspect
  }                                
  
  #Merge different outputs in single data cube
  if(length(out.list)>1){
    out.cube <- gdalcubes::join_bands(out.list, cube_names=names(out.list))
    rename_args <- as.list(names(out.list)); names(rename_args) <- names(out.cube); rename_args$cube <- out.cube
    out.cube <- do.call(gdalcubes::rename_bands, rename_args)
  } else {
    out.cube <- out.list[[1]]
  }
  
  return(out.cube)
}





###Old version functions. Deprecated? 

# 
# #deprecated
# dem2rast <- function(extent, 
#                      endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="nasadem", assets="elevation",
#                      authOpt=list()){
#   
#   items <- stac(endpoint) |>
#     stac_search(collections=collection, bbox=extent[c(1,3,2,4)]) |>
#     get_request()
#   items <- do.call(stac_auth, c(list(x=items, endpoint=endpoint), authOpt))
#   
#   #Changed this from ==1 to >1, think this was a bug, check if correct
#   if(length(items$features)>1) dem <- assets2vrt(items, assets) else dem <- assets2rast(items$features[[1]], assets)
#   names(dem) <- assets
#   
#   dem <- terra::crop(dem, extent)
#   return(dem)
# }
# 
# #Deprecated
# transform_aspect.SpatRaster <- function(r, unit="degrees"){
#   if(unit=="degrees") d2r <- pi/180 else d2r <- 1
#   asp <- rast(list(cos_aspect=cos(d2r*r["aspect"]),
#                    sin_aspect=sin(d2r*r["aspect"])))
#   r <- rast(list(subset(r, "aspect", negate=TRUE), asp))
#   return(r)
# }
