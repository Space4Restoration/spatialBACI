

#' Extract matches values from SpatRaster/data cube 
#'
#' @description
#' Helper function to extract matches from SpatRaster or data.cube object
#'
#' @importFrom sf st_as_sf
#' @importFrom gdalcubes srs select_bands join_bands extract_geom
#' @import terra 
#' 
#' @export
#' 
#' @param r a SpatRaster or data.cube raster object.
#' @param obj output of [matchCI()].
#' @param fun function to summarize extracted data by line or polygon geometries. See [terra::extract()].
#' @param method character. Method for extracting values for point geometries. See [terra::extract()].
#' @param ... additional arguments to `fun`.
#' 
#' @returns data.table of extracted raster values for control-impact pairs.
#' 
extractMatches <- function(r, obj, fun, method="simple", ...){
  
  spat.ref <- obj$spat.ref
  
  if(is.SpatRaster(spat.ref)){
    
    matches <- obj$matches
    matches_cellnr <- matches$cell
    matches_crs <- terra::crs(spat.ref)
    matches_xy <- terra::xyFromCell(spat.ref, matches_cellnr)
    
    xy_unique <- unique(matches_xy)
    
    if(is.SpatRaster(r)){
      xy_unique_proj <- terra::project(xy_unique, from=spat.ref, to=r)
      vals <- terra::extract(r, xy_unique_proj, method=method)
      vals <- cbind(xy_unique, vals)
    } else if (is.cube(r)){
      xy_unique_sf <- sf::st_as_sf(x=as.data.frame(xy_unique), coords=c("x","y"), crs=terra::crs(spat.ref), remove=FALSE)
      vals <- gdalcubes::extract_geom(r, xy_unique_sf, merge=TRUE, drop_geom=TRUE)
      vals <- subset(vals, select=-c(time))
    } 
    out <- merge(cbind(matches,matches_xy), 
                 vals, 
                 by=c("x", "y"), sort=FALSE) 
    
    return(subset(out, select=-c(x, y)))
  }
  
  if(is.SpatVector(spat.ref)){
    
    matches <- obj$matches
    
    if(is.SpatRaster(r)){
      spat.ref_proj <- terra::project(spat.ref, r)

      if(terra::is.polygons(spat.ref) | terra::is.lines(spat.ref)){
        if(missing(fun)) fun <- mean
        vals <- terra::extract(r, spat.ref_proj, fun=fun, bind=TRUE, ...)
      } else{
        if(missing(method)) method="near"
        vals <- terra::extract(r, spat.ref_proj, method=method, bind=TRUE, ...)
      }
    } else if (is.cube(r)){
      
      spat.ref_sf <- sf::st_as_sf(spat.ref)

      if(terra::is.polygons(spat.ref) | terra::is.lines(spat.ref)){
        if(missing(fun)) fun <- mean
        vals <- gdalcubes::extract_geom(r, spat.ref_sf, FUN=fun, reduce_time=TRUE, merge=TRUE, drop_geom=TRUE)
        
      } else{

        vals <- gdalcubes::extract_geom(r, spat.ref_sf, merge=TRUE, drop_geom=TRUE)
        vals <- subset(vals, select=-time)
      }
    }
    
    out <- merge(matches,
                 vals, 
                 sort=FALSE)
    
    return(out)
  }
}

