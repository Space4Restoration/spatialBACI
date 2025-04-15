


#' Extract data from raster for matches
#'
#' Extract the "before" and "after", or "effect", values from a SpatRaster or data cube for the provided matching pairs
#'
#' @importFrom sf st_as_sf
#' @importFrom gdalcubes srs select_bands join_bands extract_geom
#' @importFrom terra rast subset extract
#'
#' @export
#'
#' @param matches output of \code{matchCI}
#' @param effect SpatRaster of effect values
#' @param before SpatRaster of before values
#' @param after SpatRaster of after values
#' @param band character. If before/after contain more than one band, this parameter can be used to select the band to be extracted
#'
#'
extractMatches <- function(matches, effect, before, after, band=NULL){
  #Need a better name for this function
  
  if(missing(effect) & (missing(before) | missing(after))){
    stop('Either "effect", or both "before" and "after" must be provided')
  }
  
  if(!missing(effect)){
    if(!(is.cube(effect) | is.spatRaster(effect))){
      stop('Effect must be a data cube or spatRaster')
    }
  } else{
    if(!((is.cube(before) & is.cube(after)) | (is.spatRaster(before) & is.spatRaster(after)))){
      stop('Before and after object must be of the same class, either data cube or spatRaster')
    }
  }
  
  #subset band, if parameter provided
  if(!is.null(band)){
    if(!missing(effect)){
      effect <- sel_band(effect, band)
    } else {
      before <- sel_band(before, band)
      after <- sel_band(after, band)
    }
  }
  
  if(!missing(effect)){
    if(is.cube(effect)){
      m <- gdalcubes::extract_geom(effect,
                                   sf::st_as_sf(x=matches, coords=c("x","y"), crs=gdalcubes::srs(effect)))
      names(m)[3] <- "effect"
    } else {
      m <- terra::extract(effect, as.matrix(matches[,c("x","y")]))
      names(m) <- "effect"
    }
    matched_data <- cbind(matches, m["effect"]) #this may result in error when missing data occur in data cube, check and replace by merge
    
  } else {
    if(is.cube(before)){
      m_before <- gdalcubes::extract_geom(before,
                                          sf::st_as_sf(x=matches, coords=c("x","y"), crs=gdalcubes::srs(before)))
      names(m_before)[3] <- "before"
      m_after  <- gdalcubes::extract_geom(after,
                                          sf::st_as_sf(x=matches, coords=c("x","y"), crs=gdalcubes::srs(after)))
      names(m_after)[3] <- "after"
    } else {
      m_before <- terra::extract(before, as.matrix(matches[,c("x","y")]))
      names(m_before) <- "before"
      m_after <- terra::extract(after, as.matrix(matches[,c("x","y")]))
      names(m_after) <- "after"
    }
    matched_data <- cbind(matches, m_before["before"], m_after["after"])
  }
  
  return(matched_data)
}


#' generic method for selecting band
#' 
#' 
#' @noRd
sel_band <- function(x,...){
  if(is.cube(x)) sel_band.cube(x, ...)
  if(is.spatRaster(x)) sel_band.SpatRaster(x, ...)
}
#' for cube
#' 
#' @noRd
sel_band.cube <- function(x, ...){
  gdalcubes::select_bands(x, ...)
}

#' for SpatRaster
#' 
#' 
#' @noRd
sel_band.SpatRaster <- function(x, ...){
  terra::subset(x, ...)
}




##To be updated

#' Extract from raster
#' 
#' Generic function to extract from SpatRaster or data cube object
#' 
#' @import terra
#' @importFrom gdalcubes extract_geom
#' @importFrom data.table as.data.table
#' @importFrom sf st_as_sf
#' 
#' 
#' @param name description
#' @param xy description
#' @param crs_xy description
#' 
#' @returns data.table
#' 
extractFromRaster <- function(r, xy, crs_xy){
  
  if(missing(r)) stop('Argument "r" is missing.')
  if(missing(xy)) stop('Argument "xy" is missing.')
  
  
  xy_unique <- unique(xy[,c("x","y")])
  
  if(is.SpatRaster(r)){
    
    if(!missing(crs_xy)){
      #as.matrix(xy) |> terra::project(crs_xy, crs(r)) |> as.data.table
    }
    
    cells <- terra::cellFromXY(r,as.matrix(xy_unique))
    vals <- terra::extract(r, cells)
    out <- merge(xy, 
                 cbind(xy_unique, vals), 
                 by=c("x", "y"), sort=FALSE)

  } else if (is.cube(r)){
    
    if(!missing(crs_cy)){
      #...
    }
    
    vals <- gdalcubes::extract_geom(r,  sf::st_as_sf(x=xy_unique, coords=c("x","y"), crs=gdalcubes::srs(r)))
    out <- merge(xy, 
                 subset(cbind(xy_unique, vals), select=-c(FID, time)),
                 by=c("x", "y"), sort=FALSE)
  } else (stop("r must be SpatRaster of data cube object"))
  
  return(out)
  
}
  
  
#' Extract from generic raster
#' 
#' Generic function to extract from SpatRaster or data cube object
#' 
#' @export
#' 
#' @importFrom terra extract project
#' @importFrom gdalcubes extract_geom
#' @importFrom sf st_as_sf
#' 
#' @param x SpatRaster or data cube
#' @param y SpatVector or sf. Additionally, if x is SpatRaster, see possibilities in \code{terra::extract}
#' @param fun function to summarize by line or polygon geometry
#' @param method method for extracting values with points
#' @param reduce_time see \code{gdalcubes::extract_geom}
#' @param na.rm additional argument to \code{fun}
#' @param names.out not implemented
#' @param ... Additional arguments passed to \code{terra::extract} or \code{gdalcubes::extract_geom}
#' 
#' @returns data.frame, matrix or SpatVector (see \code{terra::extract} or \code{gdalcubes::extract_geom})
#' 
extractGeneric <- function(x, y, fun=mean, method="simple", reduce_time=TRUE, names.out=NULL, ...){
  
  if(is.character(x)){
    x <- rast(x)
  }
  
  if(is.SpatRaster(x)){
    
    if(is.sf(y)) y <- terra::vect(y)
    if(is.SpatVector(y)) if(crs(x) != crs(y)) y <- terra::project(y,x)
    
    out <- terra::extract(x, y, 
                          fun=fun, 
                          method=method, 
                          ...)
    
  } else if (is.cube(r)){
    
    if(is.SpatVector(y)) y <- sf::st_as_sf(y)
    out <- gdalcubes::extract_geom(x, y, FUN=fun, reduce_time=reduce_time, ...)

  } else {
    stop("x must be SpatRaster of data cube object")
  }
  
  if(!is.null(names.out)){
    nnames <- length(names.out)
    names(out)[(length(names(out))-nnames+1):length(names(out))] <- names.out
  } 
  return(out)
  
}  
