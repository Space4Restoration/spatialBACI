#' Extract data for matches
#' 
#' Extract the "before" and "after", or "effect", values for the matching pairs
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
