#' Create candidate control raster
#' 
#' @description
#' Create a SpatRaster of impact/treatment and candidate control pixels.  
#' 
#' @details
#' Impact pixels are defined by providing a SpatVector object. 
#' 
#' Pixels to include as candidate control units can be defined from a SpatVector file, using the \code{control_from_include} argument, or from bounding box around the impact spatVector object, using the \code{control_from_buffer} argument. 
#' At least one of these arguments must be provided, if both are provided the \code{control_from_buffer} argument is ignored.
#' 
#' Pixels can be excluded as candidate control units by providing a SpatVector in the \code{control_exclude} argument. 
#' Additionally, pixels at the borders of the impact polygon can be excluded as both impact and/or control with the \code{exclude_impact_buffer} argument, to account for adjacency effects. 
#' A negative value of \code{exclude_impact_buffer} will eliminate pixels in the inner buffer around the polygon as impact units, 
#' a positive value will eliminate pixels in the outer buffer as control units. 
#' Providing a vector of length two can be used to exclude an inner and outer buffer.
#'  
#' @import terra
#' 
#' @export
#' 
#' @param impact SpatVector. Vector representation (points or polygons) of the impact sites
#' @param resolution numeric. Spatial resolution of the output SpatRaster, in units of \code{crs}
#' @param crs Coordinate Reference System in PROJ.4, WKT or authority:code notation. Defaults to the UTM zone of center coordinates of \code{impact}
#' @param control_from_buffer numeric vector of 1, 2, or 4 elements. Indicates by how much (in units of \code{crs}) the spatial extent around the impact sites should be enlarged on each side. 
#' @param control_from_include SpatVector. Indicates area to include in candidate control selection
#' @param control_exclude SpatVector. Indicates area to exclude from candidate control selection
#' @param exclude_impact_buffer numeric vector of length 1 or 2. Indicates buffer around \code{impact}, in units of \code{crs}, to exclude as impact and/or control. See Details.
#' @param round_coords logical or integer. Should the coordinates of the output SpatRaster be rounded.
#' If TRUE, coordinates are rounded to the nearest integer; a positive numeric rounds to the corresponding decimal,
#' a negative integer rounds to the corresponding power of 10 (e.g., \code{round_coords=-2} rounds to the nearest 100).
#' 
#' @returns a SpatRaster in which impact pixels have value 1, candidate control pixels have value 0, 
#' and pixels excluded as impact or control have value NA
#'
create_control_candidates <- function(impact,
                                      resolution,
                                      crs,
                                      control_from_buffer=NULL,
                                      control_from_include=NULL,
                                      #control_from_bbox=NULL,
                                      control_exclude=NULL,
                                      exclude_impact_buffer=NULL,
                                      #control_fraction=NULL, #not yet implemented
                                      #control_N=NULL, #not yet implemented
                                      round_coords=FALSE){
  
  #Check if all required arguments are provided
  if(missing(impact)) stop("Impact units must be provided to create candidate control units")
  if(missing(resolution)) stop("Resolution must be provided to create candidate control units")
  
  #check class of inputs
  # to do
  
  #Get crs from impact spatVector
  if(missing(crs)) crs <- utm_zone(impact, proj4string = TRUE)
  impact_projected <- project(impact, crs)
  ext_i <- ext(impact_projected)

  #Define output extent
  if(!is.null(control_from_include)){
    control_projected <- project(control_from_include,crs)
    ext_c <- ext(control_projected)
    ext_ref <- ext(min(ext_i[1], ext_c[1]), max(ext_i[2], ext_c[2]), min(ext_i[3], ext_c[3]), max(ext_i[4], ext_c[4]))
  } else if (!is.null(control_from_buffer)){
    ext_ref <- extend(ext_i, control_from_buffer)
  } else {
    stop("Either control_from_buffer or control_from_include must be provided.")
  }
    
  #Check if output extent coordinates must be rounded
  if(!isFALSE(round_coords)){
    if(isTRUE(round_coords)) round_coords <- 0
    ext_ref <- base::round(ext_ref, round_coords)
  }
  
  #Reference raster
  rast_ref <- terra::rast(extent=ext_ref, crs=crs, resolution=resolution, vals=0)
  
  #Fill the reference raster with control(0)/impact(1)/NA
  if(!is.null(control_from_include)){
    rast_out <- terra::rasterize(impact_projected, rast_ref, background=NA)
    rast_out <- terra::mask(rast_out, control_projected, updatevalue=0, inverse=TRUE)
  } else if (!is.null(control_from_buffer)){
    rast_out <- terra::rasterize(impact_projected, rast_ref, background=0)
  }
  
  #Exclude areas identified by control_exclude
  if(!is.null(control_exclude)){
    rast_out <- mask(rast_out, terra::project(control_exclude, crs), updatevalue=NA, inverse=TRUE)
  }
  
  #Exclude areas identified by exclude_impact_buffer
  if(!is.null(exclude_impact_buffer)){
    
    #Different functions required for inner and outer buffer
    fun_buffer <- function(val){
      if (val>0){
        buf <- erase(terra::buffer(aggregate(snap(impact_projected, tolerance=resolution/2), dissolve=TRUE), width=val),
                     snap(impact_projected, tolerance=resolution/2)) 
      } else {
        buf <- erase(snap(impact_projected, tolerance=resolution/2), 
                     terra::buffer(aggregate(snap(impact_projected, tolerance=resolution/2), dissolve=TRUE), width=val))
      }
      return(buf)
    }
    
    if(length(exclude_impact_buffer)==1){
      buf <- fun_buffer(exclude_impact_buffer)
    } else {
      buf1 <- fun_buffer(exclude_impact_buffer[1])
      buf2 <- fun_buffer(exclude_impact_buffer[2])
      buf <- aggregate(rbind(buf1, buf2), dissolve=TRUE)
    }
    
    rast_out <- mask(rast_out, buf, updatevalue=NA, inverse=TRUE)
    
  }

  return(rast_out)
}
  
  
