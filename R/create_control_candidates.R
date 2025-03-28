#' Create candidate control raster
#' 
#' Creates a spatRaster of impact and candidate control pixels 
#'  
#' Impact pixels are defined by providing a spatVector object. Candidate control pixels can be defined from a spatVector file, or using a buffer around the bounding box of the impact sites.
#'  
#' @import terra
#' 
#' @export
#' 
#' @param impact spatVector. Vector representation (points or polygons) of the impact sites
#' @param resolution numeric. Spatial resolution of the output spatRaster
#' @param crs Coordinate Reference System in PROJ.4, WKT or authority:code notation. Defaults to the UTM zone of center coordinates of \code{impact}
#' @param control_from_buffer numeric vector of 1, 2, or 4 elements. Indicates by how much (in units of crs) the spatial extent of x/y should be enlarged on each side.
#' @param control_from_include spatVector. Indicates area to include in candidate control selection (optional)
#' @param control_exclude spatVector. Indicates area to exclude from candidate control selection
#' @param round_coords logical or integer. Should the coordinates of the reference SpatRaster be rounded.
#' If TRUE, coordinates are rounded to the nearest integer; if a positive numeric rounded to the corresponding decimal; 
#' if a negative integer rounded to the corresponding power of 10 (e.g., \code{round_corrs=2} round to the nearest 100).
#' 
#' @returns a spatRaster in which impact pixels have value 1, candidate control pixels value 0, and all other pixels value NA
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
    if (exclude_impact_buffer >0){
      buf <- terra::buffer(impact_projected, width=exclude_impact_buffer) |> 
        erase(impact_projected)
    } else {
      buf <- erase(impact_projected, terra::buffer(impact_projected, width=exclude_impact_buffer))
    }
    rast_out <- mask(rast_out, buf, updatevalue=NA, inverse=TRUE)
  }

  return(rast_out)
}
  
  
