#' Create candidate control raster
#' 
#' @description
#' Create a SpatRaster of impact/treatment and candidate control pixels.  
#' 
#' @details
#' Impact pixels are defined by providing a SpatVector object. 
#' 
#' Pixels to include as candidate control units can be defined from a SpatVector file, using the `control_from_include` argument, or from bounding box around the impact SpatVector object, using the `control_from_buffer` argument. 
#' At least one of these arguments must be provided, if both are provided the `control_from_buffer` argument is ignored.
#' 
#' Pixels can be excluded as candidate control units by providing a SpatVector in the `control_exclude` argument. 
#' Additionally, pixels at the borders of the impact polygon can be excluded as both impact and/or control with the `exclude_impact_buffer` argument, to account for adjacency effects. 
#' A negative value of `exclude_impact_buffer` will eliminate pixels in the inner buffer around the polygon as impact units, 
#' a positive value will eliminate pixels in the outer buffer as control units. 
#' Providing a vector of length two can be used to exclude an inner and outer buffer.
#' 
#' For large areas, impact and candidate control pixels can be randomly sampled from the population of all impact/control pixels, using the `sample_impact` and `sample_control` arguments, respectively. 
#' This can be useful to reduce computational cost of the subsequent matching analysis.
#'  
#' @import terra
#' 
#' @export
#' 
#' @param impact SpatVector. Vector representation of the impact sites
#' @param resolution numeric. Spatial resolution of the output SpatRaster, in units of `crs`
#' @param crs Coordinate Reference System in PROJ.4, WKT or authority:code notation. Defaults to the UTM zone of center coordinates of `impact`
#' @param control_from_buffer numeric vector of 1, 2, or 4 elements. Indicates by how much (in units of `crs`) the spatial extent around the impact sites should be enlarged on each side. 
#' @param control_from_include SpatVector. Indicates area to include in candidate control selection
#' @param control_exclude SpatVector. Indicates area to exclude from candidate control selection
#' @param exclude_impact_buffer numeric vector of length 1 or 2. Indicates buffer around `impact`, in units of `crs`, to exclude as impact and/or control. See Details.
#' @param round_coords logical or integer. Should the coordinates of the output SpatRaster be rounded.
#' If TRUE, coordinates are rounded to the nearest integer; a positive numeric rounds to the corresponding decimal,
#' a negative integer rounds to the corresponding power of 10 (e.g., `round_coords=-2` rounds to the nearest 100).
#' @param sample_control numeric. A positive integer giving the number of control units to choose, or a value between 0 and 1 giving the fraction of control units to choose. 
#' @param sample_impact numeric. Same as `sample_control`, but for impact units.
#' 
#' @returns a SpatRaster in which impact pixels have value 1, candidate control pixels have value 0, 
#' and pixels excluded as impact or control have value NA
#'
create_control_candidates <- function(impact,
                                      resolution,
                                      crs,
                                      control_from_buffer=NULL,
                                      control_from_include=NULL,
                                      #control_from_bbox=NULL, #not implemented
                                      control_exclude=NULL,
                                      exclude_impact_buffer=NULL,
                                      sample_control=NULL,
                                      sample_impact=NULL,
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
  
  # Subsample of candidate control / impact units
  if(!is.null(sample_control)){
    rast_samp <- rast_out
    v <- cells(rast_samp, 0)[[1]]
    
    #define sample size if only fraction is provided
    if(!is.numeric(sample_control) | sample_control<=0) stop('"sample_control" must be a positive number')
    if(1>sample_control & sample_control>0) n_sample <- round(length(v)*sample_control) else n_sample <- sample_control

    if (n_sample<=length(v)){
      v.samp <- sample(v, n_sample)
      v.mask <- v[!v%in%v.samp]
      rast_samp[v.mask] <- NA
      rast_out <- rast_samp
    } else {
      warning('"sample_control" is larger than available candidate control units - no subsampling performed')
    }
  }
  
  if(!is.null(sample_impact)){
    rast_samp <- rast_out
    v <- cells(rast_samp, 1)[[1]]
    
    #define sample size if only fraction is provided
    if(!is.numeric(sample_impact) | sample_impact<=0) stop('"sample_impact" must be a positive number')
    if(1>sample_impact & sample_impact>0) n_sample <- round(length(v)*sample_impact) else n_sample <- sample_impact
    
    if (n_sample<=length(v)){
      v.samp <- sample(v, n_sample)
      v.mask <- v[!v%in%v.samp]
      rast_samp[v.mask] <- NA
      rast_out <- rast_samp
    } else {
      warning('"sample_impact" is larger than available candidate control units - no subsampling performed')
    }
  }

  

  return(rast_out)
}
  
  
