#' Control-impact matching
#' 
#' Match control units to impact units
#' 
#' This function uses the provided matching layers to match the provided candidate and impact units 
#' using the MatchIt package and the matching options provided therein.
#' 
#' If cands is a spatRaster, impact pixels should be labelled 1, candidate control pixels labelled 0, and pixels to be excluded labelled NA.
#' In that case, matching variables in \code{matchvars} are to be provided as a single (multilayer) terra spatRaster object, gdalcubes data cube object, character pathname (which will be assumed to refer to a raster imagee), or as a list of these classes.
#' If cands is a spatVector, attributes should include "ID", "treatment" (1 for impact sites and 0 for candidate control sites). All remaining columns will be interpreted as 
#' 
#' @export matchCI
#' 
#' @importFrom data.table as.data.table
#' @import MatchIt
#' @import terra
#' 
#' @param cands spatRaster or spatVector object. See Details.
#' @param matchlyrs multilayer SpatRaster (same geometry as CI_cand) of matching variables
#' @param eval should matching be evaluated, return NULL if matching is rejected
#' @param cols columns to be returned in the data table
#' @param ... additional inputs to \code{matchit} function
#' 
#' @returns data table with matched control-impact units. Column names are "ID", "treatment", and "subclass". 
#' 
matchCI <- function(cands, matchVars, 
                    ratio=1, replace=FALSE, method="nearest", distance="glm", link="logit", 
                    eval=FALSE, asmd_warn=NULL, asmd_error=NULL, ...){

  #TODO?
  # subset of potential control (/impact) pixels before matching for large areas?
  # check if matchlyrs has names, add default names (e.g. 1:n) if not
  
  if(is.SpatRaster(cands)){ #Candidate controls from raster
    
    matchVars <- collate_matching_layers(cands, matchVars)
    
    #Prepare data.table for matching
    names(cands) <- "treatment"
    matchVars_names <- names(matchVars)
    #TODO: Check that layers of matching variables have (unique) names, fix if not. 
    dt <- cbind(as.data.table(cands, na.rm=FALSE, cells=TRUE, xy=TRUE),
                as.data.table(matchVars, na.rm=FALSE))
    dt <- na.omit(dt)
    
    names_out <- c("cell", "x", "y", "subclass", "treatment")
    
  } else if (is.SpatVector(cands)){ #Candidate controls from vector
   
    #Remove any columns in cands that are not "ID" or "treatment"
    cands <- cands[c("ID", "treatment")]
    
    #combine the matching layers
    matchVars <- collate_matching_layers(cands, matchVars)
    matchVars_names <- names(matchVars) |> setdiff(c("ID", "treatment"))
    
    dt <- as.data.table(matchVars)
    names_out <- c("ID", "subclass", "treatment")
    
  } else {
    stop("SpatRaster of SpatVector expected")
  }
  
  #Define matching formula
  frm <- as.formula(paste0("treatment ~ ", paste0(matchVars_names, collapse=" + ")))
  
  #Run MatchIt
  m.out <- MatchIt::matchit(formula=frm, data=dt, 
                            ratio=ratio, replace=replace, method=method, distance=distance, link=link, 
                            ...)
  
  #Interactive evaluation of matching
  if(isTRUE(eval)){
    print(m.out)
    readline(prompt = "Press <Enter> to continue.")
    print(summary(m.out, un = TRUE))
    readline(prompt = "Press <Enter> to continue.")
    plot(m.out, type = "jitter", interactive = FALSE)
    readline(prompt = "Press <Enter> to continue.")
    # plot(m.out, type = "density")
    plot(summary(m.out), xlim=c(0,1))
    
    accept <- readline(prompt = "Accept matching (Y/N)? ")
    if(toupper(substr(accept,1,1))=="N") return(NULL)
  }
  
  asmd_matched <- summary(m.out, un = TRUE)$sum.matched[,"Std. Mean Diff."] |> abs()
  if(!(isTRUE(is.null(asmd_warn)) | isTRUE(is.na(asmd_warn)))){
    if(!is.numeric(asmd_warn)) stop("asmd_warn must be numeric")
    asmd_ex <- asmd_matched>asmd_warn
    if(sum(asmd_ex)>1) warning(paste0("ASMD threshold of ", asmd_warn, " exceeded for matching variables: ", paste0(names(asmd_matched[asmd_ex]), collapse=", "))) 
  }
  if(!(isTRUE(is.null(asmd_error)) | isTRUE(is.na(asmd_error)))){
    if(!is.numeric(asmd_error)) stop("asmd_warn must be numeric")
    asmd_ex <- asmd_matched>asmd_error
    if(sum(asmd_ex)>1) stop(paste0("ASMD threshold of ", asmd_error, " exceeded for matching variables: ", paste0(names(asmd_matched[asmd_ex]), collapse=", "))) 
  }
  
  #Extract the matched dataset
  m.data <- MatchIt::get_matches(m.out)[names_out] |> as.data.table()
  return(m.data)
}

#' Collate matching layers
#' 
#' Process different matching layers in compatible format
#' 
#' All matching layers should be provided as additional arguments (\code{...}). Matching layers can be provided as spatRaster, data cube, or a character (which will be interpreted as a readable file name). 
#' Alternatively, \code{...} can be a list of matching layers (SpatRaster, data cube or character)
#' 
#' @importFrom terra compareGeom rast project
#' 
#' @export
#' 
#' @param x SpatRaster providing spatial reference
#' @param ... Matching layers. See Details.
#' 
#' 
#' @returns description
#' 
collate_matching_layers <- function(x, vars_list){
  
  if(!is.list(vars_list)) vars_list <- list(vars_list)
  
  if (is.SpatRaster(x)){
    
    rast_list <- vector(mode="list", length=length(vars_list))
    
    for(i in 1:length(vars_list)){
      inLyr <- vars_list[[i]]
      #If gdalcube -> transform to spatRaster
      if(is.cube(inLyr)){
        inLyr <- gdalcube_as_terra(inLyr)
      }
      #If character -> assume it is filename, read as spatRaster
      if(is.character(inLyr)){
        inLyr <- terra::rast(inLyr)
      }
      #If spatRaster -> project to correct geometry
      if(is.spatRaster(inLyr)){
        if(isTRUE(terra::compareGeom(x, inLyr, stopOnError=FALSE))){
          i_out <- inLyr
        } else {
          i_out <- terra::project(inLyr, x)
        }
      } else {
        warning("Unrecognized format, skipping input")
        i_out <- NULL
      }
      
      rast_list[[i]] <- i_out  
    }
    out <- terra::rast(rast_list)
    
  } else if(is.SpatVector(x)){
    
    x_out <- x
    for (i in 1:length(vars_list)){
      inLyr <- vars_list[[i]]
      if(!is.SpatVector(inLyr)) stop("Matching variable should be a SpatRaster layer")
      if("ID" %in% names(inLyr)){
        x_out <- merge(x_out, inLyr)
      } else {
        x_out <- cbind(x_out,inLyr)
      }
    }
    
    out <- x_out
  } else {
    stop("SpatRaster or SpatVector input expected.")
  }
  return(out)
}


#'   #' Matching candidates - DEPRECATED: replaced by create_control_candidates
#'   #' 
#'   #' Identifies control-impact candidates for matching.
#'   #' 
#'   #' This function creates a raster object in which candidate impact pixels are labeled as 1,
#'   #' candidate control pixels labeled as 0, and excluded pixels as NA.
#'   #' 
#'   #' 
#'   #' @export matchCandidates
#'   #' @import terra
#'   #' @param x SpatVector: polygon of "impact" area
#'   #' @param y SpatRaster: reference geometry
#'   #' @param excludeBufferIn Numeric: Buffer (in m) inside polygon to be excluded
#'   #' @param excludeBufferOut Numeric: Buffer (in m) outside polygon to be excluded
#'   #' @param excludeOther SpatVector: other areas to be excluded
#'   #' 
#'   #' @returns SpatRaster object with candidate impact, control and excluded pixels 
#'   #' 
#' matchCandidates <- function(x, y, 
#'                             excludeBufferIn=0,
#'                             excludeBufferOut=0,
#'                             excludeOther=NULL){
#'   #TODO:  
#'   # For now only works with vector impact and raster reference, other tabular/vector/raster combinations still to be implemented
#'   
#'   #Project x to geometry of y
#'   x <- project(x,y)
#'   #Rasterize x
#'   x_ras <- rasterize(x,y, background=0)
#'   
#'   #Define inner and outer buffers
#'   if(excludeBufferIn>0 | excludeBufferOut>0){
#'     buf_o <- buffer(x, width=excludeBufferOut) |> 
#'       rasterize(y, background=0)
#'     buf_i <- buffer(x, width=-excludeBufferIn) |> 
#'       rasterize(y, background=0)
#'     buf_c <- buf_o-buf_i
#'     x_ras <- mask(x_ras, buf_c, maskvalues=1, updatevalue=NA)
#'   }
#'   #Exclude other areas
#'   if(!is.null(excludeOther)){
#'     exc <- project(excludeOther, y) |>
#'       crop(y) |>
#'       rasterize(y, background=0)
#'     x_ras <- mask(x_ras, exc, maskvalues=1, updatevalue=NA)
#'     
#'   }
#'   return(x_ras)
#' }
