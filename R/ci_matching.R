#' Control-impact matching
#' 
#' Match control units to impact units
#' 
#' This function uses the provided matching layers to match the provided candidate and impact units 
#' using the MatchIt package and the matching options provided therein.
#' 
#' @export matchCI
#' @import data.table
#' @import MatchIt
#' @import terra
#' @param cands Control-Impact candidates - SpatRaster - 0=control, 1=impact, NA=exclude
#' @param matchlyrs multilayer SpatRaster (same geometry as CI_cand) of matching variables
#' @param eval should matching be evaluated, return NULL if matching is rejected
#' @param cols columns to be returned in the data table
#' @param ... additional inputs to matchit function
#' @returns data table with matched control-impact units
matchCI <- function(cands, matchlyrs, eval=FALSE, cols=c("subclass", "x", "y", "treatment"), ...){

  #TODO
  # For now only for raster, vector/tabular to do
  # subset of potential control (/impact) pixels before matching for large areas?
  # check if matchlyrs has names, add default names (e.g. 1:n) if not

  #Stop if geometries do not match
  if(!compareGeom(cands, matchlyrs)) stop("Geometries do not match")
  
  #Prepare data.table for matching
  names(cands) <- "treatment"
  dt <- cbind(as.data.table(cands, na.rm=FALSE, xy=TRUE),
              as.data.table(matchlyrs, na.rm=FALSE))
  dt <- na.omit(dt)

  #Define matching formula
  frm <- as.formula(paste0("treatment ~ ", paste0(names(dt)[-c(1:3)], collapse=" + ")))
  
  #Run MatchIt
  m.out <- matchit(frm, dt, ...)

  #Interactive evaluation of matching
  if(isTRUE(eval)){
    print(m.out)
    readline(prompt = "Press <Enter> to continue.")
    print(summary(m.out, un = TRUE))
    readline(prompt = "Press <Enter> to continue.")
    plot(m.out, type = "jitter", interactive = FALSE)
    readline(prompt = "Press <Enter> to continue.")
    # plot(m.out, type = "density")
    plot(summary(m.out))
    
    accept <- readline(prompt = "Accept matching (Y/N)? ")
    if(toupper(substr(accept,1,1))=="N") return(NULL)
  }

  #Extract the matched dataset
  m.data <- get_matches(m.out)[cols] |> as.data.table()
  
  return(m.data)
}


#' Matching candidates
#' 
#' Identifies control-impact candidates for matching.
#' 
#' This function creates a raster object in which candidate impact pixels are labeled as 1,
#' candidate control pixels labeled as 0, and excluded pixels as NA.
#' 
#' @export matchCandidates
#' @import terra
#' @param x SpatVector: polygon of "impact" area
#' @param y SpatRaster: reference geometry
#' @param excludeBufferIn Numeric: Buffer (in m) inside polygon to be excluded
#' @param excludeBufferOut Numeric: Buffer (in m) outside polygon to be excluded
#' @param excludeOther SpatVector: other areas to be excluded
#' 
#' @returns SpatRaster object with candidate impact, control and excluded pixels 
#' 
matchCandidates <- function(x, y, 
                            excludeBufferIn=0,
                            excludeBufferOut=0,
                            excludeOther=NULL){
  #TODO:  
  # For now only works with vector impact and raster reference, other tabular/vector/raster combinations still to be implemented
  
  #Project x to geometry of y
  x <- project(x,y)
  #Rasterize x
  x_ras <- rasterize(x,y, background=0)
  
  #Define inner and outer buffers
  if(excludeBufferIn>0 | excludeBufferOut>0){
    buf_o <- buffer(x, width=excludeBufferOut) |> 
      rasterize(y, background=0)
    buf_i <- buffer(x, width=-excludeBufferIn) |> 
      rasterize(y, background=0)
    buf_c <- buf_o-buf_i
    x_ras <- mask(x_ras, buf_c, maskvalues=1, updatevalue=NA)
  }
  #Exclude other areas
  if(!is.null(excludeOther)){
    exc <- project(excludeOther, y) |>
      crop(y) |>
      rasterize(y, background=0)
    x_ras <- mask(x_ras, exc, maskvalues=1, updatevalue=NA)
    
  }
  return(x_ras)
}
