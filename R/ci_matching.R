#Author: Jasper Van doninck
#Date: November 2023

matchCI <- function(cands, matchlyrs, eval=FALSE, cols=c("subclass", "x", "y", "treatment"), ...){
  
  ##  Match control-impact pairs
  ##
  ##  For now only raster, vector/tabular to do
  ##  
  ##  cands: Control-Impact candidates - SpatRaster - 0=control, 1=impact, NA=exclude
  ##  matchlyrs: multilayer SpatRaster (same geometry as CI_cand) of matching variables
  ##  eval: evaluate matching, return NULL if matching is rejected
  ##  ... additional inputs to matchit
  ##
  ##  TO DO: subset of potential control (/impact) pixels before matching for large areas?
  ##  TO DO: check if matchlyrs has names, add default names (e.g. 1:n) if not
  ##
  
  library(MatchIt)
  
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
  m.data <- get_matches(m.out)[cols] %>% as.data.table()
  
  return(m.data)
}

matchCandidates <- function(x, y, 
                            excludeBufferIn=0,
                            excludeBufferOut=0,
                            excludeOther=NULL){
  #
  #
  # Create the SpatRaster layer from which Impact and Control pixels will be extracted
  #
  # Arguments:
  #   x                 SpatVector: polygon of "impact" area  
  #   y                 SpatRaster: reference geometry
  #   excludeBufferIn   Numeric: Buffer (in m) inside polygon to be excluded
  #   excludeBufferOut  Numeric: Buffer (in m) outside polygon to be excluded
  #   excludeOther:     SpatVector: other areas to be excluded
  #
  # Output:
  #   SpatRaster object with candidate impact pixels labeled as 1, control pixels as 0, and excluded pixels as NA
  #
  # Notes:
  #   For now only works with vector impact and raster reference, other tabular/vector/raster combinations still to be implemented
  
  require(terra)
  
  #Project x to geometry of y
  x <- project(x,y)
  #Rasterize x
  x_ras <- rasterize(x,y, background=0)
  
  #Define inner and outer buffers
  if(excludeBufferIn>0 | excludeBufferOut>0){
    buf_o <- buffer(x, width=excludeBufferOut) %>% rasterize(y, background=0)
    buf_i <- buffer(x, width=-excludeBufferIn) %>% rasterize(y, background=0)
    buf_c <- buf_o-buf_i
    x_ras <- mask(x_ras, buf_c, maskvalues=1, updatevalue=NA)
  }
  #Exclude other areas
  if(!is.null(excludeOther)){
    exc <- project(excludeOther, y) %>%
      crop(y) %>%
      rasterize(y, background=0)
    x_ras <- mask(x_ras, exc, maskvalues=1, updatevalue=NA)
    
  }
  return(x_ras)
}









