#Author: Jasper Van doninck
#Date: Nevember 2023

matchCI <- function(cands, matchlyrs, eval=FALSE, ...){
  
  ##  Match control-impact pairs
  ##
  ##  For now only raster, vector still to do
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
  #m.data <- match.data(m.out2)
  m.data <- get_matches(m.out)[c("subclass", "x", "y", "treatment")] %>%
    as.data.table()
  
  return(m.data)
}


