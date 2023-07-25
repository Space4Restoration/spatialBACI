#Author: Jasper Van doninck
#Date: July 2023

#Harmonize functions for lists of SpatRasters
harm_list_res <- function(x, target="high", ...){
  
  # Check spatial resolutions of assets
  resolutions <- sapply(x, function(y) res(y)[1])
  resLevels <- as.numeric(levels(as.factor(resolutions)))
  
  if((length(resLevels)>1) | (length(resLevels)>1 & is.numeric(target) & resLevels[1]!=target)){
    #Assets with different resolution: harmonize assets to common resolution (assuming multiples)
    # Target resolution: either specified or highest/lowest
    if(is.numeric(target)){
      targetRes <- target
    } else {
      targetRes <- switch(target,
                          low=max(resLevels),
                          high=min(resLevels))
    }
    
    fun <- function(y){
      if(res(y)[1]==targetRes){
        return(y)
      } else if(res(y)[1]>targetRes){
        #disaggregate
        return(disagg(y, fact=res(y)[1]/targetRes, method="near"))
      } else if(res(y)[1]<targetRes){
        #aggregate
        return(aggregate(y, fact=targetRes/res(y)[1], fun="mean"))
      }
    }
    x <- lapply(x, fun)
  }
  return(x)
}

#Harmonize extents
harm_list_ext <- function(x, target=NULL){
  #TODO: Implement alternatives for target. Now uses largest common extent
  if(!is.TRUE(do.call(compareGeom, c(unname(x), ext=TRUE, stopOnError=FALSE)))){
    ext_tab <- do.call(rbind, lapply(x, function(y) ext(y)[1:4]))
    target_ext <- ext(max(ext_tab[,'xmin']), min(ext_tab[,'xmax']), max(ext_tab[,'ymin']), min(ext_tab[,'ymax']))
    x <- lapply(x, crop, target_ext)
  }
  return(x)
}

#Harmonize projections
harm_list_crs <- function(x, target=NULL){
  #TODO: Alternatives for target? Now uses majority
  if(!is.TRUE(do.call(compareGeom, c(unname(x), crs=TRUE, stopOnError=FALSE)))){
    crs_values <- unlist(lapply(x, crs, proj=TRUE))
    target_crs <- names(which.max(table(crs_values)))
    target_ras <- x[[which(crs_values==target_crs)[1]]]
    
    x <- lapply(x, terra::project, target_ras)
  }
  return(x)
}
