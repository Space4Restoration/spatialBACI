#Author: Jasper Van doninck
#Date: July 2023

lulc <- function(x, year, source='io-lulc-9-class', xy=TRUE, authOpt=list()){
  
  library(terra)
  if (class(x)=="SpatRaster"){ 
    extent <- terra::project(ext(x), from=crs(x), to=crs("epsg:4326"))
  } else if (class(x)=="SpatExtent"){
    extent <- x
  } else if(class(x)=="numeric"){
    extent <- ext(x, xy=xy)
  }
  bbox <- extent[c(1,3,2,4)]

  if(source=='io-lulc-9-class'){
    if(year<2017){
      warning("Land cover product currently has data 2017-2022. 2017 selected.")
      year <- 2017 
    }
    if(year>2022){
      warning("Land cover product currently has data 2017-2022. 2022 selected.")
      year <- 2022 
    }
    
    endpoint <- 'https://planetarycomputer.microsoft.com/api/stac/v1'
    items <- stac(endpoint) %>%
      stac_search(collections=source, datetime=paste0(year,"-07-01"), bbox=extent[c(1,3,2,4)]) %>%
      get_request()
    items <- do.call(sign_featureCollection, c(list(featureCollection=items, endpoint=endpoint), authOpt))
    
    if(length(items$features)==1) r <- assets2vrt(items, "data") else r <- assets2rast(items$features[[1]], "data")
    names(r) <- source

    cls <- as.data.frame(do.call(rbind, lapply(feature$assets$data$`file:values`, unlist)))
    cls[,1] <- as.numeric(cls[,1])
    names(cls) <- c("value", "landcover")
    levels(r) <- cls
  }
  if (class(x)=="SpatRaster"){ 
    r <- project(r, x, method="near")
  }
  return(r)
}


