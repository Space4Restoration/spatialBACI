# Author: Jasper Van doninck
# Date: July 2023

#  Climatologies at high resolution for the earth’s land surface areas  (https://chelsa-climate.org)
#  Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, P., Kessler, M. (2017): Climatologies at high resolution for the Earth land surface areas. Scientific Data. 4 170122. https://doi.org/10.1038/sdata.2017.122


chelsa2rast <- function(product="climatologies", period="1981-2010", data="bio", layers=paste0("bio",c(1:19)), version="2.1"){
  library(terra)
  chelsa_version_dir <- paste0("chelsa_V",substr(version, 1,1))
  chelsa_version_lyr <- paste0("V.",version,".tif")
  chelsa_data_dir <- paste("https://os.zhdk.cloud.switch.ch/envicloud/chelsa",
                           chelsa_version_dir,
                           "GLOBAL",
                           product,
                           period,
                           data, 
                           sep="/")
  chelsa_data_lyrs <- paste("CHELSA",
                            layers,
                            period, 
                            chelsa_version_lyr,
                            sep="_")
  chelsa_urls <- paste(chelsa_data_dir, chelsa_data_lyrs, sep="/")
  chelsa_rast <- rast(lapply(chelsa_urls, rast, vsi=TRUE))
  return(chelsa_rast)
}


setGeneric("chelsa", function(x, ...) standardGeneric("chelsa"))

setMethod("gfc", signature="SpatExtent",
          function(x, CRS, product="climatologies", period="1981-2010", data="bio", layers=paste0("bio",c(1:19)), version="2.1", ...){
            chelsa_rast <- chelsa2rast(product=product, period=period, data=data, layers=layers, version=version)
            
            if(!missing(CRS)) x <- terra:project(x, CRS, crs(chelsa_rast))
            chelsa_x <- terra::crop(chelsa_rast, x, ...)
            return(chelsa_x)
          }
)

setMethod("gfc", signature="SpatRaster",
          function(x, product="climatologies", period="1981-2010", data="bio", layers=paste0("bio",c(1:19)), version="2.1", ...){
            chelsa_rast <- chelsa2rast(product=product, period=period, data=data, layers=layers, version=version)
            
            chelsa_x <- terra::project(chelsa_rast, x, ...)
            return(x)
          }
)

setMethod("gfc", signature="SpatRaster",
          function(x, product="climatologies", period="1981-2010", data="bio", layers=paste0("bio",c(1:19)), version="2.1", 
                   fun=mean, bind=TRUE, na.rm=TRUE, ...){
            chelsa_rast <- chelsa2rast(product=product, period=period, data=data, layers=layers, version=version)
            
            chelsa_x <- terra::extract(chelsa_rast, terra::project(x, chelsa_rast), fun=fun, bind=bind, na.rm=ra.rm, ...)
            return(chelsa_x)
          }
)

