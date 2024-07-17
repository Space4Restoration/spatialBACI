

#' Read SoilGrids layer as spatRaster
#' 
#' @export
#' 
#' @importFrom terra rast
#' 
#' @param product SoilGrids layer to be read as spatRaster, defaults to "MostProbable"
#' 
#' @references SoilGrids ISRIC CC-BY 4.0 License
#' 

SoilGrids2rast <- function(product="MostProbable"){
  
  SoilGrids_url_base <- "https://files.isric.org/soilgrids/latest/data"
  SoilGrids_product <- "wrb"
  SoilGrids_lyr <- paste0(product,".vrt")
  
  SoilGrids_urls <- paste(SoilGrids_url_base, SoilGrids_product, SoilGrids_lyr, sep="/")
  
  SoilGrids_rast <- rast(lapply(SoilGrids_urls, rast, vsi=TRUE))
  return(SoilGrids_rast)
}


#' Extract most probable soil type from SoilGrids
#' 
#' Read most probable soil time from ISRIC SoilGrids data as SpatRaster
#' 
#' This is a quick draft that should be further developed based on the soilDB package
#' 
#' @export
#' 
#' @import terra
#' 
#' @param x reference SpatRaster
#' 
#' @references SoilGrids ISRIC CC-BY 4.0 License
#' 
#' 
setGeneric("SoilGrids_soilgroup", function(x){
  standardGeneric("SoilGrids_soilgroup")
})

setMethod("SoilGrids_soilgroup", signature="SpatRaster",
          function(x){
            SoilGrids_rast <- SoilGrids2rast()
            SoilGrids_x <- terra::crop(SoilGrids_rast, project(ext(x), crs(x), crs(SoilGrids_rast))) |>
              terra::project(x, method="near")
            return(SoilGrids_x)
          }
)




