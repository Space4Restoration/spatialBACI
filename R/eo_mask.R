####  FUNCTIONS RELATED TO MASKING OF EO DATA
####    For now, focus on Landsat/Sentinel2


#' EO mask for data cube
#' 
#' Create mask from Landsat QA layer or Sentinel-2 SCL that can be used in gdalcubes::raster_cube
#' 
#' The metadata layer from which the mask is derived should be specified in \code{maskLyr}. For now, mask derived from "qa_pixel" (for Landsat) and "SCL" (for Sentinel-2) are implemented.
#' Clouds, cloud shadow and pixels adjacent to clouds are always masked (when these fields are specified in the masking layer). 
#' Snow and water can be masked by setting the respective parameters, by default these are not masked
#' 
#' @export
#' @importFrom gdalcubes image_mask
#' 
#' @param maskOptions named list. Masking options. See details.
#' @param maskLyr description
#' @param maskSnow logical. Defines whether snow should be masked.
#' @param maskWater logical. Defines whether water should be masked.
#' 
#' @returns an object of class "image_mask"
#' 
#' @examples
#' # mask from Landsat "qa_pixel" layer
#' eo_mask.cube("qa_pixel")
#' # mask from Sentinel-2 "SCL" layer
#' eo_mask.cube("SCL", maskSnow=TRUE, maskWater=TRUE)
#' 

eo_mask.cube <- function(maskLyr,
                         maskSnow=FALSE,
                         maskWater=FALSE){
  
  if(missing(maskLyr)) return(NULL)
  maskSnow <- as.logical(maskSnow)
  maskWater <- as.logical(maskWater)

  if(isTRUE(maskLyr == "qa_pixel")){
    bits <- 0:7
    mskValues <- bitmask_values(8, snow=maskSnow, water=maskWater)
  } else if (isTRUE(maskLyr == "SCL")){
    bits <- NULL
    mskValues <- c(0,1,2,3,8,9,10)
    if(maskWater) mskValues <- c(mskValues,6)
    if(maskSnow) mskValues <- c(mskValues,11)
  } else {
    warning("Mask layer name is not implemented")
    return(NULL)
  }
  msk <- gdalcubes::image_mask(maskLyr, bits=bits, values=mskValues)
  return(msk)
}


#' EO mask for spatRaster
#' 
#' Create mask from Landsat QA layer or Sentinel-2 SCL terra::spatRaster objects
#' 
#' The metadata layer from which the mask is derived should be specified in \code{maskLyr}. For now, mask derived from "qa_pixel" (for Landsat) and "SCL" (for Sentinel-2) are implemented.
#' Clouds, cloud shadow and pixels adjacent to clouds are always masked (when these fields are specified in the masking layer). 
#' Snow and water can be masked by setting the respective parameters, by default these are not masked
#' 
#' @export
#' @importFrom terra app classify subset
#' 
#' @param r a spatRaster object containing a layer names "qa_pixel" (for Landsat mask) or "SCL" (for Sentinel-2 mask)
#' @param maskSnow logical. Defines whether snow should be masked.
#' @param maskWater logical. Defines whether water should be masked.
#' 
#' @returns a spatRaster mask with NA for values to be masked and 0 for other values
#' 
eo_mask.rast <- function(r, 
                         maskSnow=FALSE,
                         maskWater=FALSE){
  
  maskLyrName <- names(r)[names(r) %in% c("qa_pixel", "SCL")]
  maskLyr <- terra::subset(r,maskLyrName)
  
  if(maskLyrName=="qa_pixel"){
    #Bits: c("Fill", "Dilated Cloud", "Cirrus", "Cloud", "Cloud Shadow", "Snow", "Clear", "Water")
    whichBits <- seq(1:5) #always mask c"Fill", "Dilated Cloud", "Cirrus", "Cloud", "Cloud Shadow"
    if(isTRUE(maskSnow)) whichBits <- c(whichBits, 6)
    if(isTRUE(maskWater)) whichBits <- c(whichBits, 8)
    
    numToBinary <- function(nums, nBits){
      sapply(nums, function(x){as.integer(intToBits(x)[1:nBits])})
    }
    maskBits <- terra::app(maskLyr, numToBinary, nBits=max(whichBits))
    outMask <- terra::app(terra::subset(maskBits, whichBits), sum) %>%
      classify(matrix(data=c(0,1,0,1,Inf, NA), byrow=TRUE, ncol=3), right=FALSE)
    
  } else if(maskLyrName=='SCL'){
    #Apply mask based on "scene classification map" layer
    mskValues <- c(0,1,2,3,8,9,10)
    if(isTRUE(maskWater)) mskValues <- c(mskValues,6)
    if(isTRUE(maskSnow)) mskValues <- c(mskValues,11)
    
    classMatrix <- matrix(data=c(0:11, rep(0,12)), ncol=2)
    classMatrix[classMatrix[,1] %in% mskValues,2] <- NA
    outMask <- classify(meta, classMatrix)
  } else {
    stop("Metadata-based masks only implemented for Landsat and Sentinel-2")
  }
  return(outMask)
}


#' Landsat L2 QA bitmap values
#' 
#' Define the values of values to be masked from Landsat L2 products based on specified QA bits
#' 
#' @export
#' 
#' @param nbit description
#' @param fill description
#' @param dilCloud description
#' @param cirrus description
#' @param cloud description
#' @param cloudShadow description
#' @param snow description
#' @param water description
#' 
#' @returns integer vector
#' 
bitmask_values <- function(nBit=8, fill=TRUE, dilCloud=TRUE, cirrus=TRUE, cloud=TRUE, cloudShadow=TRUE, snow=FALSE, water=FALSE){
  
  numToBinary <- function(nums, nBits){
    sapply(nums, function(x){as.integer(intToBits(x)[1:nBits])})
  }
  bins <- numToBinary(0:(2^nBit-1), nBit)
  flagDescription <- c("Fill", "Dilated Cloud", "Cirrus", "Cloud", "Cloud Shadow", "Snow", "Clear", "Water")
  colnames(bins) <- 0:(2^nBit-1)
  rownames(bins) <- flagDescription[1:nBit]
  
  binMask <- logical(ncol(bins))
  names(binMask) <- 0:(2^nBit-1)
  
  if(fill) binMask[bins["Fill",]==1] <- TRUE
  if(dilCloud) binMask[bins["Dilated Cloud",]==1] <- TRUE
  if(cirrus) binMask[bins["Cirrus",]==1] <- TRUE
  if(cloud) binMask[bins["Cloud",]==1] <- TRUE
  if(cloudShadow) binMask[bins["Cloud Shadow",]==1] <- TRUE
  if(snow) binMask[bins["Snow",]==1] <- TRUE
  if(water) binMask[bins["Water",]==1] <- TRUE
  
  values <- which(binMask) |>
    names() |>
    as.numeric()
  
  return(values)
}


#' Retrieve mask layer name
#' 
#' This function returns the metadata layer name from which a mask can be derived for a collection at a STAC endpoint
#' 
#' Currently only implemented for collections "landsat-c2-l2" and "sentinel-2-l2a" at Planetary Computer. Returns NULL for all other values. Additional endpoints/collections will be added. 
#' 
#' @export
#' 
#' @param endpoint STAC endpoint
#' @param collection STAC collection
#' 
#' @returns character or NULL
#' 
eo_maskLyrName.stac <- function(endpoint, collection){
  metaLyr <- NULL
  if(endpoint=="https://planetarycomputer.microsoft.com/api/stac/v1"){
    if(collection=="landsat-c2-l2") metaLyr <- "qa_pixel"
    if(collection=="sentinel-2-l2a") metaLyr <- "SCL"
  }
  return(metaLyr)
}

