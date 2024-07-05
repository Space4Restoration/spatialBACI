#' EO mask for data cube
#' 
#' Create mask from Landsat QA layer or Sentinel-2 SCL to be used in gdalcubes::raster_cube
#' 
#' The image mask is specified by the list elements provided in the maskOptions argument. 
#' The metadata layer from which the mask is derived should be specified as maskOptions$maskLyr. For now, mask derived from "qa_pixel" (for Landsat) and "SCL" (for Sentinel-2) are implemented.
#' Clouds, cloud shadow and pixels adjacent to clouds are always masked (when these fields are specified in the masking layer). 
#' Snow and water can be masked by setting maskOptions$maskSnow=TRUE and maskOptions$maskWater=TRUE, respectively. By default, these are not masked
#' 
#' @export
#' @importFrom gdalcubes image_mask
#' 
#' @param maskOptions named list. Masking options. See details.
#' 
#' @returns an object of class "image_mask"

eo_mask.cube <- function(maskOptions=list(maskLyr=NULL)){
  
  #Default values for maskSnow and maskWater
  if(is.null(maskOptions$maskSnow)) maskOptions$maskSnow <- FALSE
  if(is.null(maskOptions$maskWater)) maskOptions$maskWater <- FALSE

  if(isTRUE(maskOptions$maskLyr == "qa_pixel")){
    bits <- 0:7
    mskValues <- bitmask_values(8, snow=maskOptions$maskSnow, water=maskOptions$maskWater)
  } else if (isTRUE(maskOptions$maskLyr == "SCL")){
    bits <- NULL
    mskValues <- c(0,1,2,3,8,9,10)
    if(maskOptions$maskWater) mskValues <- c(mskValues,6)
    if(maskOptions$maskSnow) mskValues <- c(mskValues,11)
  } else {
    #warning("unrecognized mask layer name")
    return(NULL)
  }
  msk <- gdalcubes::image_mask(maskOptions$maskLyr, bits=bits, values=mskValues)
  return(msk)
}
  

#' EO mask for spatRaster
#' 
#' Create mask from Landsat QA layer or Sentinel-2 SCL terra::spatRaster objects
#' 
#' Clouds, cloud shadow and pixels adjacent to clouds are always masked (when these fields are specified in the masking layer). 
#' Snow and water can be masked by setting maskOptions$maskSnow=TRUE and maskOptions$maskWater=TRUE, respectively. By default, these are not masked.
#' 
#' @export
#' @importFrom terra app classify subset
#' 
#' @param r a spatRaster object containing a layer names "qa_pixel" (for Landsat mask) or "SCL" (for Sentinel-2 mask)
#' @param maskOptions list. see Details.
#' 
#' @returns a spatRaster mask with NA for values to be masked and 0 for other values
#' 
eo_mask.rast <- function(r, maskOptions=list(NULL)){
  
  maskLyrName <- names(r)[names(r) %in% c("qa_pixel", "SCL")]
  maskLyr <- terra::subset(r,maskLyrName)
  
  if(maskLyrName=="qa_pixel"){
    #Bits: c("Fill", "Dilated Cloud", "Cirrus", "Cloud", "Cloud Shadow", "Snow", "Clear", "Water")
    whichBits <- seq(1:5) #always mask c"Fill", "Dilated Cloud", "Cirrus", "Cloud", "Cloud Shadow"
    if(isTRUE(maskOptions$maskSnow)) whichBits <- c(whichBits, 6)
    if(isTRUE(maskOptions$maskWater)) whichBits <- c(whichBits, 8)
    
    numToBinary <- function(nums, nBits){
      sapply(nums, function(x){as.integer(intToBits(x)[1:nBits])})
    }
    maskBits <- terra::app(maskLyr, numToBinary, nBits=max(whichBits))
    outMask <- terra::app(terra::subset(maskBits, whichBits), sum) %>%
      classify(matrix(data=c(0,1,0,1,Inf, NA), byrow=TRUE, ncol=3), right=FALSE)
    
  } else if(maskLyrName=='SCL'){
    #Apply mask based on "scene classification map" layer
    mskValues <- c(0,1,2,3,8,9,10)
    if(isTRUE(maskOptions$maskWater)) mskValues <- c(mskValues,6)
    if(isTRUE(maskOptions$maskSnow)) mskValues <- c(mskValues,11)
    
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


