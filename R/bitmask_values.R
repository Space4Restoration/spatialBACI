
#' EO mask for gdalcubes
#' 
#' Create mask for gdalcube from Landsat QA layer or Sentinel-2 SCL
#' 
#' Clouds+cloud shadows always masked. Snow and water defined in argument maskOptions (default=FALSE, for both snow and water)
#' Possible list names: maskLyr, maskSnow, maskWater
#' 
#' @export
#' 
#' @importFrom gdalcubes image_mask
#' 
#' @param maskOptions named list. Masking options. See details.
#' 
#'  

#change name to cubeMask
eo_image_mask <- function(maskOptions=list(maskLyr=NULL)){
  
  #Default values for maskSnow and maskWater
  if(is.null(maskOptions$maskSnow)) maskOptions$maskSnow <- FALSE
  if(is.null(maskOptions$maskWater)) maskOptions$maskWater <- FALSE

  if(isTRUE(maskOptions$maskLyr == "qa_pixel")){
    bits <- 0:7
    mskValues <- bitmask_values(8, snow=maskOptions$maskSnow, water=maskOptions$maskWater)
  } else if (isTRUE(maskOptions$maskLYr == "SCL")){
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

