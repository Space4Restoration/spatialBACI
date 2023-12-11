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

