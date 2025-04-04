#' Wildlife circles data
#' 
#' Simplified subset of the Finnish wildlife triangles dataset. 
#' 
#' @docType data
#' @usage data(wildlife_circles)
#' 
#' @importFrom terra unwrap plot
#' @format A SpatVector object
#' 
#' @keywords datasets
#' 
#' @references [Helle et al., 2016](https://cdnsciencepub.com/doi/10.1139/cjfr-2015-0454), [Terraube et el., 2020](https://www.nature.com/articles/s41467-020-16792-7) 
#' 
#' @examples
#' data(wildlife_circles)
#' wildlife_circles <- unwrap(wildlife_circles)
#' plot(wildlife_circles, "PA")
#' 
"wildlife_circles"
