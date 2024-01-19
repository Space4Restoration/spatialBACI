#' Baviaanskloof roads
#' 
#' Subset of the [OpenStreetMap](https://www.openstreetmap.org/copyright) roads dataset for the area around Baviaankloof. Downloaded from download.geofabrik.de.
#'  
#' @docType data
#' @usage data(baviaanskloof_roads)
#' 
#' @importFrom terra unwrap
#' @importFrom terra plot
#' @format A SpatVector object
#' 
#' @keywords datasets
#' 
#' @examples
#' data(baviaanskloof_roads)
#' baviaanskloof <- unwrap(baviaanskloof_roads)
#' plot(baviaanskloof_roads)
#' 
"baviaanskloof_roads"