#' Baviaanskloof revegetation data
#' 
#' Data on large scale planting in Baviaanskloof, South Africa. 
#' 
#' Data used and described in detail in del Río-Mena et al.
#' 
#' @docType data
#' @usage data(baviaanskloof)
#' 
#' @importFrom terra unwrap
#' @importFrom terra plot
#' @format A SpatVector object
#' 
#' @keywords datasets
#' 
#' @references del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A., 2021. Long-term assessment of ecosystem services at ecological restoration sites using Landsat time series. PLOS ONE 16, e0243020. <https://doi.org/10.1371/journal.pone.0243020>
#' 
#' @examples
#' data(baviaanskloof)
#' baviaanskloof <- unwrap(baviaanskloof)
#' plot(baviaanskloof, "Planting_date")
#' 
"baviaanskloof"