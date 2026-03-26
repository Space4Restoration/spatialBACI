#' Wildlife circles data
#' 
#' Simplified subset of the Finnish wildlife triangles dataset. 
#' 
#' @docType data
#' @usage data(wildlife_circles)
#' 
#' @import terra 
#' @format A SpatVector object with 671 polygon geometries and 2 attributes
#' \describe{
#'   \item{ID}{Numeric of unique ID of the polygon gemetries (1--671)}
#'   \item{PA}{Numeric indicating whether polygon is within WDPA protected area (1) or not (0)}
#' }
#' 
#' @keywords datasets
#' 
#' @references \href{https://cdnsciencepub.com/doi/10.1139/cjfr-2015-0454}{Helle et al. (2016)}, \href{https://www.nature.com/articles/s41467-020-16792-7}{Terraube et el. (2020)}  
#' @source Subset of 610 control sites and 61 impact site available in the complementary data to \href{https://www.nature.com/articles/s41467-020-16792-7}{Terraube et el. (2020)}.
#' 
#' @examples 
#' data(wildlife_circles)
#' wildlife_circles <- terra::unwrap(wildlife_circles)
#' terra::plot(wildlife_circles, "PA")
#' 
"wildlife_circles"
