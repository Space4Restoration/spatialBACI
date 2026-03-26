#' Baviaanskloof revegetation data
#' 
#' Data on large scale planting in Baviaanskloof, South Africa. 
#' 
#' Data used and described in detail in del Río-Mena et al. (2021).
#' 
#' @docType data
#' @usage data(baviaanskloof)
#' 
#' @import terra
#' @format A SpatVector object with 118 geometries and 2 attributes
#' \describe{
#'   \item{Lifestock_exclusion}{Integer indicating if lifestock exclusion was applied (1) or not (0)}
#'   \item{Planting_date}{Integer indicating year of revegetation intervention (2005--2015)}
#' }
#' 
#' @keywords datasets
#' 
#' @references del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A., 2021. Long-term assessment of ecosystem services at ecological restoration sites using Landsat time series. PLOS ONE 16, e0243020. \url{https://doi.org/10.1371/journal.pone.0243020}
#' 
#' @source Dataset provided by T. del Río-Mena and anonymized, the full dataset supporting del Río-Mena et al. (2021) is provided in \url{https://doi.org/10.17026/dans-zrc-hmz4}.
#' 
#' @examples
#' data(baviaanskloof)
#' baviaanskloof <- terra::unwrap(baviaanskloof)
#' terra::plot(baviaanskloof, "Planting_date")
#' 
"baviaanskloof"
