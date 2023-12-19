#' Extract data for matches
#' 
#' Extract the "before" and "after", or "effect", values for the matching pairs
#' 
#' @export
#' 
#' @param matches output of \code{matchCI}
#' @param effect SpatRaster of effect values 
#' @param before SpatRaster of before values
#' @param after SpatRaster of after values
#' 
#' @seealso [matchCI()]
#' 
extractMatches <- function(matches, effect, before, after){
  #Need a better name for this function
  if(!missing(effect)){
    names(effect) <- "effect"
  } else if(!missing(before) & !missing(after)){
    effect <- rast(list(before=before, after=after))
  } else {
    stop('Either "effect", or both "before" and "after" must be provided')
  }
  
  matched_data <- cbind(matches,
                        terra::extract(effect, as.matrix(matches[,c("x","y")])))
}