#' Control-impact matching
#' 
#' @description
#' Match control units to impact/treatment units for vector or raster data. Wrapper for the [MatchIt::matchit()] function and the matching options provided therein.
#' 
#' @details
#' The function returns a names list.
#' 
#' The list element `model` is a `matchit` object returned by the [MatchIt::matchit()] function. This object can be used to assess the quality of the matching, e.g., through assessing the covariance balance in [evaluate_matching()].
#' The list element `matches` is a data.table containing the matched control-impact pairs. 
#' Column names of this data.table are those defined by the `colname.id`, `colname.treatment` parameters, and "subclass". 
#' The list element `spat.ref` is copied from the same list element in `matching_input`.
#' 
#' @export matchCI
#' 
#' @importFrom data.table as.data.table is.data.table
#' @import MatchIt
#' @import terra
#' 
#' @param matching_input SpatVector or data.table. Output of [collate_matching_layers()].
#' @param colname.id character. Name of the column containing the unique ID of the spatial unit of analysis. Defaults to "cell" if `matching_input` is a data.table (representing raster data).
#' @param colname.treatment character. Name of the column containing the treatment variable. Defaults to "treatment" if `matching_input` is a data.table (representing raster data).
#' @param colnames.ignore character vector. Name of columns (other than `colname.id` and `colname.treatment`) in `matching_input` to be ignored as matching covariates.
#' @param ... additional inputs to [MatchIt::matchit()], e.g., `ratio`, `replace`, `method`, `distance`, or `link`.
#' 
#' @returns named list with list elements `model`, `matches`, and `spat.ref`. See Details.  
#' 
matchCI <- function(matching_input, colname.id, colname.treatment, colnames.ignore=NULL, ...){
  
  spat.ref <- matching_input$spat.ref
  matching_input_dt <- matching_input$data
  colnames_input <- colnames(matching_input_dt)
  
  if(is.SpatVector(spat.ref)){
    if(missing(colname.id)) stop('matchCI - Argument "colname.id" required for SpatVector input')
    if(missing(colname.treatment)) stop('matchCI - Argument "colname.treatment" required for SpatVector input')
  } else if (is.SpatRaster(spat.ref)){
    if(missing(colname.treatment)){
      if("treatment" %in% colnames_input){
        colname.treatment <- "treatment"
      } else {
        stop('matchCI - argument "colname.treatment" expected')
      }
    }
    if(missing(colname.id)){
      if("cell" %in% colnames_input){
        colname.id <- "cell"
      } else {
        stop('matchCI - argument "colname.id" expected')
      }
    }
    
  } else {
    stop('test_multicollinearity: "matching_input$spat.ref" must be of class Spatvector or SpatRaster')
  }
  
  #Define matching formula
  matchvar_names <- colnames_input[!colnames_input %in% c(colname.treatment, colname.id, colnames.ignore)]
  frm <- as.formula(paste0(colname.treatment," ~ ", paste0(matchvar_names, collapse=" + ")))
  
  #Run MatchIt
  matching_output <- MatchIt::matchit(formula=frm, data=matching_input_dt, 
                                      ...)
  
  ##Extract the matched dataset
  names_out <- c(colname.id, colname.treatment, "subclass")
  matching_matches <- MatchIt::get_matches(matching_output, id="matching.id", data=matching_input_dt)[names_out] |> as.data.table()
  
  out <- list(model=matching_output, 
              matches=matching_matches,
              spat.ref=spat.ref)
  return(out)

}

