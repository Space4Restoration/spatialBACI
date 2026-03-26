#' Before-After-Control-Impact contrast
#' 
#' @description
#' Calculate the (Before-After-)Control-Impact contrast and p-value
#' 
#' After [Meroni et al. (2017)](https://doi.org/10.1016/j.jag.2017.02.016) and [del Río-Mena et al. (2021)](https://doi.org/10.1371/journal.pone.0243020)
#' 
#' @details
#' An "effect" raster of the variable of interest can be provided instead of the pre-intervention and post-intervention variable, the effect variable takes priority if all are provided. 
#' The function returns a list with named elements `data`, containing (BA)CI contrast and p-value for each impact unit of analysis as a data.table, and `spat`, containing the same data as a spatial object. 
#' 
#' @export
#' 
#' @importFrom data.table data.table as.data.table is.data.table
#' @import terra
#' 
#' @param ci_matches list. Control-impact pairs and spatial reference, output of [matchCI()]
#' @param before SpatRaster or data.cube of pre-intervention observable of interest
#' @param after SpatRaster or data.cube of post-intervention observable of interest
#' @param effect SpatRaster or data.cube of effect observable of interest. See Details.
#' @param colname.id character. Name of the column containing the unique ID of the spatial unit of analysis in `ci_matches`. Defaults to "cell" for SpatRaster input in `ci_matches`.
#' @param colname.treatment character. Name of the column containing the treatment variable in `ci_matches`. Defaults to "treatment" for SpatRaster input in `ci_matches`.
#' @param colname.subclass character. Name of the column containing the subclass assignemt in `ci_matches`. Defaults to "subclass".
#' @param fun function to summarize extracted data by line or polygon geometries. See [terra::extract()].
#' @param method character. Method for extracting values for point geometries. See [terra::extract()].
#' @param ... additional arguments to `fun`.
#' 
#' @returns list (See Details)
#' 
#' @references Meroni, M., Schucknecht, A., Fasbender, D., Rembold, F., Fava, F., Mauclaire, M., Goffner, D., Di Lucchio, L.M., Leonardi, U., 2017. Remote sensing monitoring of land restoration interventions in semi-arid environments with a before–after control-impact statistical design. International Journal of Applied Earth Observation and Geoinformation 59, 42–52.
#' @references del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A., 2021. Long-term assessment of ecosystem services at ecological restoration sites using Landsat time series. PLOS ONE 16, e0243020.
#' 


BACI_contrast <- function(ci_matches, before, after, effect, colname.id, colname.treatment, colname.subclass="subclass", fun, method="simple", ...){
  
  spat.ref <- ci_matches$spat.ref
  matches <- ci_matches$matches
  if(!is.data.table(matches)) matches <- data.table::as.data.table(matches)
  
  #Check on arguments
  if(is.SpatRaster(spat.ref)){
    if(missing(colname.treatment)) colname.treatment <- "treatment"
    if(missing(colname.id)) colname.id <- "cell"
  } else if(is.SpatVector(spat.ref)){
    if(missing(colname.treatment)) stop('BACI_contrast - Argument "colname.treatment" expected.')
    if(missing(colname.id)) stop('BACI_contrast - Argument "colname.id" expected.')
  } else {
    stop('BACI_contrast - SpatRaster or SpatVector spatial reference expected')
  }
  if((missing(before) | missing(after)) & missing(effect)) stop('BACI_contrast - Arguments "before" and "after", or argument "effect" must be provided.')


  if(!missing(effect)){
    if(!missing(fun)) data_effect <- extractMatches(effect, ci_matches, fun=fun, ...) else data_effect <- extractMatches(effect, ci_matches, method=method)
  } else {
    if(!all.equal(names(before), names(after))) stop('Incompatible layer names in "before" and "after"') 

    if(!missing(fun)) data_before <- extractMatches(before, ci_matches, fun=fun, ...) else data_before <- extractMatches(before, ci_matches, method=method)
    if(!missing(fun)) data_after <- extractMatches(after, ci_matches, fun=fun, ...) else data_after <- extractMatches(after, ci_matches, method=method)
    
    data_merge <- merge(data_before, 
                        data_after, 
                        by=c(colname.id, colname.treatment, colname.subclass), 
                        suffixes=c(".before", ".after"), sort=FALSE)
    
    data_effect <- data_merge[, grep(".after", names(data_merge), value=TRUE), with=FALSE] -    
      data_merge[, grep(".before", names(data_merge), value=TRUE), with=FALSE]
    names(data_effect) <- gsub(".after", "", names(data_effect))
    
    data_effect <- cbind(data_merge[, c(colname.id, colname.treatment, colname.subclass), with=FALSE],
                         data_effect)
  }

  contrast_p.data <- calc_contrast_p(data_effect[,! names(data_effect) %in% colname.id, with=FALSE], 
                                     colname.subclass=colname.subclass, 
                                     colname.treatment=colname.treatment)
  out.data <- merge(matches[get(colname.treatment)==1],
                    contrast_p.data, 
                    by=colname.subclass)
  
  out <- list(data=out.data)
  
  ## spatial output
  if(is.SpatRaster(spat.ref)){
    out.names <- names(out.data)[!(names(out.data) %in% c(colname.subclass, colname.id, colname.treatment))]
    out.spat <- rast(ci_matches$spat.ref, 
                     nlyrs=length(out.names), 
                     names=out.names)
    out.spat[out.data$cell] <- contrast_p.data[,..out.names]
    out$spat <- out.spat
  } else if(is.SpatVector(spat.ref)){
    contrast_p.vect <- merge(spat.ref, out.data)
    out$spat <- contrast_p.vect
  }
  
  return(out)
  
}




#' Calculate contrast and p value
#' 
#' Calculate the (BA)CI contrast and p-value for matched control-impact units
#' 
#' Parameter \code{data} must be a data.table (or data.frame) with columns indicating the subclass and treatment, the remaining columns will be assumed to be the effects for which contrast and p-value must be calculated
#' 
#' @export
#'
#' @importFrom data.table as.data.table is.data.table
#' @importFrom data.table .N .SD .BY .I .GRP .NGRP .EACHI
#' @importFrom stats t.test
#' 
#' @param data data.table or data.frame. See details.
#' @param colname.subclass character. Name of column indicating subclass
#' @param colname.treatment character. Name of column indicating treatment
#'  
#' 
calc_contrast_p <- function(data, colname.subclass, colname.treatment){

  ##  Coerce "data" to data.table
  if(!is.data.table(data)) data <- as.data.table(data)
  
  ##  Check columns
  if(!colname.subclass %in% names(data)) stop('Column defining subclass is missing in "data".')
  if(!colname.treatment %in% names(data)) stop('Column defining treatment is missing in "data".')
  
  ##  Identify effect variables (= all column that are not subclass or treatment)
  effect_names <- names(data)[! (names(data) %in% c(colname.subclass, colname.treatment))]

  fun_i <- function(i){
    effect_name_i <- effect_names[i]
    contrast <- data[, .(contrast = mean(.SD[get(colname.treatment)==0, get(effect_name_i)]) - mean(.SD[get(colname.treatment)==1, get(effect_name_i)])) , by=colname.subclass,]
    names(contrast) <- c(colname.subclass, paste0("contrast.",effect_name_i))
    
    p_value <- data[ ,if(.SD[get(colname.treatment)==1,.N]==1) {
      #1:k (k control units for each impact unit, contrast/p.value for each control unit individually - t.test for means=0)
      .(p_value= t.test(.SD[get(colname.treatment)==0, get(effect_name_i)]-.SD[get(colname.treatment)==1, get(effect_name_i)])$p.value)
    } else {
      #n:k (n control units for k control units, contrast/p.value for all n control units combined - t.test for equal means)
      .(p_value= t.test(.SD[get(colname.treatment)==0, get(effect_name_i)],.SD[get(colname.treatment)==1, get(effect_name_i)])$p.value)
    }, by=colname.subclass]
    names(p_value) <- c(colname.subclass, paste0("p_value.",effect_name_i))
    
    out_i <- merge(contrast, p_value, by=colname.subclass)
    return(out_i)
  }
  out <- lapply(1:length(effect_names), FUN=fun_i)
  if(length(out)>1) {
    out <- do.call(merge, out)
  } else {
    out <- out[[1]]
  }
  return(out)
}





