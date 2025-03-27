
#' Before-After-Control-Impact contrast
#' 
#' Calculate the Control-Impact contrast and p-value
#' 
#' After Meroni et al. (2017) and del Río-Mena et al. (2021)
#' 
#' @seealso [\code{\link{CI_contrast}}]
#' 
#' @export
#' 
#' @importFrom data.table as.data.table is.data.table
#' @import terra
#' 
#' @param ci_matches description
#' @param before description
#' @param after description
#' @param effect description
#' @param spatref Spatial reference in case crs ci_matches does not correspond to crs raster layers. Not yet implemented.
#' 
#' @returns list (See Details)
#' 
#' 
#' @references Meroni, M., Schucknecht, A., Fasbender, D., Rembold, F., Fava, F., Mauclaire, M., Goffner, D., Di Lucchio, L.M., Leonardi, U., 2017. Remote sensing monitoring of land restoration interventions in semi-arid environments with a before–after control-impact statistical design. International Journal of Applied Earth Observation and Geoinformation 59, 42–52.
#' @references del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A., 2021. Long-term assessment of ecosystem services at ecological restoration sites using Landsat time series. PLOS ONE 16, e0243020.
#' 

BACI_contrast <- function(ci_matches, before, after, effect, crs.matches=NULL, rast.out=NULL){
  
  
  if((missing(before) | missing(after)) & missing(effect)) stop('Arguments "before" and "after", or argument "effect" must be provided.')
  
  if(!is.data.frame(ci_matches)) stop('Argument "ci_matches" of class data.frame or data.table expected.')
  if(!is.data.table(ci_matches)) ci_matches <- as.data.table(ci_matches)
  
  if(!all(c("subclass", "treatment") %in% names(ci_matches))) stop('Column names "subclass" and "treatment" expected.')

  
  if(all(c("x", "y") %in% names(ci_matches))){
    #Raster input
    
    #Drop all excess columns from matching results
    ci_matches <- subset(ci_matches, select=c(x,y,subclass,treatment))
    
    if(!missing(effect)){
      data_effect <- extractFromRaster(effect, ci_matches)
    } else {
      data_before <- extractFromRaster(before, ci_matches)
      data_after <- extractFromRaster(after, ci_matches)
      #data_xy <- subset(data_before, select=c(x,y))
      data_effect <- cbind (subset(data_before, select=c(subclass, treatment)), 
                            subset(data_after, select=-c(x,y,subclass, treatment)) - subset(data_before, select=-c(x,y,subclass, treatment)))
      #might be safer to do this with merge or other operation, also could introduce check that column names match
    }
    
    contrast_p.data <- calc_contrast_p(data_effect)
    contrast_p.data <- merge(unique(subset(ci_matches, subset= treatment==1, select=c(x,y, subclass))),
                             contrast_p.data, by="subclass")
    
    out <- list()
    out$data <- contrast_p.data
      
    if(!is.null(rast.out)){
      out.names <- names(contrast_p.data)[!(names(contrast_p.data) %in% c("subclass", "x", "y"))]
      
      contrast_p.rast <- rast(rast.out, nlyrs=length(out.names), names=out.names)
      
      #TODO: transform xy to crs(rast), if required 
      cellnrs <- cellFromXY(contrast_p.rast, contrast_p.data[,c("x","y")])
      contrast_p.rast[cellnrs] <- contrast_p.data[,..out.names]
      
      out$spat <- contrast_p.rast
    }
     
    return(out)

    
  } else if ("ID" %in% names(ci_matches)){
    #vector input
    
    
    return(NULL)
  }
  
  
  
  return(NULL)
} 





calc_contrast_p <- function(data, colname.subclass="subclass", colname.treatment="treatment"){

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





# 
# ##  Create output
# CI_out <- list()
# CI_out$data <- merge(contrast, p_value, by="subclass")
# 
# ##  Add spatial output
# if(!is.null(SpatRef)){
#   require(terra)
#   if(class(SpatRef)=="SpatRaster"){
#     if(!("x" %in% names(data) & "y" %in% names(data))){
#       stop('Column "x" and/or "y" is missing in "data", spatial output ignored.')
#     } else {
#       CI_out$data <- merge(unique(data[treatment==1, .(subclass,x,y), ]), CI_out$data)
#       
#       r <- rast(SpatRef, nlyrs=2, names=c("contrast", "p_value") ,vals=NA)
#       cellnrs <- cellFromXY(r, CI_out$data[,c("x","y")])
#       r[cellnrs] <- CI_out$data[,c("contrast", "p_value")]
#       
#       CI_out$spat <- r
#     }
#   } else if(class(SpatRef)==SpatVector){
#     if(!("id" %in% names(data))){
#       stop('Column "id" is missing in "data", spatial output ignored.')
#     } else {
#       ##To be checked
#       #remove existing attributes
#       v <- SpatRef
#       v <- v[,-c(1:ncol(v))]
#       #Add contrast/p_value as new attributes
#       CI_out$data <- merge(unique(data[treatment==1, .(subclass,id), ]), CI_out$data)
#       v <- cbind(v, merge(data.frame(id=1:nrow(v)), CI_out$data[, .(id, contrast, p_value)], all.x=TRUE))
#       CI_out$spat <- v
#     }
#   } else {
#     warning("Invalid spatial reference (must be of class SpatRaster of SpatVector), spatial output ignored")
#   }
# }
# ##  Return output
# return(CI_out)
# 




#' 
#' #' Control-Impact contrast
#' #' 
#' #' Calculate the Control-Impact contrast and p-value
#' #' 
#' #' @seealso [\code{\link{BACI_contrast}}]
#' #' @export
#' #' @import data.table
#' #' @import terra
#' #' @param data data.table
#' #' @param SpatRef SpatRaster
#' #' @returns data.table
#' CI_contrast <- function(data, SpatRef=NULL){
#'   #TO DO: output as SpatVector
#' 
#'   ##  Coerce "data" to data.table
#'   if(!is.data.table(data)) data <- as.data.table(data)
#'   
#'   ##  Check columns
#'   if(!"subclass" %in% names(data)) stop('Column "subclass" is missing in "data".')
#'   if(!"treatment" %in% names(data)) stop('Column "treatment" is missing in "data".')
#'   if(!"effect" %in% names(data)) stop('Column "effect" is missing in "data".')
#' 
#'   ##  Calculate contrast/p_value
#'   contrast <- merge(data[treatment==1, .(impact_effect=mean(effect)), by = .(subclass)],
#'                     data[treatment==0, .(control_effect=mean(effect)), by = .(subclass)],
#'                     by=c("subclass"))[,.(subclass, contrast=control_effect-impact_effect)]
#'   #contrast <- data[, .(control = mean(.SD[treatment==0, effect]) - mean(.SD[treatment==1, effect])), by=subclass,]
#'   #The more elegant version using .SD seems to be slightly slower.
#' 
#'   p_value <- data[ ,if(.SD[treatment==1,.N]==1) {
#'     #1:k (k control units for each impact unit, contrast/p.value for each control unit individually - t.test for means=0)
#'       .(p_value= t.test(.SD[treatment==0, effect]-.SD[treatment==1, effect])$p.value)
#'     } else {
#'       #n:k (n control units for k control units, contrast/p.value for all n control units combined - t.test for equal means)
#'     .(p_value= t.test(.SD[treatment==0,effect],.SD[treatment==1,effect])$p.value)
#'     }, by=subclass]
#' 
#'   ##  Create output
#'   CI_out <- list()
#'   CI_out$data <- merge(contrast, p_value, by="subclass")
#'   
#'   ##  Add spatial output
#'   if(!is.null(SpatRef)){
#'     require(terra)
#'     if(class(SpatRef)=="SpatRaster"){
#'       if(!("x" %in% names(data) & "y" %in% names(data))){
#'         stop('Column "x" and/or "y" is missing in "data", spatial output ignored.')
#'       } else {
#'         CI_out$data <- merge(unique(data[treatment==1, .(subclass,x,y), ]), CI_out$data)
#'         
#'         r <- rast(SpatRef, nlyrs=2, names=c("contrast", "p_value") ,vals=NA)
#'         cellnrs <- cellFromXY(r, CI_out$data[,c("x","y")])
#'         r[cellnrs] <- CI_out$data[,c("contrast", "p_value")]
#'         
#'         CI_out$spat <- r
#'       }
#'     } else if(class(SpatRef)==SpatVector){
#'       if(!("id" %in% names(data))){
#'         stop('Column "id" is missing in "data", spatial output ignored.')
#'       } else {
#'         ##To be checked
#'         #remove existing attributes
#'         v <- SpatRef
#'         v <- v[,-c(1:ncol(v))]
#'         #Add contrast/p_value as new attributes
#'         CI_out$data <- merge(unique(data[treatment==1, .(subclass,id), ]), CI_out$data)
#'         v <- cbind(v, merge(data.frame(id=1:nrow(v)), CI_out$data[, .(id, contrast, p_value)], all.x=TRUE))
#'         CI_out$spat <- v
#'       }
#'     } else {
#'       warning("Invalid spatial reference (must be of class SpatRaster of SpatVector), spatial output ignored")
#'     }
#'   }
#'   ##  Return output
#'   return(CI_out)
#' }
#' 
#' #' Before-After-Control-Impact contrast
#' #' 
#' #' Calculate the Control-Impact contrast and p-value
#' #' 
#' #' After Meroni et al. (2017) and del Río-Mena et al. (2021)
#' #' 
#' #' @seealso [\code{\link{CI_contrast}}]
#' #' @export
#' #' @import data.table
#' #' @import terra
#' #' @param data data.table
#' #' @param SpatRef SpatRaster
#' #' @returns data.table
#' #' @references Meroni, M., Schucknecht, A., Fasbender, D., Rembold, F., Fava, F., Mauclaire, M., Goffner, D., Di Lucchio, L.M., Leonardi, U., 2017. Remote sensing monitoring of land restoration interventions in semi-arid environments with a before–after control-impact statistical design. International Journal of Applied Earth Observation and Geoinformation 59, 42–52.
#' #' @references del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A., 2021. Long-term assessment of ecosystem services at ecological restoration sites using Landsat time series. PLOS ONE 16, e0243020.
#' BACI_contrast <- function(data, SpatRef=NULL){
#' 
#'   
#'   #Coerce "data" to data.table
#'   require(data.table)
#'   if(!is.data.table(data)) data <- as.data.table(data)
#'   
#'   #Check columns
#'   if(!"subclass" %in% names(data)) stop('Column "subclass" is missing in "data".')
#'   if(!"treatment" %in% names(data)) stop('Column "treatment" is missing in "data".')
#'   if(!"before" %in% names(data)) stop('Column "before" is missing in "data".')
#'   if(!"after" %in% names(data)) stop('Column "after" is missing in "data".')
#'   
#'   #Calculate effect
#'   data[, ':='(effect=after-before)]
#'   
#'   #Call CI_calc
#'   BACI_out <- CI_contrast(data, SpatRef) 
#'   return(BACI_out)
#' } 
#' 
#' 
#' #TO DO
#' #_____
#' #in BACI_contrast: provide option to use cell nr/id instead of x/y to refer to position in SpatRaster
#' #implementation of Wauchope et al. (2021) - SI
#' # before-after on average
#' #BA_avg <- function(...){}
#' # before-after-control-impact on average
#' #BACI_avg <- function{...}{}
#' # before-after on trend+immediate
#' #BA_ti <- function{...}{}
#' #before-after-control-impact on trend+immediate
#' #BACI_ti <- function{...}{}
