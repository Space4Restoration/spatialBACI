#' Control-Impact contrast
#' 
#' Calculate the Control-Impact contrast and p-value
#' 
#' @seealso [\code{\link{BACI_contrast}}]
#' @export
#' @import data.table
#' @import terra
#' @param data data.table
#' @param SpatRef SpatRaster
#' @returns data.table
CI_contrast <- function(data, SpatRef=NULL){
  #TO DO: output as SpatVector

  ##  Coerce "data" to data.table
  if(!is.data.table(data)) data <- as.data.table(data)
  
  ##  Check columns
  if(!"subclass" %in% names(data)) stop('Column "subclass" is missing in "data".')
  if(!"treatment" %in% names(data)) stop('Column "treatment" is missing in "data".')
  if(!"effect" %in% names(data)) stop('Column "effect" is missing in "data".')

  ##  Calculate contrast/p_value
  contrast <- merge(data[treatment==1, .(impact_effect=mean(effect)), by = .(subclass)],
                    data[treatment==0, .(control_effect=mean(effect)), by = .(subclass)],
                    by=c("subclass"))[,.(subclass, contrast=control_effect-impact_effect)]
  #contrast <- data[, .(control = mean(.SD[treatment==0, effect]) - mean(.SD[treatment==1, effect])), by=subclass,]
  #The more elegant version using .SD seems to be slightly slower.

  p_value <- data[ ,if(.SD[treatment==1,.N]) {
      #1:k (k control units for each impact unit, contrast/p.value for each control unit individually - t.test for means=0)
      .(p_value= t.test(.SD[treatment==0, effect]-.SD[treatment==1, effect])$p.value)
    } else {
      #n:k (n control units for k control units, contrast/p.value for all n control units combined - t.test for equal means)
    .(p_value= t.test(.SD[treatment==0,effect],.SD[treatment==1,effect])$p.value)
    }, by=subclass]

  ##  Create output
  CI_out <- list()
  CI_out$data <- merge(contrast, p_value, by="subclass")
  
  ##  Add spatial output
  if(!is.null(SpatRef)){
    require(terra)
    if(class(SpatRef)=="SpatRaster"){
      if(!("x" %in% names(data) & "y" %in% names(data))){
        stop('Column "x" and/or "y" is missing in "data", spatial output ignored.')
      } else {
        CI_out$data <- merge(unique(data[treatment==1, .(subclass,x,y), ]), CI_out$data)
        
        r <- rast(SpatRef, nlyrs=2, names=c("contrast", "p_value") ,vals=NA)
        cellnrs <- cellFromXY(r, CI_out$data[,c("x","y")])
        r[cellnrs] <- CI_out$data[,c("contrast", "p_value")]
        
        CI_out$spat <- r
      }
    } else if(class(SpatRef)==SpatVector){
      if(!("id" %in% names(data))){
        stop('Column "id" is missing in "data", spatial output ignored.')
      } else {
        ##To be checked
        #remove existing attributes
        v <- SpatRef
        v <- v[,-c(1:ncol(v))]
        #Add contrast/p_value as new attributes
        CI_out$data <- merge(unique(data[treatment==1, .(subclass,id), ]), CI_out$data)
        v <- cbind(v, merge(data.frame(id=1:nrow(v)), CI_out$data[, .(id, contrast, p_value)], all.x=TRUE))
        CI_out$spat <- v
      }
    } else {
      warning("Invalid spatial reference (must be of class SpatRaster of SpatVector), spatial output ignored")
    }
  }
  ##  Return output
  return(CI_out)
}

#' Before-After-Control-Impact contrast
#' 
#' Calculate the Control-Impact contrast and p-value
#' 
#' After Meroni et al. (2017) and del Río-Mena et al. (2021)
#' 
#' @seealso [\code{\link{CI_contrast}}]
#' @export
#' @import data.table
#' @import terra
#' @param data data.table
#' @param SpatRef SpatRaster
#' @returns data.table
#' @references Meroni, M., Schucknecht, A., Fasbender, D., Rembold, F., Fava, F., Mauclaire, M., Goffner, D., Di Lucchio, L.M., Leonardi, U., 2017. Remote sensing monitoring of land restoration interventions in semi-arid environments with a before–after control-impact statistical design. International Journal of Applied Earth Observation and Geoinformation 59, 42–52.
#' @references del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A., 2021. Long-term assessment of ecosystem services at ecological restoration sites using Landsat time series. PLOS ONE 16, e0243020.
BACI_contrast <- function(data, SpatRef=NULL){
  ####  CI_calc
  ####
  ####  Before-After-Control-Impact evaluation calculation
  ####
  ####  Implementation of BACI contrast as used by Meroni et al. (2017) and del Rio-Mena et al. (2021)
  ####
  
  
  #Coerce "data" to data.table
  require(data.table)
  if(!is.data.table(data)) data <- as.data.table(data)
  
  #Check columns
  if(!"subclass" %in% names(data)) stop('Column "subclass" is missing in "data".')
  if(!"treatment" %in% names(data)) stop('Column "treatment" is missing in "data".')
  if(!"before" %in% names(data)) stop('Column "before" is missing in "data".')
  if(!"after" %in% names(data)) stop('Column "after" is missing in "data".')
  
  #Calculate effect
  data[, ':='(effect=after-before)]
  
  #Call CI_calc
  BACI_out <- CI_contrast(data, SpatRef) 
  return(BACI_out)
} 


#TO DO
#_____
#in BACI_contrast: provide option to use cell nr/id instead of x/y to refer to position in SpatRaster
#implementation of Wauchope et al. (2021) - SI
# before-after on average
#BA_avg <- function(...){}
# before-after-control-impact on average
#BACI_avg <- function{...}{}
# before-after on trend+immediate
#BA_ti <- function{...}{}
#before-after-control-impact on trend+immediate
#BACI_ti <- function{...}{}
