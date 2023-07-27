#Author: Jasper Van doninck
#Date: July 2023



CI_calc <- function(data, SpatRef=NULL){
  ####  CI_calc
  ####
  ####  Control-Impact evaluation calculation
  ####
  ####
  ####
  
  #TO DO: output as SpatVector
  
  
  ##  Coerce "data" to data.table
  require(data.table)
  if(!is.data.table(data)) data <- as.data.table(data)
  
  ##  Check columns
  if(!"subclass" %in% names(data)) stop('Column "subclass" is missing in "data".')
  if(!"treatment" %in% names(data)) stop('Column "treatment" is missing in "data".')
  if(!"value" %in% names(data)) stop('Column "value" is missing in "data".')

  ##  Calculate contrast/p_value
  contrast <- merge(data[treatment==1, .(impact_value=mean(value)), by = .(subclass)],
                    data[treatment==0, .(control_value=mean(value)), by = .(subclass)],
                    by=c("subclass"))[,.(subclass, contrast=control_value-impact_value)]
  #contrast <- data[, .(control = mean(.SD[treatment==0, value]) - mean(.SD[treatment==1, value])), by=subclass,]
  #The more elegant version using .SD seems to be slightly slower.

  p_value <- data[ ,if(.SD[treatment==1,.N]) {
      #1:k (k control units for each impact unit, contrast/p.value for each control unit individually - t.test for means=0)
      .(p_value= t.test(.SD[treatment==0, value]-.SD[treatment==1, value])$p.value)
    } else {
      #n:k (n control units for k control units, contrast/p.value for all n control units combined - t.test for equal means)
    .(p_value= t.test(.SD[treatment==0,value],.SD[treatment==1,value])$p.value)
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


BACI_calc <- function(data, SpatRef=NULL){
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
  data[, ':='(value=after-before)]
  
  #Call CI_calc
  BACI_out <- CI_calc(data, SpatRef) 
  return(BACI_out)
} 


#TO DO
#implementation of Wauchope et al. (2021) - see SI

# before-after on average
#BA_avg <- function(...){}

# before-after-control-impact on average
#BACI_avg <- function{...}{}

# before-after on trend+immediate
#BA_ti <- function{...}{}

#before-after-control-impact on trend+immediate
#BACI_ti <- function{...}{}



