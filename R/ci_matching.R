#' Control-impact matching
#' 
#' @description
#' Match control units to impact/treatment vector or raster units.
#' 
#' This function uses the provided matching layers to match the provided candidate and impact units 
#' using the \code{MatchIt} package and the matching options provided therein.
#' 
#' @details
#' If \code{cands} is a SpatRaster, impact pixels should be labelled 1, candidate control pixels labelled 0, and pixels to be excluded labelled NA.
#' 
#' If \code{cands} is a SpatRaster, matching covariates provided as list elements in \code{matchvars} should be (a combination of) SpatRaster, 
#' gdalcubes data cube, 
#' or character (which will be assumed to refer to a filename and read using the default settings of \code{\link[terra]{rast}}).
#' Matching covariate layers should have band names, which should not be duplicated.
#' 
#' If \code{cands} is a SpatVector, attributes should include an ID column with name specified by the argument \code{colname.id} (defaults to "ID"), 
#' and a treatment column with name specified by the argument \code{colname.treatment} (defaults to "treatment") and with value 1 for impact/treatment units and value 0 for candidate control units.
#' All remaining columns in \code{cands} will be interpreted as matching variables.
#' 
#' if \code{cands} is a SpatVector, matching covariates provided as list elements in \code{matchvars} should be (a combination of) SpatVector objects with identical geometry as \code{cands},
#' or data.table/data.frame object with an ID column with name corresponding to the \code{colname.id} argument. 
#' All columns in \code{matchVars} except those specified \code{colname.id} and \code{colname.treatment} will be interpreted as matching variables.
#' 
#' The function returns an object of class data.table, unless matching is rejected in the interactive evaluation in which case the function returns NULL.
#' Column names of the returned data.table object are "subclass", and the treatment column name defined by \code{colname.treatment}. 
#' If \code{cands} is a SpatRaster, additional returned column names are "cell" (indicating the cell number of pixels in \code{cands}), and "x" and "y" (relative to the crs of \code{cands}).
#' If \code{cands} is a SpatVector, an additional column identifies the vector ID specified in \code{colname.id}.
#' 
#' 
#' @export matchCI
#' 
#' @importFrom data.table as.data.table
#' @import MatchIt
#' @import terra
#' 
#' @param cands SpatRaster or SpatVector object. See Details.
#' @param matchVars list of matching variables. See Details.
#' @param eval logical. Should matching summary be shown for interactive evaluation? Function will return NULL if matching is rejected.
#' @param asmd_warn numeric. Function will output a warning message if Absolute Standardized Mean Difference is above this value.
#' @param asmd_error numeric. Function will output an error if Absolute Standardized Mean Difference is above this value.
#' @param ... additional inputs to \link[MatchIt]{matchit}, e.g., \code{ratio}, \code{replace}, \code{method}, \code{distance}, or \code{link}.
#' 
#' @returns data table with matched control-impact units.  
#' 
matchCI <- function(cands, matchVars, colname.id="ID", colname.treatment="treatment",
                    eval=FALSE, asmd_warn=NULL, asmd_error=NULL, ...){

  #TODO?
  # subset of potential control (/impact) pixels before matching for large areas?
  # check if matchlyrs has names, add default names (e.g. 1:n) if not
  
  if(is.SpatRaster(cands)){ 
    ##  Candidate controls from raster
    
    matchVars <- collate_matching_layers(cands, matchVars)
    #Check that layers of matching variables have (unique) names, fix if not. 
    if(any(nchar(names(matchVars))==0)) stop("Missing name(s) in matchVars")
    if(any(duplicated(names(matchVars)))) stop("Duplicated band names in matchVars")

    #Prepare data.table for matching
    if(nlyr(cands)>1){
      warning("nlyr(cands)>1, only first layer will be used")
      cands <- subset(cands,subset=1)
    }
    names(cands) <- colname.treatment
    matchVars_names <- names(matchVars)
    
    dt <- cbind(as.data.table(cands, na.rm=FALSE, cells=TRUE, xy=TRUE),
                as.data.table(matchVars, na.rm=FALSE))
    dt <- na.omit(dt)
    
    names_out <- c("cell", "x", "y", "subclass", colname.treatment)
    
  } else if (is.SpatVector(cands)){ #Candidate controls from vector
   
    #Remove any columns in cands that are not "ID" or "treatment"
    cands <- cands[c(colname.id, colname.treatment)]
    
    #combine the matching layers
    matchVars <- collate_matching_layers(cands, matchVars, colname.id=colname.id, colnames.ignore=colname.treatment)
    matchVars_names <- names(matchVars) |> setdiff(c(colname.id, colname.treatment))
    
    dt <- as.data.table(matchVars)
    names_out <- c(colname.id, "subclass", colname.treatment)
    
  } else {
    stop("cands of class SpatRaster of SpatVector expected")
  }
  
  #Define matching formula
  frm <- as.formula(paste0(colname.treatment," ~ ", paste0(matchVars_names, collapse=" + ")))
  
  #Run MatchIt
  m.out <- MatchIt::matchit(formula=frm, data=dt, 
                            ...)
  
  #Interactive evaluation of matching
  if(isTRUE(eval)){
    print(m.out)
    readline(prompt = "Press <Enter> to continue.")
    print(summary(m.out, un = TRUE))
    readline(prompt = "Press <Enter> to continue.")
    plot(m.out, type = "jitter", interactive = FALSE)
    readline(prompt = "Press <Enter> to continue.")
    # plot(m.out, type = "density")
    plot(summary(m.out), xlim=c(0,1))
    
    accept <- readline(prompt = "Accept matching (Y/N)? ")
    if(toupper(substr(accept,1,1))=="N") return(NULL)
  }
  
  asmd_matched <- summary(m.out, un = TRUE)$sum.matched[,"Std. Mean Diff."] |> abs()
  if(!(isTRUE(is.null(asmd_warn)) | isTRUE(is.na(asmd_warn)))){
    if(!is.numeric(asmd_warn)) stop("asmd_warn must be numeric")
    asmd_ex <- asmd_matched>asmd_warn
    if(sum(asmd_ex)>1) warning(paste0("ASMD threshold of ", asmd_warn, " exceeded for matching variables: ", paste0(names(asmd_matched[asmd_ex]), collapse=", "))) 
  }
  if(!(isTRUE(is.null(asmd_error)) | isTRUE(is.na(asmd_error)))){
    if(!is.numeric(asmd_error)) stop("asmd_warn must be numeric")
    asmd_ex <- asmd_matched>asmd_error
    if(sum(asmd_ex)>1) stop(paste0("ASMD threshold of ", asmd_error, " exceeded for matching variables: ", paste0(names(asmd_matched[asmd_ex]), collapse=", "))) 
  }
  
  #Extract the matched dataset
  m.data <- MatchIt::get_matches(m.out)[names_out] |> as.data.table()
  return(m.data)
}

#' Collate matching layers
#' 
#' @description
#' Process different matching layers in compatible format
#' 
#' @details
#' If x is a SpatRaster, vars_list should have as list elements SpatRaster, data cube objects. 
#' If x is a SpatVector, vars_list should have as list elements SpatVector, data.frame, or data.table objects.
#' 
#' @importFrom terra compareGeom rast project relate
#' @importFrom data.table is.data.table as.data.table
#' 
#' @param x SpatRaster or SpatVector
#' @param vars_list List if matching variables. See Details.
#' @param colname.id if x is a SpatVector, the column name identifying the ID allowing merging of different datasets
#' @param colnames.ignore if x is a SpatVector, vector of column names in vars_list to ignore
#' 
#' @returns description
#' 
collate_matching_layers <- function(x, vars_list, colname.id="ID", colnames.ignore=NULL){
  
  if(!is.list(vars_list)) vars_list <- list(vars_list)
  
  if (is.SpatRaster(x)){
    
    rast_list <- vector(mode="list", length=length(vars_list))
    
    for(i in 1:length(vars_list)){
      inLyr <- vars_list[[i]]
      #If gdalcube -> transform to spatRaster
      if(is.cube(inLyr)){
        inLyr <- as.SpatRaster(inLyr)
      }
      #If character -> assume it is filename, read as spatRaster
      if(is.character(inLyr)){
        inLyr <- terra::rast(inLyr)
      }
      #If spatRaster -> project to correct geometry
      if(is.spatRaster(inLyr)){
        if(isTRUE(terra::compareGeom(x, inLyr, stopOnError=FALSE))){
          i_out <- inLyr
        } else {
          i_out <- terra::project(inLyr, x)
        }
      } else {
        warning("Unrecognized format, skipping input")
        i_out <- NULL
      }
      
      rast_list[[i]] <- i_out  
    }
    out <- terra::rast(rast_list)
    
  } else if(is.SpatVector(x)){
    
    x_out <- x
    for (i in 1:length(vars_list)){
      inLyr <- vars_list[[i]]
      if(is.SpatVector(inLyr)){
        if(!is.null(colnames.ignore)){
          lyr_select <- which((!(names(inLyr) %in% colnames.ignore)))
          inLyr <- inLyr[,lyr_select]
        }
        
        if(colname.id %in% names(inLyr)){
          x_out <- merge(x_out, as.data.table(inLyr), by=colname.id)
        } else {
          pairs <- relate(x_out, inLyr, "equals", pairs=TRUE)
          x_out <-  cbind(x_out[pairs[,1]], inLyr[pairs[,2]])
        }  
      } else if (is.data.frame(inLyr) | is.data.table(inLyr)){
        if(!is.data.table(inLyr)) inLyr <- as.data.table(inLyr)
        if(!is.null(colnames.ignore)){
          lyr_select <- which((!(names(inLyr) %in% colnames.ignore)))
          inLyr <- subset(inLyr, select= lyr_select)
        } 
        x_out <- merge(x_out, inLyr, by=colname.id)
        
      } else {
        stop("Matching variable should be a SpatVector, data.frame, or data.table")
      }
      
    }
    
    out <- x_out
  } else {
    stop("SpatRaster or SpatVector input expected.")
  }
  return(out)
}


