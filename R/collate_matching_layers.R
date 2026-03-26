#' Collate matching layers
#' 
#' @description
#' This function serves a a pre-processing step to transform the spatial layers of candidate control and input units, and the different spatial layers of matching covariates, into a format suitable for control-impact matching. 
#' 
#' @details
#' 
#' The argument `vars_list` is used to provide the different matching covariates, with each list element of `vars_list` corresponding to a (set of) covariates.
#' 
#' If `x` is a SpatRaster, list elements of `vars_list` should be SpatRaster or `gdalcubes` data cube objects. Each layer of these raster datasets will be considered as a matching covariate.
#' 
#' If `x` is a SpatVector, list elements of `vars_list` can be SpatVector, data.frame, or data.table object that can be linked (through geometry or common attributes/column names) to x.
#' Alternatively, list elements can refer to raster datasets (SpatRaster or data cube). 
#' In this case, each list element must by itself be a list with minimum list element named `data` corresponding to the raster dataset.
#' Additional list elements can then be provided to define how the raster data is summarized over the vector units of analysis in `x`.
#' If geometries in `x` are polygons, this summarizing function can be provided by the additional list element named `fun` (e.g., `fun="mean"`). More additional list elements can be provided as arguments to `fun`, for example `na.rm=TRUE`. 
#' If geometries in `x` are points, the additional list element named `method` can be defined to specify nearest neighbour (`method="simple"`) of bilinear interpolation (`method="bilinear"`) extraction from the raster dataset.
#' In the absence of an additional list element specifying `fun` or `method`, nearest neighbour extraction for point geometries is performed, which will be the polygon centroids if geometries in `x` are polygons.
#' Combinations of SpatVector, data.frame/data.table and/or raster/summarizing function pairs are allowed.
#' 
#' The output object is a names list with elements named `data` and `spat.ref`. 
#' The list element `data` contains a data.table with rows corresponding to the spatial units of analysis defined in `x`. 
#' If `x` is a SpatRaster, columns indicate the unique ID corresponding to cell number (column named "cell"),
#' whether a unit of analysis is a candidate control (0) or impact (1) unit (in column with name specified in `colname.treatment`),
#' and the values of the different matching covariates (in column names corresponding to the layer names of the matching covariates raster dataset).
#' If `x` is a SpatVector, columns refer to unique ID and treatment (defined in `x` attributes), and the different matching covariates. 
#' 
#' @import terra
#' @importFrom gdalcubes extract_geom srs
#' @importFrom sf st_as_sf
#' @importFrom data.table is.data.table as.data.table
#' 
#' @param x SpatRaster or SpatVector of candidate control and impact units. E.g., the output of [create_control_candidates()] for SpatRaster.
#' @param vars_list List of matching variables and, optionally, associated summarizing functions. See Details.
#' @param colname.treatment if x is a SpatRaster, character identifying the column name that will be assigned to the treatment variable. Defaults to "treatment". 
#' 
#' @export
#' 
#' @returns a named list. The list element `data` is a data.table (see Details), list element `spat.ref` corresponds to `x`.
#' 

collate_matching_layers <- function(x, vars_list, colname.treatment){
  
  if (is.SpatRaster(x)){
    if(missing(colname.treatment)) colname.treatment <- "treatment"
    matching_data <- collate_matching_layers.SpatRaster(x, vars_list, colname.treatment)
  } else if(is.SpatVector(x)){
    matching_data <- collate_matching_layers.SpatVector(x, vars_list)
  } else {
    stop("SpatRaster or SpatVector input expected.")
  }
  
  return(matching_data)
}

#' Collate matching layers - SpatRaster
#' 
#' @description
#' SpatRaster case for [collate_matching_layers()]
#' 
#' @details
#' Internal function
#' 
#' @noRd
#'
collate_matching_layers.SpatRaster <- function(x, vars_list, colname.treatment){
  
  # If vars_list is a single SpatRaster, transform into list
  if(!is.list(vars_list)) vars_list <- list(vars_list)

  ##  Combine all matching layers in a single SpatRaster object
  match_rast <- vector(mode="list", length=length(vars_list))
  for(i in 1:length(vars_list)){
    inLyr <- vars_list[[i]]
    #If gdalcube -> transform to spatRaster (This should be updated to directly extract values from cube)
    if(is.cube(inLyr)){
      inLyr <- as.SpatRaster(inLyr)
    }
    #If character -> assume it is filename, read as spatRaster
    if(is.character(inLyr)){
      inLyr <- terra::rast(inLyr)
    }
    #If SpatRaster -> project to correct geometry
    if(is.SpatRaster(inLyr)){
      if(isTRUE(terra::compareGeom(x, inLyr, stopOnError=FALSE))){
        i_out <- inLyr
      } else {
        i_out <- terra::project(inLyr, x)
      }
    } else {
      warning("Unrecognized format, skipping input")
      i_out <- NULL
    }
    
    match_rast[[i]] <- i_out  
  }
  match_rast <- terra::rast(match_rast)
  
  ##  Check that layers of matching variables have (unique) names - to do: fix problem instead of throwing error
  if(any(nchar(names(match_rast))==0)) stop("Missing name(s) in variables names in matching variables")
  if(any(duplicated(names(match_rast)))) stop("Duplicated band names in matching variables")
  if(any(c("cell") %in% names(match_rast))) stop('Matching matching variables names cannot be "cell"')

  #Prepare data.table for matching
  if(nlyr(x)>1){
    warning("Multiple layers in control candidates raster, only first layer will be used")
    x <- subset(x,subset=1)
  }
  names(x) <- colname.treatment

  dt <- cbind(as.data.table(x, na.rm=FALSE, cells=TRUE, xy=FALSE),
              as.data.table(match_rast, na.rm=FALSE))
  dt <- na.omit(dt)

  return(list(data=dt, 
              spat.ref=x))
}


#' Collate matching layers - SpatRaster
#' 
#' @description
#' SpatVector case for [collate_matching_layers()]
#' 
#' @details
#' Internal function
#' 
#' @noRd
#'
collate_matching_layers.SpatVector <- function(x, vars_list){
  
  ## Initiate output SpatVector
  x_out <- x

  ## Iteratively process each matching variable in "var_list"
  for (i in 1:length(vars_list)){
    var <- vars_list[[i]]
    
    if(!is.list(var) | is.data.frame(var)){
      var.data <- var
      var.args <- NULL
    } else {
      var.data <- var$data
      var.args <- var[names(var) != "data"]
    }

    #data.frame/data.table:
    if (is.data.frame(var.data)) {
       #merge with x if corresponding column names exist, error if not
      if (length(intersect(names(x), names(var.data)))==0) stop("No matching column names")
      x_out <- merge(x_out, var.data)
    }
    
    #SpatVector:
    if(is.SpatVector(var.data)){
      if (length(intersect(names(x), names(var.data)))>0){
        #If matching column names exist between x and var, merge based on column names
        x_out <- merge(x_out, var.data)
      } else {
        #Merge based on geometry (option: add warning if geometries are dropped)
        pairs <- terra::relate(x_out, var.data, "equals", pairs=TRUE, na.rm=TRUE)
        x_out <-  cbind(x_out[pairs[,1]], var.data[pairs[,2]])
      }
    } 
    
    #Character
    if(is.character(var.data)){
      var.data <- rast(var.data)
    }
    
    #SpatRaster
    if(is.SpatRaster(var.data)){
      
      # If argument "fun" is not provided, extract point values - even if x has polygon geometries
      if(is.null(var.args$fun) | isTRUE(var.args$method %in% c("simple","bilinear"))){
        #Extract values for points. If geometries of x are not points, use centroids
        if(geomtype(x)=="points"){
          x_extract <- x
        } else {
          x_extract <- centroids(x)
        }
      } else {
        x_extract <- x
      }
      
      if(terra::crs(x_extract) != terra::crs(var.data)) x_extract <- terra::project(x_extract,var.data)
      
      # Set function arguments
      fun_args <- list()
      fun_args$x <- var.data
      fun_args$y <- x_extract
      fun_args$bind <- TRUE
      fun_args <- c(fun_args, var.args)
      
      # Call terra::extract with arguments
      var_extract <- do.call(terra::extract, fun_args)
      
      # Merge extracted data to SpatVector
      x_out <- merge(x_out, var_extract)
      
    }
    
    #data cube
    if(is.cube(var.data)){
      
      #Rename argument "fun" to "FUN" 
      if("fun" %in% names(var.args)) names(var.args)[which(names(var.args)=="fun")] <- "FUN"
      
      # If argument "fun" (or "FUN") is not provided, extract point values - even if x has polygon geometries
      if(is.null(var.args$FUN) | isTRUE(var.args$method %in% c("simple","bilinear"))){
        #Extract values for points. If geometries of x are not points, use centroids
        if(geomtype(x)=="points"){
          x_extract <- x
        } else {
          x_extract <- centroids(x)
        }
      } else {
        x_extract <- x
      }
      
      if(terra::crs(x_extract) != gdalcubes::srs(var.data)) x_extract <- terra::project(x_extract, gdalcubes::srs(var.data))

      fun_args <- list()
      fun_args$cube <- var.data
      fun_args$sf <- sf::st_as_sf(x_extract)
      fun_args$reduce_time <- TRUE
      fun_args$merge <- TRUE
      fun_args$drop_geom <- TRUE
      fun_args <- c(fun_args, var.args)
      
      var_extract <- do.call(gdalcubes::extract_geom, fun_args)
      
      # Remote the "time" field (for point extractions)
      if("time" %in% names(var_extract)) var_extract <- var_extract[names(var_extract)!="time"]
      
      x_out <- merge(x_out, var_extract)
    }
  }
  return(list(data=as.data.table(x_out), 
              spat.ref=x)) 
}

