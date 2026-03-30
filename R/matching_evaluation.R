
#' Matching evaluation
#' 
#' @description
#' Methods for evaluating matching results
#' 
#' @details
#' This function uses the [cobalt::bal.tab()] function to generate balance statistics from the [MatchIt::matchit()] call. 
#' If the argument `covariate_overlap` is set to `TRUE`, the function calls [cobalt::bal.plot()] and returns density plots for the all covariates for control and treatment groups.
#' If the argument `love_plot` is set to `TRUE`, a Love plot of Standardizes Mean Difference is generated with [cobalt::bal.plot()].
#' 
#' @export
#' 
#' @importFrom cobalt bal.tab bal.plot love.plot
#' @import terra
#' 
#' @param match_object description
#' @param covariate_overlap logical. Plot covariate density plots to evaluate covariate overlap.
#' @param love_plot logical. Plot Love plot.
#' 
#' 

evaluate_matching <- function(match_object, covariate_overlap=TRUE, love_plot=TRUE){ #spatial_autocor=FALSE, hidden_bias=FALSE
  
  # if(isTRUE(spatial_autocor)){
  #   spat.ref <- match_object$spat.ref
  #   if(is.null(spat.ref)) stop("Spatial reference missing")
  # }
  
  if(!is.null(match_object$model)){
    obj <- match_object$model
  } else {
    obj <- match_object
  }
  
  var.names <- names(obj$X)
  
  ## Balance measures and Sample size
  cobalt::bal.tab(obj) |> print()
  wait <- readline(prompt = "Press <Enter> to continue.")
  
  ## Covariates overlap
  if(isTRUE(covariate_overlap)){
    for(var.name in var.names){
      p <- cobalt::bal.plot(obj, var.name=var.name, which="both")
      print(p)
      wait <- readline(prompt = "Press <Enter> to continue.")
    }
  }
  
  ## Love plot
  if(isTRUE(love_plot)){
    p <- cobalt::love.plot(obj, binary = "std", thresholds = c(m = .1))
    print(p)
    wait <- readline(prompt = "Press <Enter> to continue.")
  }
  
  
  # ## Spatial autocorrelation
  # if(isTRUE(spatial_autocor)){
  #   if(!is.SpatRaster(spat.ref)) stop("Only SpatRaster spatial reference implemented for spatial autocorrelation")
  #   
  #   resid <- obj$model$residuals
  #   cellnrs <- obj$model$data[,"cell"]
  #   resid_SpatRaster <- rast(spat.ref)
  #   resid_SpatRaster[cellnrs] <- resid
  #   
  # }
  
  # ## Hidden bias
  # if(isTRUE(hidden_bias)){
  #   
  #   
  #   
  #   
  # }
  

}





