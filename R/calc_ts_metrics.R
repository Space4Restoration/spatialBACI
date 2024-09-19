
#' Calculate time series metrics
#' 
#' Generic function for \code{calc_ts_metrics.cube} and \code{calc_ts_metrics.spatRaster}
#' 
#' Currently, there are some differences in the inputs/outputs of the functions for data cube and spatRaster, this has to be harmonized.
#' 
#' @export
#' 
#' @param x data cube or spatRaster
#' @param ... additional arguments passed to \code{calc_ts_metrics.cube} or \code{calc_ts_metrics.rast}
#' 
#' 
calc_ts_metrics <- function(x, ...) {
  UseMethod("calc_ts_metrics")
}



#' Calculate time series metrics from a data cube
#' 
#' Calculate average, trend, and intercepts from a multitemporal data cube
#' 
#' @importFrom gdalcubes reduce_time join_bands select_bands
#'
#' @export
#' 
#' @param cube data cube with a t dimension and single band.
#' @param average logical. Calculate the time series average.
#' @param trend logical. Calculate the time series trend (= slope of linear regression).
#' @param intercept_tmin logical. Calculate the intercept at tmin.
#' @param intercept_tmax logical. Calculate the intercept at tmax.
#' 
#' 
#' @returns data cube
#' 
calc_ts_metrics.cube <- function(cube, average=TRUE, trend=TRUE, intercept_tmin=FALSE, intercept_tmax=FALSE){
  
  out_list <- list()
  
  if(isTRUE(average) & !isTRUE(trend)){
    out <- gdalcubes::reduce_time(cube, names="mean", 
                                               FUN=function(x){mean(x, na.rm=TRUE)})
  } else if (trend==1){
    out <- gdalcubes::reduce_time(cube, names=c("average", "trend", "intercept_tmin", "intercept_tmax"),
                                  FUN=function(y){

                                    df <- data.frame(x=seq(0, ncol(y)-1, 1), y=y[1,])
                                    df <- na.omit(df)

                                    N <- nrow(df)
                                    b <- (N*sum(df$x*df$y) - sum(df$x)*sum(df$y)) / (N*sum(df$x^2) - sum(df$x)^2)
                                    a <- (sum(df$y) - b*sum(df$x))/N

                                    trend <- b
                                    intercept_tmin <- a
                                    intercept_tmax <- a+ncol(y)*b
                                    average <- mean(df$y)

                                    return(c(average, trend, intercept_tmin, intercept_tmax))
                                  })
  
    sbands <- c()
    if(average==1) sbands <- c(sbands, "average")
    if(trend==1) sbands <- c(sbands, "trend")
    if(intercept_tmin) sbands <- c(sbands, "intercept_tmin")
    if(intercept_tmax) sbands <- c(sbands, "intercept_tmax")
    
    out <- gdalcubes::select_bands(out, sbands)
  }

  return(out)
}


#' Calculate time series metrics from a SpatRaster
#' 
#' Calculate average and trend from a SpatRaster times series
#' 
#' @importFrom terra rast app subset time
#'
#' @export
#' 
#' @param rastTimeSeries SpatRaster. Each layer represents a time step, provided in "time" field
#' @param average numeric. If not 0, calculate the time series average
#' @param trend numeric. If not 0, calculate the time series trend
#' @param immediate numeric. If not 0, calculate the time series component required to calculate the immediate effect. Provide negative value for the "before", positive value for the "after" time series.
#' 
#' @returns SpatRaster with selected parameters as layers
#' 
calc_ts_metrics.SpatRaster <- function(rastTimeSeries, average=1, trend=1, immediate=0){

  out <- list()
  
  if(average!=0){
    out$average <- app(rastTimeSeries, "mean", na.rm=TRUE)
  }
  
  if(trend!=0 | immediate!=0){
    app_lm <- function(ts){
      x <- time(ts)
      Y <- ts
      
      sum_x <- sum(x)
      sum_Y <- app(Y, "sum", na.rm=TRUE)
      sum_xY <- app(x*Y, "sum", na.rm=TRUE)
      sum_xsq <- sum(x^2)
      N <- app(!is.na(Y), "sum")
      
      b <- (N*sum_xY - sum_x*sum_Y)/(N*sum_xsq - sum_x^2)
      
      a <- (sum_Y - b*sum_x)/N
      
      return(rast(list(intercept=a, slope=b)))
    }
    int_slope <- app_lm(rastTimeSeries)
    
    
    if(trend!=0){
      out$trend <- subset(int_slope,2)
    }
    
    if(immediate < 0){
      out$immediate <- subset(int_slope,1) + max(time(rastTimeSeries))* subset(int_slope,2)
    } else if(immediate > 0){
      out$immediate <- subset(int_slope,1) + min(time(rastTimeSeries))* subset(int_slope,2)
    }
  }
  return(rast(out))
}
