#' Calculate time series metrics
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
calc_ts_metrics <- function(rastTimeSeries, average=1, trend=1, immediate=0){

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
