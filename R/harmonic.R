
#' Fit harmonic
#' 
#' Fit a harmonic function to a time series
#' 
#' Simple function to fit harmonic function to a time series (variable in x, time in y).
#' 
#' @noRd
#' @export
#' 


harmonic <- function(x, y, nHarmonic=1, x_0=min(x), period=365.25){
  
  df <- data.frame(vals=y, const=rep(1, length(y)), t=x-x_0)
  for(j in 1:nHarmonic){
    dfj <- data.frame(sin(df$t*j*2*pi/period),
                      cos(df$t*j*2*pi/period))
    names(dfj) <- paste0(c("sin", "cos"),j)
    df <- cbind(df, dfj)
  }

  # Ordinary Least Squares Regression
  linmod <- lm(formula = vals ~ ., data = df, na.action=na.omit)
  
  #Get harmonic coefficients
  coeffs <- numeric(2+nHarmonic*2)
  names(coeffs) <- c("intercept", "slope", c(outer(c("amplitude", "phase"), 1:nHarmonic, paste0)))
  coeffs[1] <- linmod$coefficients[1]
  coeffs[2] <- linmod$coefficients[3]
  for(j in 1:nHarmonic){
    coeffs[2*(j-1)+3] <- sqrt(linmod$coefficients[4+2*(j-1)]^2 + linmod$coefficients[4+2*(j-1)+1]^2)
    coeffs[2*(j-1)+4] <- atan2(linmod$coefficients[5+2*(j-1)], linmod$coefficients[4+2*(j-1)])
  }

  # Calculate fitted values
  fitted_values <- data.frame(intercept=rep(coeffs[1], nrow(df)), slope=df$t*coeffs[2])
  for(j in 1:nHarmonic){
    fit_j <- data.frame(coeffs[2*(j-1)+3] * sin(j*df$t*2*pi/period + coeffs[2*(j-1)+4]))
    names(fit_j) <- paste0("Harmonic_", j)
    fitted_values <- cbind(fitted_values, fit_j)
  }
  
  return(list(x=x,
              y=y,
              x_0=x_0,
              coefficients=coeffs,
              fitted_values=fitted_values
  ))
}
