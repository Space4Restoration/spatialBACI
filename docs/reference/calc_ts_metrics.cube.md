# Calculate time series metrics from a data cube

Calculate average, trend, and intercepts from a multitemporal data cube

## Usage

``` r
calc_ts_metrics.cube(
  cube,
  average = TRUE,
  trend = TRUE,
  intercept_tmin = FALSE,
  intercept_tmax = FALSE
)
```

## Arguments

- cube:

  data cube with a t dimension and single band.

- average:

  logical. Calculate the time series average.

- trend:

  logical. Calculate the time series trend (= slope of linear
  regression).

- intercept_tmin:

  logical. Calculate the intercept at tmin.

- intercept_tmax:

  logical. Calculate the intercept at tmax.

## Value

data cube
