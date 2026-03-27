# Calculate time series metrics from a SpatRaster

Calculate average and trend from a SpatRaster times series

## Usage

``` r
calc_ts_metrics.SpatRaster(
  rastTimeSeries,
  average = 1,
  trend = 1,
  immediate = 0
)
```

## Arguments

- rastTimeSeries:

  SpatRaster. Each layer represents a time step, provided in "time"
  field

- average:

  numeric. If not 0, calculate the time series average

- trend:

  numeric. If not 0, calculate the time series trend

- immediate:

  numeric. If not 0, calculate the time series component required to
  calculate the immediate effect. Provide negative value for the
  "before", positive value for the "after" time series.

## Value

SpatRaster with selected parameters as layers
