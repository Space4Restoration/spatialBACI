# Coerce data cube to SpatRaster

Writes a data cube as a temporal file and reads that as SpatRaster
object

## Usage

``` r
as.SpatRaster(x)
```

## Arguments

- x:

  data cube

## Value

(list of) SpatRaster object(s)

## Details

Data cubes with n\>1 in both the "time" and "bands" dimension will be
transformed to a list of SpatRaster objects with the list elements
corresponding to different time steps. Otherwise the function will
return a single (multi-layer) SpatRaster.
