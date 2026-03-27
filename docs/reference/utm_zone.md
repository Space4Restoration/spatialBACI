# Given a spatial object, calculate the UTM zone of the centroid

For a line or polygon, the UTM zone of the centroid is given, after
reprojecting the object into WGS-84.

## Usage

``` r
utm_zone(x, y, proj4string = FALSE)

# S4 method for class 'numeric,numeric'
utm_zone(x, y, proj4string = FALSE)

# S4 method for class 'SpatVector,missing'
utm_zone(x, proj4string)
```

## Arguments

- x:

  a longitude (with western hemisphere longitudes negative), a `Spatial`
  object, or a `SpatVector` object

- y:

  a latitude (with southern hemisphere latitudes negative), or missing
  (if x is a `Spatial` or `SpatVector` object)

- proj4string:

  if FALSE (default) return the UTM zone as a string (for example "34S"
  for UTM Zone 34 South). If TRUE, return a proj4string using the EPSG
  code as an initialization string.

## Value

character

## Examples

``` r
utm_zone(45, 10)
#> [1] "38N"
utm_zone(45, -10)
#> [1] "38S"
utm_zone(45, 10, proj4string=TRUE)
#> [1] "+init=epsg:32638"
```
