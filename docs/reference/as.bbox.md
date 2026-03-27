# As bounding box

Create a Bounding Box from a SpatRaster, SpatVector, or SpatExtent
object in target CRS. Bounding box is a 4-element vector in the order
c(xmin, ymin, xmax, ymax)

## Usage

``` r
as.bbox(x, ...)

# S4 method for class 'SpatRaster'
as.bbox(x, crs_to = crs("epsg:4326"))

# S4 method for class 'SpatVector'
as.bbox(x, crs_to = crs("epsg:4326"))

# S4 method for class 'SpatExtent'
as.bbox(x, crs_from = crs("epsg:4326"), crs_to = crs("epsg:4326"))

# S4 method for class 'numeric'
as.bbox(
  x,
  ...,
  crs_from = crs("epsg:4326"),
  crs_to = crs("epsg:4326"),
  xy = TRUE
)
```

## Arguments

- x:

  a SpatRaster, SpatVector, SpatExtent, or numeric

- ...:

  if x is a numeric, additional values for ymin, xmax, ymax

- crs_to:

  character. Description of the Coordinate Reference System of the
  created bounding box, in PROJ.4, WKT or authority:code notation.
  Defaults to longitude/latitude.

- crs_from:

  If x has no CRS attached (i.e., if x is a SpatExtend or numeric), a
  character description of the Coordinate Reference System corresponding
  to the calues of x. Defaults to longitude/latitude.

- xy:

  logical. If x is a numeric vector of length 4 (or 4 numeric values),
  set this parameter to TRUE if coordinates are in order (xmin, ymin,
  xmax, ymax) or FALSE if in order (xmin, xmax, ymin, ymax).

## Value

numeric vector of length four
