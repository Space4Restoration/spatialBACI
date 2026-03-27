# Yearly EO cube from STAC

Create a data cube with yearly time intervals for EO images retrieved
from a STAC catalog.

## Usage

``` r
eo_cube_yearly.stac(
  x,
  endpoint,
  collection,
  stacOptions = list(),
  years,
  months,
  asset_names = NULL,
  joint_assets = TRUE,
  maxCloud = NULL,
  maskOptions = list(NULL),
  aggregation = "median",
  resampling = "bilinear",
  harm_bandnames = TRUE
)
```

## Arguments

- x:

  spatRaster. Defines spatial reference of the output data cube

- endpoint:

  character. STAC endpoint

- collection:

  character. STAC collection

- stacOptions:

  list. Optional arguments to be passed to eo_stac_search, e.g.
  authentication options

- years:

  description

- months:

  description

- asset_names:

  character vector of asset (band) names to be used, or NULL for all
  asset names with "eo:bands" attributes

- joint_assets:

  logical indicating if only the assets (bands) present in all features
  should be used, if argument asset_names is NULL

- maxCloud:

  numeric. Maximum cloud cover \[0-100\] of images to be included in
  data cube, only relevant for collections with "eo:cloud_cover"
  attributes

- maskOptions:

  list specifying layer used to create mask and masking options (see
  eo_mask.cube)

- aggregation:

  see gdalcubes::cube_view

- resampling:

  see gdalcubes::cube_view

- harm_bandnames:

  logical indicating whether endpoint/collection-specific band names
  should be changed to generic band names

## Value

proxy data cube object

## Details

This function bypasses limits on the length of the queried STAC
itemCollection by posting subsequent queries for each year
