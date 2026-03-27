# EO cube from STAC

Create a data cube for EO images retrieved from a STAC catalog.

## Usage

``` r
eo_cube.stac(
  x,
  endpoint,
  collection,
  stacOptions = list(),
  t0,
  t1,
  dt = "P1D",
  asset_names = NULL,
  joint_assets = TRUE,
  maxCloud = NULL,
  maskOptions = list(NULL),
  aggregation = "first",
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

  list. Optional arguments to be passed to \[eo_stac_search()\], e.g.
  authentication options

- t0:

  start date as numeric, character or Date

- t1:

  end date as numeric, character or Date

- dt:

  size of pixels in time-direction, expressed as ISO8601 period string

- asset_names:

  character vector of asset (band) names to be used, or NULL for all
  asset names with "eo:bands" attributes

- joint_assets:

  logical indicating if only the assets (bands) present in all features
  should be used, if argument asset_names is NULL

- maxCloud:

  maximum cloud cover \[0-100\] of images to be included in data cube,
  only relevant for collections with "eo:cloud_cover" attributes

- maskOptions:

  list specifying layer used to create mask and masking options (see
  \[eo_mask.cube()\])

- aggregation:

  see \[gdalcubes::cube_view()\]

- resampling:

  see \[gdalcubes::cube_view()\]

- harm_bandnames:

  logical indicating whether endpoint/collection-specific band names
  should be changed to generic band names

## Value

proxy data cube object

## Details

t0 and t1 can be provided as numeric in format yyyymmdd, as a character
in the format "yyyymmdd" or "yyyy-mm-dd", or as a Date object. For now,
only date is implemented, not time.
