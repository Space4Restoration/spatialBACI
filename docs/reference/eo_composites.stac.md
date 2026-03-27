# Image composites from STAC

Create EO image composite from STAC collection.

## Usage

``` r
eo_composites.stac(
  x,
  endpoint,
  collection,
  stacOptions = list(limit = 1000, authOpt = list()),
  compositing_period,
  method = "median",
  asset_names = NULL,
  joint_assets = TRUE,
  maxCloud = NULL,
  maskOptions = list(maskLyr = eo_maskLyrName.stac(endpoint, collection), maskSnow =
    FALSE, maskWater = FALSE),
  ...
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

- compositing_period:

  compositing period. See Details

- method:

  compositing method, see cube_composite

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
  eo_mask.cube)

- ...:

  additional arguments passed to to eo_cube

## Value

(list of) proxy data cube object

## Details

Compositing period can be specified as a two-element vector t0-t1 pair
(see eo_cube), as a list of t0-t1 pairs, or a list with named elements
"years" and, optionally, "months". If a list of t0-t1 pairs is provided,
the function will return a list of proxy data cube objects, where each
list element is the image composite for the corresponding t0-t1 pair. If
a named list with element "years" is provided, a composite image for
each year provided in this list argument is created; the compositing
period within each year can be specified by including the named list
element "months", in which case the provided months will be used to
refine the compositing period.
