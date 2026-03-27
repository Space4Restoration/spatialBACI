# Vegetation Index composites

Create yearly composite image time series of a selected vegetation index

## Usage

``` r
eo_VI_yearly.stac(
  x,
  VI,
  endpoint,
  collection,
  stacOptions = list(limit = 1000, authOpt = list()),
  years,
  months,
  maxCloud = NULL,
  maskOptions = list(maskLyr = eo_maskLyrName.stac(endpoint, collection), maskSnow =
    FALSE, maskWater = FALSE)
)
```

## Arguments

- x:

  SpatRaster

- VI:

  Vegetation Index (see details)

- endpoint:

  STAC endpoint

- collection:

  STAC collection

- stacOptions:

  list. Optional arguments to be passed to \[eo_stac_search()\], e.g.
  authentication options

- years:

  numeric vector of years for which composite image must be created

- months:

  numeric vector of months for which composite image must be created for
  each years

- maxCloud:

  numeric, maximum cloud cover percentage

- maskOptions:

  options defining (cloud) mask to be applied. See eo_mask.cube

## Value

SpatRaster

## Details

Limited functionalities for now, see `calc_VI.cube` for implemented
vegetation indices:
