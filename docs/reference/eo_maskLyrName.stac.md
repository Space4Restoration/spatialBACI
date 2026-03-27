# Retrieve mask layer name

This function returns the metadata layer name from which a mask can be
derived for a collection at a STAC endpoint

## Usage

``` r
eo_maskLyrName.stac(endpoint, collection)
```

## Arguments

- endpoint:

  STAC endpoint

- collection:

  STAC collection

## Value

character or NULL

## Details

Currently only implemented for collections "landsat-c2-l2" and
"sentinel-2-l2a" at Planetary Computer. Returns NULL for all other
values. Additional endpoints/collections will be added.
