# Map band

Switch between generic and endpoint/collection-specific band name

## Usage

``` r
eo_mapBand(bandnames, endpoint, collection, invert = FALSE)
```

## Arguments

- bandnames:

  character vector of bandnames

- endpoint:

  character. STAC endpoint

- collection:

  character. STAC collection

- invert:

  logical. Set to TRUE to obtain endpoint-specific bandnames from
  generic names

## Value

character vector

## Details

Limited number of endpoints/collections implemented, more to be added
