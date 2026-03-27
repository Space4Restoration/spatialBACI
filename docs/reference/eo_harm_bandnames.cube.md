# Harmonize STAC endpoint/collection-specific band names of data cubes

Replaces the STAC endpoint/collection-specific band names of a data cube
by generic band names (i.e., "blue", "green", "red", "nir", ...). This
simplifies the calculation of Vegetation Indices,

## Usage

``` r
eo_harm_bandnames.cube(cube, endpoint, collection)
```

## Arguments

- cube:

  a data cube

- endpoint:

  character. STAC endpoint

- collection:

  character. STAC collection
