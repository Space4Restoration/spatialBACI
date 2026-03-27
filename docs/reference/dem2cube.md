# Read DEM as data cube

Read a DEM from a STAC collection or from file as a gdalcubes data cube

## Usage

``` r
dem2cube(
  xmin,
  xmax,
  ymin,
  ymax,
  dx,
  dy,
  srs,
  endpoint,
  collection,
  assets,
  authOpt = list(),
  ...
)
```

## Arguments

- xmin:

  numeric. Minimum x-coordinate of output DEM

- xmax:

  numeric. Maximum x-coordinate of output DEM

- ymin:

  numeric. Minimum y-coordinate of output DEM

- ymax:

  numeric. Maximum y-coordinate of output DEM

- dx:

  numeric. Pixels size of output DEM in x-dimension

- dy:

  numeric. Pixels size of output DEM in y-dimension

- srs:

  character. Description of spatial reference system

- endpoint:

  character. STAC endpoint

- collection:

  character. STAC collection

- assets:

  character. STAC asset name

- authOpt:

  STAC authentication options (endpoint-dependent)

- ...:

  additional arguments (not used)

## Value

a gdalcubes data cube
