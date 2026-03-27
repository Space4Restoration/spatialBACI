# Extract Land Use / Land Cover data

Read a Land Use or Land Cover layer

## Usage

``` r
lulc(
  x,
  year,
  endpoint = "https://planetarycomputer.microsoft.com/api/stac/v1",
  collection = "io-lulc-9-class",
  assets = "data",
  authOpt = list()
)

# S4 method for class 'SpatRaster'
lulc(
  x,
  year,
  endpoint = "https://planetarycomputer.microsoft.com/api/stac/v1",
  collection = "io-lulc-9-class",
  assets = "data",
  authOpt = list()
)
```

## Arguments

- x:

  a SpatRaster object, defining the area for which the LULC should be
  extracted

- year:

  numeric defining year of LULC layer (see Details)

- endpoint:

  STAC endpoint, defaults to Microsoft Planetary Computer

- collection:

  STAC collection, defaults to "io-lulc-9-class"

- assets:

  character defining STAC assets name, defaults to "data"

- authOpt:

  list with STAC authentication options passed to stac_auth (e.g.
  `list(key="abc")`), endpoint-specific

## Value

SpatRaster object

## Details

Designed to be flexible with regard to STAC endpoint and collection, but
for now only tested for io-lulc-9-class on PlanetaryComputer
io-lulc-9-class is only generated for the period 2017-2022, for years
outside this range a warning will be returned and the nearest year will
be selected.
