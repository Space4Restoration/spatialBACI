# Extract Digital Elevation Model

Read Digital Elevation Model and calculate derivatives.

## Usage

``` r
dem(
  x,
  dem_source,
  v = c("elevation", "slope", "aspect"),
  neighbors = 8,
  unit = "radians",
  transformAspect = TRUE,
  ...
)

# S4 method for class 'SpatRaster'
dem(
  x,
  dem_source,
  v = c("elevation", "slope", "aspect"),
  neighbors = 8,
  unit = "radians",
  transformAspect = TRUE
)

# S4 method for class 'SpatExtent'
dem(
  x,
  dx,
  dy,
  srs,
  dem_source,
  v = c("elevation", "slope", "aspect"),
  neighbors = 8,
  unit = "radians",
  transformAspect = TRUE
)

# S4 method for class 'SpatVector'
dem(
  x,
  dx,
  dy,
  dem_source,
  v = c("elevation", "slope", "aspect"),
  neighbors = 8,
  unit = "radians",
  transformAspect = TRUE
)
```

## Arguments

- x:

  a SpatRaster, spatVector, or SpatExtent object, defining the area for
  which the DEM should be extracted

- dem_source:

  Source of the Digital Elevation Model, see Details

- v:

  character vector. Terrain parameters to be calculated

- neighbors:

  integer. Either 8 (queen case) or 4 (rook case), indicating which
  neighbors to use to compute slope or aspect.

- unit:

  character. "degrees" or "radians" for the output of slope and aspect

- transformAspect:

  logical. Should aspect be transformed to "northness" and "eastness"

- ...:

  Additional arguments passed to the relevant method

- dx:

  numeric. Output resolution in x dimension

- dy:

  numeric. Output resolution in y dimension

- srs:

  character. output spatial reference system

## Value

a data cube, SpatRaster or SpatVector object (see Details)

## Details

This function returns a data cube object when only the digital terrain
model is required (i.e. `v="elevation"`), and a SpatRaster when the
terrain derivatived are required. The reason for this is that
calculating terrain parameters using the terra::terrain function
outperforms the same calculation on data cube, with regard to time.

`dem_source` must be a list with STAC specifications (named list with
list elements "endpoint", "collection", "assets" and (optionally)
"authOpt". If no DEM source it provided, this argument defaults to
`list(endpoint="https://planetarycomputer.microsoft.com/api/stac/v1", collection="nasadem", assets="elevation", authOpt=list())`

Examples of available global DEMs with STAC endpoint/collection/assets
specification and whether authentication is required to access the DEM:

|  |  |  |  |  |
|----|----|----|----|----|
| name | endpoint | collection | assets | auth. |
|  |  |  |  |  |
| **NASA DEM (default)** | https://planetarycomputer.microsoft.com/api/stac/v1 | "nasadem" | "elevation" | no |
| ALOS World DEM 30m | https://planetarycomputer.microsoft.com/api/stac/v1 | "alos-dem" | "data" | no |
| Copernicus DEM 30m | https://planetarycomputer.microsoft.com/api/stac/v1 | "cop-dem-glo-30" | "data" | yes |
| Copernicus DEM 90m | https://planetarycomputer.microsoft.com/api/stac/v1 | "cop-dem-glo-90" | "data" | yes |
