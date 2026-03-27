# Extract matches values from SpatRaster/data cube

Helper function to extract matches from SpatRaster or data.cube object

## Usage

``` r
extractMatches(r, obj, fun, method = "simple", ...)
```

## Arguments

- r:

  a SpatRaster or data.cube raster object.

- obj:

  output of \[matchCI()\].

- fun:

  function to summarize extracted data by line or polygon geometries.
  See \[terra::extract()\].

- method:

  character. Method for extracting values for point geometries. See
  \[terra::extract()\].

- ...:

  additional arguments to \`fun\`.

## Value

data.table of extracted raster values for control-impact pairs.
