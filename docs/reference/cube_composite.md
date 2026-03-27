# Composite cube

Create a pixel-based composite image from a multitemporal data cube

## Usage

``` r
cube_composite(cube, method = "median", ...)
```

## Arguments

- cube:

  a multitemporal data cube

- method:

  compositing method: "median" or "medoid"

- ...:

  optional arguments for specific methods

## Value

data cube

## Details

See cube_composite.median, cube_composite.medoid
