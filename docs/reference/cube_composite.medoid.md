# Create medoid composite image

Create a pseudo-medoid composite image from a data cube

## Usage

``` r
cube_composite.medoid(cube)
```

## Arguments

- cube:

  source data cube

## Value

output data cube

## Details

This compositing method uses aan approximating of the medoid, selecting
the observation with the smallest euclidean distance to the band-wise
median position.
