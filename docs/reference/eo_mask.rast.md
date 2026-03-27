# EO mask for spatRaster

Create mask from Landsat QA layer or Sentinel-2 SCL terra::spatRaster
objects

## Usage

``` r
eo_mask.rast(r, maskSnow = FALSE, maskWater = FALSE)
```

## Arguments

- r:

  a spatRaster object containing a layer names "qa_pixel" (for Landsat
  mask) or "SCL" (for Sentinel-2 mask)

- maskSnow:

  logical. Defines whether snow should be masked.

- maskWater:

  logical. Defines whether water should be masked.

## Value

a spatRaster mask with NA for values to be masked and 0 for other values

## Details

The metadata layer from which the mask is derived should be specified in
`maskLyr`. For now, mask derived from "qa_pixel" (for Landsat) and "SCL"
(for Sentinel-2) are implemented. Clouds, cloud shadow and pixels
adjacent to clouds are always masked (when these fields are specified in
the masking layer). Snow and water can be masked by setting the
respective parameters, by default these are not masked
