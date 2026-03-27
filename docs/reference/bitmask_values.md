# Landsat L2 QA bitmap values

Define the values of values to be masked from Landsat L2 products based
on specified QA bits

## Usage

``` r
bitmask_values(
  nBit = 8,
  fill = TRUE,
  dilCloud = TRUE,
  cirrus = TRUE,
  cloud = TRUE,
  cloudShadow = TRUE,
  snow = FALSE,
  water = FALSE
)
```

## Arguments

- nBit:

  integer. Number of bits from bitmask used, defaults to 8

- fill:

  logical. Mask fill bit

- dilCloud:

  logical. Mask dilated cloud bit

- cirrus:

  logical. Mask cirrus bit

- cloud:

  logical. Mask cloud bit

- cloudShadow:

  logical. Mask cloud shadow bit

- snow:

  logical. Mask snow bit

- water:

  logical. Mask water bit

## Value

integer vector
