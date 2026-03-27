# Calculate Vegatation Index

Calculates Vegetation Index for a data cube of EO (surface reflectance)
images

## Usage

``` r
calc_VI.cube(cube, VI, scale = 1e-05)
```

## Arguments

- cube:

  data cube with spectral bands

- VI:

  vegetation index

- scale:

  scale of source surface reflectance data

## Value

proxy data cube object

## Details

Implemented vegetation indices:

- NDVI = (nir-red)/(nir+red)

- NDMI = (nir-swir1)/(nir+swir1)

- NBR = (nir-swir2)/(nir+swir2)

- NBR2 = (swir1-swir2)/(swir1+swir2)

- NDSI = (green-swir1)/(green+swir1)

- EVI = 2.5\*(nir-red)/(nir+6\*red-7.5\*blue+1)

- SAVI = 1.5\*(nir-red)/(nir+red+0.5)

- MSAVI = (2\*nir+1-sqrt((2\*nir+1)^2-8\*(nir-red)))/2

Scale factor has to be applied when reflectance saved as integer instead
of range \[0-1\]. To be checked for different endpoints
