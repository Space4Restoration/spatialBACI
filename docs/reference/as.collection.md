# As collection

Format to STAC collection name for some commonly used collections and
STAC endpoints

## Usage

``` r
as.collection(x, endpoint = as.endpoint("PlanetaryComputer"))
```

## Arguments

- x:

  character. Simplified collection name (see Details)

- endpoint:

  character. STAC endpoint (see as.endpoint)

## Value

character

## Details

Possible values:

For endpoint=as.endpoint("PlanetaryComputer"):

- For collection "landsat-c2-l2": "landsat", "landsat-c2-l2"

- For collection "sentinel-2-l2a": "s2", "sentinel2", "sentinel-2-l2a"
