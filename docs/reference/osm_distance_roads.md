# OpenStreetMap distance to roads

Calculated shortest geographic distance to OpenStreetMap road features

## Usage

``` r
osm_distance_roads(x, values = "residential+", timeout = 25, osm_bbox = NULL)
```

## Arguments

- x:

  SpatRaster or SpatVector for which distances are calculated

- values:

  Character vector of road categories to be included (see
  https://wiki.openstreetmap.org/wiki/Key:highway) or a single road
  category with suffix "+" to include all categories including and above
  the selected category. Set to NULL to include all values.

- timeout:

  Numeric (seconds)

- osm_bbox:

  Bounding box for OSM feature retrieval, as numeric vector or character
  string of administrative name (see
  [`osmdata::getbb`](https://docs.ropensci.org/osmdata/reference/getbb.html))

## Value

SpatRaster or SpatVector

## Details

When a bounding box is not provided in the osm_bbox argument, the
maximum required bounding box will be identified through an iterative
search. This may be a more efficient option than providing an overly
wide bounding box.
