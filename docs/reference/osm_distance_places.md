# OpenStreetMap distance to places

Calculated shortest geographic distance to OpenStreetMap place features

## Usage

``` r
osm_distance_places(
  x,
  values = "hamlet+",
  timeout = 25,
  name = NULL,
  osm_bbox = NULL
)
```

## Arguments

- x:

  SpatRaster or SpatVector for which distances are calculated

- values:

  Character vector of place categories to be included (see
  https://wiki.openstreetmap.org/wiki/Key:place) or a single place
  category with suffix "+" to include all categories including and above
  the selected category. Set to NULL to include all values.

- timeout:

  Numeric (seconds)

- name:

  Character of place name

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
