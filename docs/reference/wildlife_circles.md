# Wildlife circles data

Simplified subset of the Finnish wildlife triangles dataset.

## Usage

``` r
data(wildlife_circles)
```

## Format

A SpatVector object with 671 polygon geometries and 2 attributes

- ID:

  Numeric of unique ID of the polygon gemetries (1–671)

- PA:

  Numeric indicating whether polygon is within WDPA protected area (1)
  or not (0)

## Source

Subset of 610 control sites and 61 impact site available in the
complementary data to [Terraube et el.
(2020)](https://www.nature.com/articles/s41467-020-16792-7).

## References

[Helle et al.
(2016)](https://cdnsciencepub.com/doi/10.1139/cjfr-2015-0454), [Terraube
et el. (2020)](https://www.nature.com/articles/s41467-020-16792-7)

## Examples

``` r
data(wildlife_circles)
wildlife_circles <- terra::unwrap(wildlife_circles)
terra::plot(wildlife_circles, "PA")

```
