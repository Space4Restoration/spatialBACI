# Search STAC EO data for Planetary Computer

Search STAC EO data for Planetary Computer

## Usage

``` r
eo_stac_search.PlanetaryComputer(
  collection = NULL,
  ids = NULL,
  bbox = NULL,
  datetime = NULL,
  intersects = NULL,
  limit = NULL,
  eocc = NULL,
  sat.orbit.state = NULL,
  authOpt = list()
)
```

## Arguments

- collection:

  STAC collection

- ids:

  see \[rstac::search()\]

- bbox:

  see \[rstac::search()\]

- datetime:

  see \[rstac::search()\]

- intersects:

  see \[rstac::search()\]

- limit:

  see \[rstac::search()\]

- eocc:

  maximum cloud cover (for "eo:cloud_cover" field)

- sat.orbit.state:

  character. optional argument for filtering "ascending" or "descending"
  states

- authOpt:

  list specifying STAC authentication options (e.g. list(key="abc"))

## Value

STACItemCollection
