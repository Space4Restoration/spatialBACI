# Before-After-Control-Impact contrast

Calculate the (Before-After-)Control-Impact contrast and p-value

After \[Meroni et al.
(2017)\](https://doi.org/10.1016/j.jag.2017.02.016) and \[del Río-Mena
et al. (2021)\](https://doi.org/10.1371/journal.pone.0243020)

## Usage

``` r
BACI_contrast(
  ci_matches,
  before,
  after,
  effect,
  colname.id,
  colname.treatment,
  colname.subclass = "subclass",
  fun,
  method = "simple",
  ...
)
```

## Arguments

- ci_matches:

  list. Control-impact pairs and spatial reference, output of
  \[matchCI()\]

- before:

  SpatRaster or data.cube of pre-intervention observable of interest

- after:

  SpatRaster or data.cube of post-intervention observable of interest

- effect:

  SpatRaster or data.cube of effect observable of interest. See Details.

- colname.id:

  character. Name of the column containing the unique ID of the spatial
  unit of analysis in \`ci_matches\`. Defaults to "cell" for SpatRaster
  input in \`ci_matches\`.

- colname.treatment:

  character. Name of the column containing the treatment variable in
  \`ci_matches\`. Defaults to "treatment" for SpatRaster input in
  \`ci_matches\`.

- colname.subclass:

  character. Name of the column containing the subclass assignemt in
  \`ci_matches\`. Defaults to "subclass".

- fun:

  function to summarize extracted data by line or polygon geometries.
  See \[terra::extract()\].

- method:

  character. Method for extracting values for point geometries. See
  \[terra::extract()\].

- ...:

  additional arguments to \`fun\`.

## Value

list (See Details)

## Details

An "effect" raster of the variable of interest can be provided instead
of the pre-intervention and post-intervention variable, the effect
variable takes priority if all are provided. The function returns a list
with named elements \`data\`, containing (BA)CI contrast and p-value for
each impact unit of analysis as a data.table, and \`spat\`, containing
the same data as a spatial object.

## References

Meroni, M., Schucknecht, A., Fasbender, D., Rembold, F., Fava, F.,
Mauclaire, M., Goffner, D., Di Lucchio, L.M., Leonardi, U., 2017. Remote
sensing monitoring of land restoration interventions in semi-arid
environments with a before–after control-impact statistical design.
International Journal of Applied Earth Observation and Geoinformation
59, 42–52.

del Río-Mena, T., Willemen, L., Vrieling, A., Snoeys, A., Nelson, A.,
2021. Long-term assessment of ecosystem services at ecological
restoration sites using Landsat time series. PLOS ONE 16, e0243020.
