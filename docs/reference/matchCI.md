# Control-impact matching

Match control units to impact/treatment units for vector or raster data.
Wrapper for the \[MatchIt::matchit()\] function and the matching options
provided therein.

## Usage

``` r
matchCI(
  matching_input,
  colname.id,
  colname.treatment,
  colnames.ignore = NULL,
  ...
)
```

## Arguments

- matching_input:

  SpatVector or data.table. Output of \[collate_matching_layers()\].

- colname.id:

  character. Name of the column containing the unique ID of the spatial
  unit of analysis. Defaults to "cell" if \`matching_input\` is a
  data.table (representing raster data).

- colname.treatment:

  character. Name of the column containing the treatment variable.
  Defaults to "treatment" if \`matching_input\` is a data.table
  (representing raster data).

- colnames.ignore:

  character vector. Name of columns (other than \`colname.id\` and
  \`colname.treatment\`) in \`matching_input\` to be ignored as matching
  covariates.

- ...:

  additional inputs to \[MatchIt::matchit()\], e.g., \`ratio\`,
  \`replace\`, \`method\`, \`distance\`, or \`link\`.

## Value

named list with list elements \`model\`, \`matches\`, and \`spat.ref\`.
See Details.

## Details

The function returns a names list.

The list element \`model\` is a \`matchit\` object returned by the
\[MatchIt::matchit()\] function. This object can be used to assess the
quality of the matching, e.g., through assessing the covariance balance
in \[evaluate_matching()\]. The list element \`matches\` is a data.table
containing the matched control-impact pairs. Column names of this
data.table are those defined by the \`colname.id\`,
\`colname.treatment\` parameters, and "subclass". The list element
\`spat.ref\` is copied from the same list element in \`matching_input\`.
