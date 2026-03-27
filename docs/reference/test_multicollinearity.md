# Test multicollinearity of matching covariates

The function provides a correlation plot of the (numeric) covariates,
and iteratively reports (Generalized) Variance Inflation Factors of the
different covariates. After each iteration, the used is prompted whether
a matching covariate should be omitted.

## Usage

``` r
test_multicollinearity(
  matching_input,
  colname.id,
  colname.treatment,
  colnames.ignore = NULL
)
```

## Arguments

- matching_input:

  list with names elements "data" and "spat.ref". Output of
  \[collate_matching_layers()\].

- colname.id:

  character. Name of the column containing the unique ID of the spatial
  unit of analysis. Defaults to "cell" if \`matching_input\$spat.ref\`
  is a SpatRaster.

- colname.treatment:

  character. Name of the column containing the treatment variable.
  Defaults to "treatment" if \`matching_input\$spat.ref\` is a
  SpatRaster

- colnames.ignore:

  character vector. Name of columns (other than \`colname.id\` and
  \`colname.treatment\`) in \`matching_input\` to be ignored as matching
  covariates.

## Value

list. Similar to \`matching_input\`, with optionally covariates in the
data.table under "data" omitted.
