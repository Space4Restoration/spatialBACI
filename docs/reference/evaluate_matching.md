# Matching evaluation

Methods for evaluating matching results

## Usage

``` r
evaluate_matching(match_object, covariate_overlap = TRUE, love_plot = TRUE)
```

## Arguments

- match_object:

  description

- covariate_overlap:

  logical. Plot covariate density plots to evaluate covariate overlap.

- love_plot:

  logical. Plot Love plot.

## Details

This function uses the \[cobalt::bal.tab()\] function to generate
balance statistics from the \[MatchIt::matchit()\] call. If the argument
\`covariate_overlap\` is set to \`TRUE\`, the function calls
\[cobalt::bal.plot()\] and returns density plots for the all covariates
for control and treatment groups. If the argument \`love_plot\` is set
to \`TRUE\`, a Love plot of Standardizes Mean Difference is generated
with \[cobalt::bal.plot()\].
