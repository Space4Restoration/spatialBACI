# Calculate contrast and p value

Calculate the (BA)CI contrast and p-value for matched control-impact
units

## Usage

``` r
calc_contrast_p(data, colname.subclass, colname.treatment)
```

## Arguments

- data:

  data.table or data.frame. See details.

- colname.subclass:

  character. Name of column indicating subclass

- colname.treatment:

  character. Name of column indicating treatment

## Details

Parameter `data` must be a data.table (or data.frame) with columns
indicating the subclass and treatment, the remaining columns will be
assumed to be the effects for which contrast and p-value must be
calculated
