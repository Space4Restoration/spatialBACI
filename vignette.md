EnvImpactEval
================

## Usage

### The Baviaanskloof dataset

We will use the Baviaanskloof dataset created by [del Río-Mena et al.,
2021](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0243020),
which is included in the package, to demonstrate the restoration impact
evaluation.

We will run the restoration impact evaluation for just one of the 118
sites in the Baviaanskloof dataset, which is selected by subsetting the
baviaanskloof SpatVector object. The plot shows the location of the
selected site in the overall project area.

``` r
library(EnvImpactEval)

data(baviaanskloof)
baviaanskloof <- unwrap(baviaanskloof)

s <- 2
site <- baviaanskloof[s,]

plot(baviaanskloof)
plot(site, add=TRUE, col="red")
```

![](vignette_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
