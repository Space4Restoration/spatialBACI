EnvImpactEval
================

## Usage

### The Baviaanskloof dataset

We will use the Baviaanskloof dataset created by [del Río-Mena et al.,
2021](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0243020),
which is included in the package, to demonstrate the restoration impact
evaluation. The dataset contains the sites of large-scale spekboom
revegetation, and indicates in which year the revegetation was
undertaken. We here select the sites revegetated in 2012 that were not
subject to lifestock grazing exclusion to assess the effectiveness of
the restoration actions.

``` r
library(EnvImpactEval)

data(baviaanskloof)
baviaanskloof <- unwrap(baviaanskloof)

plot(baviaanskloof, "Planting_date")
```

![](vignette_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
year <- 2012
selected_sites <- baviaanskloof[baviaanskloof$Planting_date==year & baviaanskloof$Lifestock_exclusion==0]
other_sites <- baviaanskloof[baviaanskloof$Planting_date!=year | baviaanskloof$Lifestock_exclusion==1]
```

### Spatial reference and the candidate control pixels

The package requires the user to define the spatial parameters at which
the analysis will be performed. For now, only a raster-based framework
is implemented. The user must provide the Coordinate Reference System of
the raster (by default the UTM zone of the site’s centroid), the spatial
resolution (which is typically linked to the spatial resolution of EO
datasets used in the analysis), and the parameters that define from
which area control pixels for a control-impact or
before-after-control-impact will be selected.

We here choose to restrict the search area for potential control units
to a bounding box with a buffer of 3 km around the sites reforested in
2012. Furthermore, in order to avoid undesired effects of spatial
misregistration or adjacency effects, we exclude as control units those
pixels in a radius of 150 m around the selected impact sites. Since we
will use metrics derived from Landsat data to quantify restoration
effectiveness, we selected a corresponding spatial resolution of 30 as
the basis of our analysis. We can now plot a map showing the impact
pixels (in green), the potential control pixels (in grey), and the
excluded pixels (in white). Notice that the map shows UTM coordinates,
as this was by default selected as the Coordinate Reference System.

``` r
refRas <- ref_rast(selected_sites, resolution=30, buffer=3000, round_coords=-2)
matchingCands <- matchCandidates(selected_sites, refRas, excludeBufferOut=150, excludeOther = other_sites)
plot(matchingCands)
plot(project(selected_sites, refRas), add=TRUE)
```

![](vignette_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Evaluation metric

For this example, we chose Landsat Normalized Difference Vegetation
Index (NDVI) as the metric to use in the impact evaluation. NDVI is
indicative of vegetation “greenness” and could therefore inform us
whether a large scale replanting effort was successful. The Landsat
programme has been running for several decades and is therefore a useful
data source for impact evaluation of past interventions. Landsat data
are available from several cloud platforms. We here use Microsoft’s
Planetary Computer, which allows querying with the SpatioTemporal Asset
Catalog (STAC) language.

We first create two NDVI time series, for the 10 years before and the 10
years after the restoration intervention in 2012, we applied a

<!-- ```{r} -->
<!-- library(gdalcubes) -->
<!-- gdalcubes_options(parallel=4) -->
<!-- stac_endpoint <- as.endpoint("PlanetaryComputer") -->
<!-- stac_collection <- as.collection("Landsat", stac_endpoint) -->
<!-- #This step can take a few minutes -->
<!-- vi_before <- eo_stac_yearly_composites(endpoint=stac_endpoint,  -->
<!--                                           collection=stac_collection, -->
<!--                                           spatRef = refRas, -->
<!--                                           years = seq(year-10, year-1, 1), -->
<!--                                           months = 3:5, -->
<!--                                           maxCloud=60) |> -->
<!--   cubeVI(VI="NDVI", endpoint=stac_endpoint, collection=stac_collection) |> -->
<!--   gdalcube_as_terra() -->
<!-- vi_after <- eo_stac_yearly_composites(endpoint=stac_endpoint,  -->
<!--                                          collection=stac_collection, -->
<!--                                          spatRef = refRas, -->
<!--                                          years = seq(year+1, year+10, 1), -->
<!--                                          months = 3:5, -->
<!--                                          maxCloud=60) |> -->
<!--   cubeVI(VI="MSAVI", endpoint=stac_endpoint, collection=stac_collection) |> -->
<!--   gdalcube_as_terra() -->
<!-- # Calculate mean and trend from time series for each pixel -->
<!-- avgtrend_before <- calc_ts_metrics(vi_before) -->
<!-- avgtrend_after <- calc_ts_metrics(vi_after) -->
<!-- plot(avgtrend_before) -->
<!-- plot(avgtrend_after) -->
<!-- ``` -->

### Matching covariates

### Matching
