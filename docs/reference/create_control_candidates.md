# Create candidate control raster

Create a SpatRaster of impact/treatment and candidate control pixels.

## Usage

``` r
create_control_candidates(
  impact,
  resolution,
  crs,
  control_from_buffer = NULL,
  control_from_include = NULL,
  control_exclude = NULL,
  exclude_impact_buffer = NULL,
  sample_control = NULL,
  sample_impact = NULL,
  round_coords = FALSE
)
```

## Arguments

- impact:

  SpatVector. Vector representation of the impact sites

- resolution:

  numeric. Spatial resolution of the output SpatRaster, in units of
  \`crs\`

- crs:

  Coordinate Reference System in PROJ.4, WKT or authority:code notation.
  Defaults to the UTM zone of center coordinates of \`impact\`

- control_from_buffer:

  numeric vector of 1, 2, or 4 elements. Indicates by how much (in units
  of \`crs\`) the spatial extent around the impact sites should be
  enlarged on each side.

- control_from_include:

  SpatVector. Indicates area to include in candidate control selection

- control_exclude:

  SpatVector. Indicates area to exclude from candidate control selection

- exclude_impact_buffer:

  numeric vector of length 1 or 2. Indicates buffer around \`impact\`,
  in units of \`crs\`, to exclude as impact and/or control. See Details.

- sample_control:

  numeric. A positive integer giving the number of control units to
  choose, or a value between 0 and 1 giving the fraction of control
  units to choose.

- sample_impact:

  numeric. Same as \`sample_control\`, but for impact units.

- round_coords:

  logical or integer. Should the coordinates of the output SpatRaster be
  rounded. If TRUE, coordinates are rounded to the nearest integer; a
  positive numeric rounds to the corresponding decimal, a negative
  integer rounds to the corresponding power of 10 (e.g.,
  \`round_coords=-2\` rounds to the nearest 100).

## Value

a SpatRaster in which impact pixels have value 1, candidate control
pixels have value 0, and pixels excluded as impact or control have value
NA

## Details

Impact pixels are defined by providing a SpatVector object.

Pixels to include as candidate control units can be defined from a
SpatVector file, using the \`control_from_include\` argument, or from
bounding box around the impact SpatVector object, using the
\`control_from_buffer\` argument. At least one of these arguments must
be provided, if both are provided the \`control_from_buffer\` argument
is ignored.

Pixels can be excluded as candidate control units by providing a
SpatVector in the \`control_exclude\` argument. Additionally, pixels at
the borders of the impact polygon can be excluded as both impact and/or
control with the \`exclude_impact_buffer\` argument, to account for
adjacency effects. A negative value of \`exclude_impact_buffer\` will
eliminate pixels in the inner buffer around the polygon as impact units,
a positive value will eliminate pixels in the outer buffer as control
units. Providing a vector of length two can be used to exclude an inner
and outer buffer.

For large areas, impact and candidate control pixels can be randomly
sampled from the population of all impact/control pixels, using the
\`sample_impact\` and \`sample_control\` arguments, respectively. This
can be useful to reduce computational cost of the subsequent matching
analysis.
