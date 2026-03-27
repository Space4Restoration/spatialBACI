# EO mask for data cube

Create mask from Landsat QA layer or Sentinel-2 SCL that can be used in
gdalcubes::raster_cube

## Usage

``` r
eo_mask.cube(maskLyr, maskSnow = FALSE, maskWater = FALSE)
```

## Arguments

- maskLyr:

  character. Name of mask layer.

- maskSnow:

  logical. Defines whether snow should be masked.

- maskWater:

  logical. Defines whether water should be masked.

## Value

an object of class "image_mask"

## Details

The metadata layer from which the mask is derived should be specified in
`maskLyr`. For now, mask derived from "qa_pixel" (for Landsat) and "SCL"
(for Sentinel-2) are implemented. Clouds, cloud shadow and pixels
adjacent to clouds are always masked (when these fields are specified in
the masking layer). Snow and water can be masked by setting the
respective parameters, by default these are not masked

## Examples

``` r
# mask from Landsat "qa_pixel" layer
eo_mask.cube("qa_pixel")
#> $band
#> [1] "qa_pixel"
#> 
#> $values
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  33  34  35  36  37
#>  [37]  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55
#>  [55]  56  57  58  59  60  61  62  63  65  66  67  68  69  70  71  72  73  74
#>  [73]  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
#>  [91]  93  94  95  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
#> [109] 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 129 130
#> [127] 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
#> [145] 149 150 151 152 153 154 155 156 157 158 159 161 162 163 164 165 166 167
#> [163] 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185
#> [181] 186 187 188 189 190 191 193 194 195 196 197 198 199 200 201 202 203 204
#> [199] 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222
#> [217] 223 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241
#> [235] 242 243 244 245 246 247 248 249 250 251 252 253 254 255
#> 
#> $invert
#> [1] FALSE
#> 
#> $bits
#> [1] 0 1 2 3 4 5 6 7
#> 
#> attr(,"class")
#> [1] "image_mask"
# mask from Sentinel-2 "SCL" layer
eo_mask.cube("SCL", maskSnow=TRUE, maskWater=TRUE)
#> $band
#> [1] "SCL"
#> 
#> $values
#> [1]  0  1  2  3  8  9 10  6 11
#> 
#> $invert
#> [1] FALSE
#> 
#> $bits
#> NULL
#> 
#> attr(,"class")
#> [1] "image_mask"
```
