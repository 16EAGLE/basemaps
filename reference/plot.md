# Plot raster objects using `ggplot`

This function plots objects of class `SpatRaster`, `RasterLayer`,
`RasterBrick` or `RasterStack` as `ggplot2`. It is used internally by
`basemap*` functions that return `ggplot` plots.

## Usage

``` r
gg_raster(r, r_type = "RGB", gglayer = F, ...)
```

## Arguments

- r:

  raster of class `SpatRaster`, `RasterLayer`, `RasterBrick` or
  `RasterStack`.

- r_type:

  character, either `"gradient"` or `"discrete"`.

- gglayer:

  logical, if `FALSE` (default), a `ggplot2` plot is returned, if
  `TRUE`, a `ggplot2` layer is returned.

- ...:

  additional arguments, including

  - `maxpixels`, numeric, maximum number of pixels to be plotted
    (default: number of pixels in r). Use a value lower then ncell(r) to
    lower resolution for faster plotting.

  - `alpha`, numeric between 0 and 1, alpha value of the plotted data
    (transparency).

  - `maxColorValue`, numeric, the value to use as colour maximum.

  - `interpolate`, logical, whether to smooth the plot (default is
    `TRUE`).

## Value

A `ggplot2` object

## Examples

``` r
library(basemaps)

# example extent
data(ext)

if (FALSE) { # \dontrun{
# terra raster object
map <- basemap_terra(ext)

# plotting raster as ggplot using the with fill aesthetic
gg_raster(map, r_type = "RGB")

# or as gg layer using the with fill aesthetic
ggplot() + gg_raster(map, r_type = "RGB", gglayer = T) + scale_fill_identity()
} # }
```
