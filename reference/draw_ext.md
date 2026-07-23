# Draw extent

This function lets you draw an extent on an interactive map. It is a
simple wrapper around
[`mapedit::drawFeatures()`](https://rdrr.io/pkg/mapedit/man/drawFeatures.html)
written by Tim Appelhans et al.

## Usage

``` r
draw_ext()
```

## Value

An `sf` object

## Examples

``` r
if (FALSE) { # \dontrun{
library(basemaps)

# draw extent interactively
ext <- draw_ext()

# set defaults for the basemap
set_defaults(ext = ext, map_service = "osm", map_type = "terrain_bg")
# for mapbox maps, you need a map_token. Register for free at mapbox.com to get a token

# load and return basemap map as raster (default)
map <- basemap()
} # }
```
