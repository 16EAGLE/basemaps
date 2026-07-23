# Set, get and reset `basemaps` defaults

These functions set, get or reset the defaults of all map arguments
passed to
[`basemap`](https://jakob.schwalb-willmann.de/basemaps/reference/basemap.md)
and associated functions.

## Usage

``` r
set_defaults(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL
)

get_defaults()

reset_defaults()
```

## Arguments

- ext:

  extent to be covered by the basemap as any spatial class supported by
  `st_bbox`.

- map_service:

  character, a map service, either `"osm"`, `"carto"` or `"mapbox"`.
  Default is `"osm"`.

- map_type:

  character, a map type, e.g. `"streets"`. For a full list of available
  map types, see
  [`get_maptypes`](https://jakob.schwalb-willmann.de/basemaps/reference/maptypes.md).

- map_res:

  numeric, resolution of base map in range from 0 to 1.

- map_token:

  character, authentication token for services that require
  registration, which are `"osm_stamen"`, `"osm_stadia"`,
  `"osm_thunderforest"`, `"maptiler"` and `"mapbox"`. Register at
  <https://stadiamaps.com/> (for stamen and stadia),
  <https://www.thunderforest.com/>, <https://www.maptiler.com/> and/or
  <https://www.mapbox.com/> to get tokens. Ignored for all other map
  services.

- map_dir:

  character, cache directory where downloaded basemap tiles will be
  stored. By default, a temporary directory is used, which is destroyed
  when the session is terminated.

## Value

For `get_defaults`, a list of defaults, otherwise none.

## Examples

``` r
library(basemaps)
data(ext)

# set defaults for the basemap
set_defaults(ext = ext, map_service = "osm", map_type = "terrain_bg")

# get defaults
get_defaults()
#> $map_service
#> [1] "osm"
#> 
#> $map_type
#> [1] "terrain_bg"
#> 
#> $map_res
#> [1] 1
#> 
#> $map_token
#> [1] NA
#> 
#> $map_dir
#> [1] "/tmp/Rtmp2HFhKG/basemaps/"
#> 
#> $ext
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 11.00096 ymin: 47.55211 xmax: 11.11581 ymax: 47.62316
#> Geodetic CRS:  WGS 84
#>   X_leaflet_id feature_type                       geometry
#> 1          380    rectangle POLYGON ((11.00097 47.55211...
#> 

if (FALSE) { # \dontrun{
# load and return basemap map as raster (default)
map <- basemap()
} # }

# reset defaults
reset_defaults()
```
