# `basemaps`

[![CRAN version](https://www.r-pkg.org/badges/version/basemaps)](https://CRAN.R-project.org/package=basemaps)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/basemaps?color=brightgreen)](https://CRAN.R-project.org/package=basemaps)
[![CRAN checks](https://cranchecks.info/badges/summary/basemaps)](https://CRAN.R-project.org/package=basemaps)
[![R-CMD-check](https://github.com/16EAGLE/basemaps/workflows/R-CMD-check/badge.svg)](https://github.com/16EAGLE/basemaps/actions)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/16EAGLE/basemaps?branch=master&svg=true)](https://ci.appveyor.com/project/16EAGLE/basemaps)
[![Coverage](https://codecov.io/gh/16eagle/basemaps/branch/master/graph/badge.svg)](https://app.codecov.io/gh/16EAGLE/basemaps)
[![Package dependencies](https://tinyverse.netlify.com/badge/basemaps)](https://CRAN.R-project.org/package=basemaps)

`basemaps` is a lightweight `R` package to download and cache spatial basemaps from open sources such as *OpenStreetMap*, *Stamen*, *Thunderforest*, *Carto*, *Mapbox* and others. Retrieved basemaps are translated into and returned as classes of choice, such as `raster`, `stars`, `terra`, `ggplot`, `mapview`, `magick`, or as files, such as `png` or `geotif`. The package aims to ease the use of basemaps in different contexts by providing a function interface as minimalist as possible.


## Installation


Install the latest stable release of `basemaps` from CRAN:

```r
install.packages("basemaps")
```

Install the latest development version of `basemaps` from GitHub:

```r
devtools::install_github("16EAGLE/basemaps")
```

## Get started

All available map services and map types can be printed using `get_maptypes()`. The `basemap()` function and its class-specific aliases facilitate (down)loading a basemap and returning it as a class of choice. Map preferences that should be used during a session can be set as defaults using `set_defaults()`.

``` r
library(basemaps)
data(ext)
# or use draw_ext() to interactively draw an extent yourself

# view all available maps
get_maptypes()

# set defaults for the basemap
set_defaults(map_service = "osm", map_type = "topographic")

# load and return basemap map as class of choice, e.g. as image using magick:
basemap_magick(ext)
#> Loading basemap 'topographic' from map service 'osm'...
```

<img src="https://i.imgur.com/uVbyyXb.png" width="623" />

``` r
# or as plot:
basemap_plot(ext)

# or as ggplot2:
basemap_ggplot(ext)

# or as ggplot2 layer:
library(ggplot2)
ggplot() + 
  basemap_gglayer(ext) +
  scale_fill_identity() + 
  coord_sf()
```

<img src="https://i.imgur.com/3BzQEkx.png" width="623" />

``` r
# or as mapview:
basemap_mapview(ext)
```

<img src="https://i.imgur.com/0L6G37X.png" width="623" />

``` r
# or as spatial classes, such as raster:
basemap_raster(ext)

# or terra:
basemap_raster(ext)

# or stars:
basemap_stars(ext)

# or return files, e.g. geotif
basemap_geotif(ext)
#> [1] "basemap_20220922214954.tif"

# or png:
basemap_png(ext)
#> [1] "osm_stamen_terrain_bg_2022-09-22_12-00-00.png"
```

## Map examples

`basemaps` supports a variety of map services and types (which can be printed using `get_maptypes()`). A selection of available map types is shown below. For a complete table of available map types, see [supported services and maps](#supported-services-and-maps).

``` r
basemap_magick(ext, map_service = "osm", map_type = "streets")
#> Loading basemap 'streets' from map service 'osm'...
```

<img src="https://i.imgur.com/zT5m4Ne.png" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "satellite")
#> Loading basemap 'satellite' from map service 'mapbox'...
```

<img src="https://i.imgur.com/rM4VtoO.jpg" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "hybrid")
#> Loading basemap 'hybrid' from map service 'mapbox'...
```

<img src="https://i.imgur.com/F7fU32T.png" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "streets")
#> Loading basemap 'streets' from map service 'mapbox'...
```

<img src="https://i.imgur.com/a0FUoau.png" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "terrain")
#> Loading basemap 'terrain' from map service 'mapbox'...
```

<img src="https://i.imgur.com/HfaZvBk.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_stamen", map_type = "terrain_bg")
#> Loading basemap 'terrain_bg' from map service 'osm_stamen'...
```

<img src="https://i.imgur.com/WKwOIlW.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_stamen", map_type = "toner")
#> Loading basemap 'toner' from map service 'osm_stamen'...
```

<img src="https://i.imgur.com/b7me3PP.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_stamen", map_type = "watercolor")
#> Loading basemap 'watercolor' from map service 'osm_stamen'...
```

<img src="https://i.imgur.com/U9tCTkQ.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_thunderforest", map_type = "transport")
#> Loading basemap 'transport' from map service 'osm_thunderforest'...
```

<img src="https://i.imgur.com/urteXlA.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_thunderforest", map_type = "landscape")
#> Loading basemap 'landscape' from map service 'osm_thunderforest'...
```

<img src="https://i.imgur.com/V6oB4jP.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_thunderforest", map_type = "outdoors")
#> Loading basemap 'outdoors' from map service 'osm_thunderforest'...
```

<img src="https://i.imgur.com/e6wWgLQ.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_thunderforest", map_type = "atlas")
#> Loading basemap 'atlas' from map service 'osm_thunderforest'...
```

<img src="https://i.imgur.com/xXNSYOI.png" width="623" />

``` r
basemap_magick(ext, map_service = "carto", map_type = "light")
#> Loading basemap 'light' from map service 'carto'...
```

<img src="https://i.imgur.com/OBMrcWi.png" width="623" />

``` r
basemap_magick(ext, map_service = "carto", map_type = "dark")
#> Loading basemap 'dark' from map service 'carto'...
```

<img src="https://i.imgur.com/c0hlnYP.png" width="623" />


## Supported services and maps

This table lists all currently implemented map services and map types and indicates whether they can be used without a map token (requiring registration at the respective service) or not.

| `map_service` | `map_type` | `map_token` required? |
| ------ |  ------ | ------ |
| `osm` | `streets` | no |
| `osm` | `streets_de` | no |
| `osm` | `topographic` | no |
| `osm_stamen` | `toner` | no |
| `osm_stamen` | `toner_bg` | no |
| `osm_stamen` | `toner_lite` | no |
| `osm_stamen` | `terrain` | no |
| `osm_stamen` | `terrain_bg` | no |
| `osm_stamen` | `watercolor` | no |
| `osm_thunderforest` | `cycle` | yes |
| `osm_thunderforest` | `transport` | yes |
| `osm_thunderforest` | `landscape` | yes |
| `osm_thunderforest` | `outdoors` | yes |
| `osm_thunderforest` | `transport_dark` | yes |
| `osm_thunderforest` | `spinal` | yes |
| `osm_thunderforest` | `pioneer` | yes |
| `osm_thunderforest` | `mobile_atlas` | yes |
| `osm_thunderforest` | `neighbourhood` | yes |
| `osm_thunderforest` | `atlas` | yes |
| `carto` | `light` | no |
| `carto` | `light_no_labels` | no |
| `carto` | `light_only_labels` | no |
| `carto` | `dark` | no |
| `carto` | `dark_no_labels` | no |
| `carto` | `dark_only_labels` | no |
| `carto` | `voyager` | no |
| `carto` | `voyager_no_labels` | no |
| `carto` | `voyager_only_labels` | no |
| `carto` | `voyager_labels_under` | no |
| `mapbox` | `streets` | yes |
| `mapbox` | `outdoors` | yes |
| `mapbox` | `light` | yes |
| `mapbox` | `dark` | yes |
| `mapbox` | `satellite` | yes |
| `mapbox` | `hybrid` | yes |
| `mapbox` | `terrain` | yes |
| `esri` | `natgeo_world_map` | no |
| `esri` | `usa_topo_maps` | no |
| `esri` | `world_imagery` | no |
| `esri` | `world_physical_map` | no |
| `esri` | `world_shaded_relief` | no |
| `esri` | `world_street_map` | no |
| `esri` | `world_terrain_base` | no |
| `esri` | `world_topo_map` | no |
| `esri` | `world_dark_gray_base` | no |
| `esri` | `world_dark_gray_reference` | no |
| `esri` | `world_light_gray_base` | no |
| `esri` | `world_light_gray_reference` | no |
| `esri` | `world_hillshade_dark` | no |
| `esri` | `world_hillshade` | no |
| `esri` | `world_ocean_base` | no |
| `esri` | `world_ocean_reference` | no |
| `esri` | `antarctic_imagery` | no |
| `esri` | `arctic_imagery` | no |
| `esri` | `arctic_ocean_base` | no |
| `esri` | `arctic_ocean_reference` | no |
| `esri` | `world_boundaries_and_places_alternate` | no |
| `esri` | `world_boundaries_and_places` | no |
| `esri` | `world_reference_overlay` | no |
| `esri` | `world_transportation` | no |
| `esri` | `delorme_world_base_map` | no |
| `esri` | `world_navigation_charts` | no |
| `usgs` | `hydro_cached` | no |
| `usgs` | `imagery_only` | no |
| `usgs` | `imagery_topo` | no |
| `usgs` | `shaded_relief` | no |
| `usgs` | `national_map` | no |
| `usgs` | `topo` | no |

## Available functions

* `get_maptypes()` returns every supported map service and map type that can be used as input to the `map_service` and `map_type` arguments of `set_defaults()`, `basemap()` or associated functions.
* `draw_ext()` lets you draw an extent on an interactive map.
* `set_defaults()`, `get_defaults()` and `reset_defaults()` set, get or reset the defaults of all map arguments passed to `basemap()` or associated functions.
* `basemap()` and its aliases `basemap_raster()`, `basemap_stars()`, `basemap_mapview()`, `basemap_plot()`, `basemap_ggplot()`, `basemap_gglayer()`, `basemap_magick()`, `basemap_png()` and `basemap_geotif()` (down)load and cache a basemap of a defined extent `ext`, `map_service` and `map_type` and return it as an object of the defined class.
* `gg_raster()` plots objects of class `SpatRaster`, `RasterLayer`, `RasterBrick` or `RasterStack` as `ggplot2`.
* `flush_cache()` deletes all cached map tiles and basemaps.

## Related packages

If you are interested in obtaining basemaps, you also may want to have a look at [`ceramic`](https://github.com/hypertidy/ceramic), an R package developed by Michael Sumner with a similar goal.

The underpinning code of `basemaps` was originally developed for [`moveVis`](https://movevis.org) and has been detached to allow its general use.

<br>

***

<sup>&copy; Mapbox &copy; Thunderforest &copy; Stamen &copy; Carto &copy; Esri. Many thanks to the creators of [reprex v2.0.2](https://reprex.tidyverse.org) which this README was compiled with.</sup>
