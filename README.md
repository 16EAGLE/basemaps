# `basemaps`

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/basemaps)](https://CRAN.R-project.org/package=basemaps)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/basemaps?color=brightgreen)](https://CRAN.R-project.org/package=basemaps)
[![CRAN checks](https://cranchecks.info/badges/summary/basemaps)](https://CRAN.R-project.org/package=basemaps)
[![R-CMD-check](https://github.com/16EAGLE/basemaps/workflows/R-CMD-check/badge.svg)](https://github.com/16EAGLE/basemaps/actions)
[![Build Status](https://travis-ci.org/16EAGLE/basemaps.svg?branch=master)](https://travis-ci.org/16EAGLE/basemaps) 
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/16EAGLE/basemaps?branch=master&svg=true)](https://ci.appveyor.com/project/16EAGLE/basemaps)
[![Coverage](https://codecov.io/gh/16eagle/basemaps/branch/master/graph/badge.svg)](https://app.codecov.io/gh/16EAGLE/basemaps)
[![Package dependencies](https://tinyverse.netlify.com/badge/basemaps)](https://CRAN.R-project.org/package=basemaps)
<!-- badges: end -->

`basemaps` is a lightweight `R` package to download and cache spatial basemaps from open sources such as *OpenStreetMap*, *Carto*, *Mapbox* and others. Retrieved basemaps are translated into and returned as classes of choice, such as `raster`, `stars`, `ggplot`, `mapview` or `magick`. The package aims to ease the use of basemaps in different contexts by providing a function interface as minimalist as possible.


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
set_defaults(map_service = "mapbox", map_type = "satellite",
             map_token = "YOUR_MAPTOKEN_IF_NEEDED")

# load and return basemap map as many different classes:
basemap_plot(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

![](https://i.imgur.com/54KABxh.png)

``` r
basemap_mapview(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

![](https://i.imgur.com/hcSaXip.png)

``` r
basemap_ggplot(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

![](https://i.imgur.com/6Q8iweb.png)

``` r
basemap_magick(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

<img src="https://i.imgur.com/QDnY4Nw.jpg" width="1339" />

``` r
basemap_raster(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> class      : RasterBrick 
#> dimensions : 1228, 1339, 1644292, 3  (nrow, ncol, ncell, nlayers)
#> resolution : 9.554629, 9.554629  (x, y)
#> extent     : 1224617, 1237410, 6032659, 6044392  (xmin, xmax, ymin, ymax)
#> crs        : +proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs 
#> source     : /tmp/Rtmp1Ceog7/basemaps/basemap_20210514130850.tif 
#> names      : basemap_20210514130850.1, basemap_20210514130850.2, basemap_20210514130850.3
basemap_stars(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> stars object with 3 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>  basemap_20210514130850.tif 
#>  Min.   :  0.00             
#>  1st Qu.: 25.00             
#>  Median : 38.00             
#>  Mean   : 42.56             
#>  3rd Qu.: 53.00             
#>  Max.   :233.00             
#> dimension(s):
#>      from   to  offset    delta                   refsys point values x/y
#> x       1 1339 1224617  9.55463 WGS 84 / Pseudo-Mercator FALSE   NULL [x]
#> y       1 1228 6044392 -9.55463 WGS 84 / Pseudo-Mercator FALSE   NULL [y]
#> band    1    3      NA       NA                       NA    NA   NULL
basemap_png(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> [1] "/tmp/Rtmp1Ceog7/basemaps//mapbox_satellite_2021-05-14_13-09-09.png"
basemap_geotif(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> [1] "/tmp/Rtmp1Ceog7/basemaps/basemap_20210514130850.tif"
 
library(ggplot2)
ggplot() + 
  basemap_gglayer(ext) + 
  coord_sf() +
  scale_fill_identity() 
#> Loading basemap 'satellite' from map service 'mapbox'...
```

![](https://i.imgur.com/6wBYd2o.png)


## Map examples

`basemaps` supports a variety of map services and types (which can be printed using `get_maptypes()`). A selection of available map types is shown below. For a complete table of available map types, see [supported services and maps](#supported-services-and-maps).

``` r
basemap_magick(ext, map_service = "osm", map_type = "topographic")
#> Loading basemap 'topographic' from map service 'osm'...
```

<img src="https://i.imgur.com/4mVIjci.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm_stamen", map_type = "toner")
#> Loading basemap 'toner' from map service 'osm'...
```

<img src="https://i.imgur.com/sPQ8uIV.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm", map_type = "hike")
#> Loading basemap 'hike' from map service 'osm'...
```

<img src="https://i.imgur.com/2u6BMSO.png" width="623" />

``` r
basemap_magick(ext, map_service = "carto", map_type = "dark")
#> Loading basemap 'dark' from map service 'carto'...
```

<img src="https://i.imgur.com/bcFKweX.png" width="623" />

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

<img src="https://i.imgur.com/WOVHncM.png" width="623" />

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

## Available functions

* `get_maptypes()` returns every supported map service and map type that can be used as input to the `map_service` and `map_type` arguments of `set_defaults()`, `basemap()` or associated functions.
* `draw_ext()` lets you draw an extent on an interactive map.
* `set_defaults()`, `get_defaults()` and `reset_defaults()` set, get or reset the defaults of all map arguments passed to `basemap()` or associated functions.
* `basemap()` and its aliases `basemap_raster()`, `basemap_stars()`, `basemap_mapview()`, `basemap_plot()`, `basemap_ggplot()`, `basemap_gglayer()`, `basemap_magick()`, `basemap_png()` and `basemap_geotif()` (down)load and cache a basemap of a defined extent `ext`, `map_service` and `map_type` and return it as an object of the defined class.


## Related packages

If you are interested in obtaining basemaps, you also may want to have a look at [`ceramic`](https://github.com/hypertidy/ceramic), an R package developed by Michael Sumner with a similar goal.

The underpinning code of `basemaps` was originally developed for [`moveVis`](http://movevis.org) and has been detached to allow its general use.


