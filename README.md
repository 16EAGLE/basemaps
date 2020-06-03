# `basemaps`

[![CRAN version](https://www.r-pkg.org/badges/version/basemaps)](https://CRAN.R-project.org/package=basemaps)
[![Build Status](https://travis-ci.org/16EAGLE/basemaps.svg?branch=master)](https://travis-ci.org/16EAGLE/basemaps) 
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/16EAGLE/basemaps?branch=master&svg=true)](https://ci.appveyor.com/project/16EAGLE/basemaps)
[![Coverage](https://codecov.io/gh/16eagle/basemaps/branch/master/graph/badge.svg)](https://codecov.io/gh/16EAGLE/basemaps)
[![Package dependencies](https://tinyverse.netlify.com/badge/basemaps)](https://CRAN.R-project.org/package=basemaps)

`basemaps` is a lightweight `R` package to download and cache spatial basemaps from open sources such as *OpenStreetMap*, *Carto* and *Mapbox*. Retrieved basemaps are translated into and returned as classes of choice, such as `raster`, `stars`, `ggplot`, `mapview` or `magick`.

## Get started

All available map services and types can be printed using `get_maptypes()`. The `basemap()` function and its class-specific aliases facilitate (down)loading a basemap and returning it as a class of choice. Map preferences that should be used during a session can be set as defaults using `set_defaults()`.

``` r
library(basemaps)
data(ext)

# view all available maps
get_maptypes()

# set defaults for the basemap
set_defaults(map_service = "mapbox", map_type = "satellite",
             map_token = "YOUR_MAPTOKEN_IF_NEEDED")
# token only needed for mapbox maps, register for free at mapbox.com to get a token

# load and return basemap map as many different classes:
basemap_plot(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

<img src="https://i.imgur.com/Z6a4ucn.png" width="623" />

``` r

basemap_mapview(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

<img src="https://i.imgur.com/ILLiSi6.png" width="623" />

``` r
basemap_ggplot(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

<img src="https://i.imgur.com/ATyDXRQ.png" width="623" />

``` r

basemap_magick(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

<img src="https://i.imgur.com/JPcsRRH.png" width="623" />

``` r
basemap_raster(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> class      : RasterBrick 
#> dimensions : 582, 623, 362586, 3  (nrow, ncol, ncell, nlayers)
#> resolution : 19.10926, 19.10926  (x, y)
#> extent     : 1225763, 1237668, 6034379, 6045500  (xmin, xmax, ymin, ymax)
#> crs        : +proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +no_defs 
#> source     : /tmp/RtmpjCVUpd/basemaps/basemap_20200528212540.tif 
#> names      : val1, val2, val3 
#> min values :   23,   16,   14 
#> max values :  255,  255,  255

basemap_stars(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> stars object with 3 dimensions and 1 attribute
#> attribute(s), summary of first 1e+05 cells:
#>      val1        
#>  Min.   : 23.00  
#>  1st Qu.: 64.00  
#>  Median : 77.00  
#>  Mean   : 84.12  
#>  3rd Qu.:104.00  
#>  Max.   :255.00  
#> dimension(s):
#>      from  to  offset    delta                       refsys point        values
#> x       1 623 1225763  19.1093 +proj=merc +a=6378137 +b=...    NA          NULL
#> y       1 582 6045500 -19.1093 +proj=merc +a=6378137 +b=...    NA          NULL
#> band    1   3      NA       NA                           NA    NA val1,...,val3
#>         
#> x    [x]
#> y    [y]
#> band

basemap_png(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
#> [1] "/tmp/RtmpjCVUpd/basemaps//mapbox_satellite_2020-05-28_21-25-47.png"
```

## Map services and types

`basemaps` supports a variety of map services and types (which can be printed using `get_maptypes()`). A selection of available map types is shown below:

``` r
basemap_magick(ext, map_service = "osm", map_type = "topographic")
#> Loading basemap 'topographic' from map service 'osm'...
```

<img src="https://i.imgur.com/4mVIjci.png" width="623" />

``` r
basemap_magick(ext, map_service = "osm", map_type = "toner")
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
basemap_magick(ext, map_service = "mapbox", map_type = "hike")
#> Loading basemap 'hike' from map service 'mapbox'...
```

<img src="https://i.imgur.com/syITrpf.png" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "terrain")
#> Loading basemap 'terrain' from map service 'mapbox'...
```

<img src="https://i.imgur.com/WOVHncM.png" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "comic")
#> Loading basemap 'comic' from map service 'mapbox'...
```

<img src="https://i.imgur.com/ppOyTPL.png" width="623" />

``` r
basemap_magick(ext, map_service = "mapbox", map_type = "pirates")
#> Loading basemap 'pirates' from map service 'mapbox'...
```

<img src="https://i.imgur.com/4uLFU2D.png" width="623" />

<sup>Created on 2020-05-28 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>

## Available functions

* `get_maptypes()` returns every supported map type that can be used as input to the `map_type` argument of `set_defaults()`, `basemap()` or associated functions.
* `set_defaults()` and `get_defaults()` sets and gets the defaults of all map arguments passed to `basemap()` or associated functions.
* `basemap()` and its aliases `basemap_raster()`, `basemap_stars()`, `basemap_mapview()`, `basemap_plot()`, `basemap_ggplot()`, `basemap_gglayer()`, `basemap_magick()` and `basemap_png()` (down)load and cache a basemap of a defined extent `ext`, `map_service` and `map_type` and return it as an object of the defined class.


## Related packages

If you are interested in obtaining basemaps, you also may want to have a look at [`ceramic`](https://github.com/hypertidy/ceramic), an R package developed by Michael Sumner with a similar goal.

The underpinning code of `basemaps` was originally developed for [`moveVis`](http://movevis.org) and has been detached to allow its general use.


