# `basemaps`

`basemaps` is a lightweight `R` package to download and cache spatial basemaps from open sources, including `OpenStreetMap`, `Carto`, `Mapbox` or `Esri`. Retrieved basemaps are translated into and returned as classes of choice, such as `ggplot`, `raster` or `stars`. 

## Get started

``` r
library(basemaps)
data(ext)

# view all available maps
get_maptypes()

# set defaults for the basemap
set_defaults(map_service = "mapbox", map_type = "satellite",
             map_token = "YOUR_MAPTOKEN_IF_NEEDED")

# load and return basemap map as many different classes
map <- basemap_raster(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
basemap_ggplot(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

![](https://i.imgur.com/7lIzqGD.png)

``` r
basemap_plot(ext)
#> Loading basemap 'satellite' from map service 'mapbox'...
```

![](https://i.imgur.com/5gSK5Uj.png)

<sup>Created on 2020-05-01 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>

## Available functions

* `get_maptypes()` returns every supported map type that can be used as input to the `map_type` argument of `set_defaults()`, `basemap()` or associated functions.
* `set_defaults()` and `get_defaults()` sets and gets the defaults of all map arguments passed to `basemap()` or associated functions.
* `basemap()` and its aliases `basemap_raster()`, `basemap_stars()`, `basemap_mapview()`, `basemap_plot()`, `basemap_ggplot()` and `basemap_gglayer()` (down)load and cache a basemap of a defined extent `ext`, `map_service` and `map_type` and return it as a object of the defined class.


## Related packages

If you are interested in obtaining basemaps, you also may want to have a look at [`ceramic`](https://github.com/hypertidy/ceramic), an R package developed by Micheal Sumner with a similar goal.

The underpinning code of `basemaps` was originally developed for [`moveVis`](http://movevis.org) and has been detached to allow its general use.


