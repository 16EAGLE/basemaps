# `basemaps`

A lightweight `R` package to load spatial basemaps from open sources such as `OpenStreetMap`, `Carto` and `Mapbox` and return them as a object class of choice.

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
