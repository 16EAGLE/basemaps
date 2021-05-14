***

## basemaps 0.0.1
Initial release.

**New functions:**

* `get_maptypes()` returns every supported map service and map type that can be used as input to the `map_service` and `map_type` arguments of `set_defaults()`, `basemap()` or associated functions.
* `draw_ext()` lets you draw an extent on an interactive map.
* `set_defaults()`, `get_defaults()` and `reset_defaults()` set, get or reset the defaults of all map arguments passed to `basemap()` or associated functions.
* `basemap()` and its aliases `basemap_raster()`, `basemap_stars()`, `basemap_mapview()`, `basemap_plot()`, `basemap_ggplot()`, `basemap_gglayer()`, `basemap_magick()`, `basemap_png()` and `basemap_geotif()` (down)load and cache a basemap of a defined extent `ext`, `map_service` and `map_type` and return it as an object of the defined class.

**New features:**

* Everything.

<br>

***
This document should provide a broad overview on changes that are applied to the basemaps R package. There is no warranty for completeness, since minor changes might not be included. All improvement and feature descriptions are bundled per release version. The document is currently maintained by Jakob Schwalb-Willmann.
