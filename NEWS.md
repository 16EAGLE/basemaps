***

## basemaps 0.0.4
Updated help, bug fixes.

**Bugs:**

* dropped defunct/not maintained tile servers (OSM) from map services list
* fixing a bug (`"dimensions of e1 and e2 do not match"`) when trying to build a map from `map_service="mapbox"` and `map_type="terrain"` with newer versions of `stars`
* added a handling to fix a plotting bug for cases in which maps returned by tile servers (for whatever reasons) consist of only a single value (e.g. 0) for the given query extent ([#9](https://github.com/16EAGLE/basemaps/issues/9))
* fixed a bug placing cached maps in the wrong directory when `map_dir` is defined by the user ([#11](https://github.com/16EAGLE/basemaps/issues/11))
* cached maps stored in other directories than the currently set `map_dir` are now disregarded to avoid path errors when switching map directories during a session ([#11](https://github.com/16EAGLE/basemaps/issues/11))

**Features:**

* added `flush_cache`, a function that flushes the cache and thereby removes all previously queried and/or composited products from the map directories (temporary or user-defined using the argument `map_dir`) used during the current session.

<br>

***

## basemaps 0.0.3
Minor improvements.

**Features:**

* added CRS warning in response to issues such as #3, #7
* added dpi argument, passing down input to png::writePNG (issue #5)

<br>

***

## basemaps 0.0.2
Bug fixes.

**Fixed bugs:**

* added minimum version requirement for dependency `stars` (>= 0.5.0) to make sure that `st_set_bbox` is exported (see issue #3)

<br>

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
