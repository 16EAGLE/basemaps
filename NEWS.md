***

## basemaps 0.0.8
New map service *Maptiler*, minor improvements

**Features:**

* added Maptiler support (`map_service="maptiler"`) for many new map types from https://maptiler.com (free token required), including `"aquarelle"`, `"backdrop"`, `"basic"`, `"bright"`, `"dataviz"`, `"landscape"`, `"ocean"`, `"outdoor"`, `"satellite"`, `"streets"`, `"toner"`, `"topo"` and `"winter"` (see `get_maptypes()`)

**Bugs:**

* Resolves 404 HTTP error when using `map_service="osm_stamen"`, `map_type="watercolor"` (#31, #30, thanks to @pushing-boulders for the PR)
* fixes an issue remarked in #29 that produces to much verbosity

<br>


***

## basemaps 0.0.7
Minor improvements

**Changes:**

* maps rendered using `basemap_ggplot` or `basemap_gglayer` or objects plotted using `gg_raster` are now displayed with all pixels by default (full resolution, `maxpixels` argument) instead of rendering in a lower resolution (previous default: `500000`).
* switched from using `aes_string` to `aes` in `gg_raster` due to deprectation of the function in `ggplot2`.

<br>


***

## basemaps 0.0.6
Major improvements, adding new map services

**Bugs:**

* Fixing an issue with maps appearing blurred/of low resolution when using `basemap_ggplot` or `basemap_gglayer`. Maps should now show up with consistent resolutions across plot/viewing methods.
* Fixing a bug from upstream causing maps to not get mosaiced correctly
* Fixing `grDevices` display bug when `NA`s are in map imagery

**Changes:**

* `gg_raster` now accepts `interpolate`, a logical argument to control smoothing of the plotted raster.
* added Stadia Maps support for `osm_stamen` maps: Due to changes by Stamen, you now need a map token from https://stadiamaps.com (free registration) for Stamen maps.
* added Stadia Maps as new map service `osm_stadia` (requires map token from https://stadiamaps.com)

<br>

***

## basemaps 0.0.5
Added `gg_raster` and `basemap_terra`

**Features:**

* added `basemap_terra` to output maps as class `SpatRaster` from package `terra`
* added `gg_raster`, a function that plots objects of class `RasterLayer`, `RasterBrick` or `{RasterStack` as `ggplot2`

**Changes:**

* included `terra` as *Imports* dependency
* replaced internal calls to `raster` functions with equivalent functions of its successor `terra`
* moved `raster` from *Imports* to *Suggests*, as it has been internally replaced by its successor `terra`
* moved `stars` from *Imports* to *Suggests*
* changed default value of argument `class` to `basemap()` to `plot` for being more user-friendly to users not familiar with spatial classes

<br>

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
