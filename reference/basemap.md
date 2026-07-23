# Get a spatial basemap

These functions (down)load and cache a basemap of a defined extent
`ext`, `map_service` and `map_type` and return it as an object of the
defined class. Alternatively to defining the following arguments,
[`set_defaults`](https://jakob.schwalb-willmann.de/basemaps/reference/defaults.md)
can be used to define basemap preferences once for the running session.

## Usage

``` r
basemap(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  class = "plot",
  force = FALSE,
  ...,
  verbose = TRUE
)

basemap_plot(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_magick(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_png(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_geotif(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_terra(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_raster(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_stars(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_ggplot(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_gglayer(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)

basemap_mapview(
  ext = NULL,
  map_service = NULL,
  map_type = NULL,
  map_res = NULL,
  map_token = NULL,
  map_dir = NULL,
  force = NULL,
  ...,
  verbose = TRUE
)
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

- class:

  character, output class, either either `plot` (default), `magick`,
  `png`, `geotif` or if suggested packages are installed, `terra`,
  `raster`, `stars`, `ggplot`, `gglayer` or `mapview`.

- force:

  logical, whether to force download over cached files or not. Default
  is `FALSE`.

- ...:

  additional arguments, including

  - `browse`, logical, for `class = "png"` and interactive sessions
    only. Whether to open the png file in the system's default PNG
    viewer or not. Default is `TRUE`.

  - `col`, character vector of colours for transforming single-layer
    basemaps into RGB, if `class = "png"` or `class = "magick"`. Default
    is `topo.colors(25)`.

  - `dpi`, numeric vector of length 1 or 2 specifying the resolution of
    the image in DPI (dots per inch) for x and y (in that order) - it is
    recycled to length 2.

  - etc. (see
    [`?gg_raster`](https://jakob.schwalb-willmann.de/basemaps/reference/plot.md)
    for valid arguments when using `class = "gglayer"` or
    `class = "ggplot"`, including `maxpixels` to control resolution of
    ggplot outputs

- verbose:

  logical, if `TRUE`, messages and progress information are displayed on
  the console (default).

## Value

A basemap of the defined class in Web/Pseudo Mercator Projection (EPSG:
3857)

## Note

See
[get_maptypes](https://jakob.schwalb-willmann.de/basemaps/reference/maptypes.md)
for available map services and their sources.

The use of the map services `"osm_thunderforest"` and `"mapbox"` require
registration to obtain an API token/key which can be supplied to
`map_token`. Register at <https://www.thunderforest.com/> and/or
<https://www.mapbox.com/> to get a token.

## Examples

``` r
library(basemaps)

# example extent
data(ext)

# view all available maps
get_maptypes()
#> $osm
#> [1] "streets"     "streets_de"  "topographic"
#> 
#> $osm_stamen
#> [1] "toner"      "toner_bg"   "terrain"    "terrain_bg" "watercolor"
#> 
#> $osm_stadia
#> [1] "alidade_smooth"      "alidade_smooth_dark" "outdoors"           
#> [4] "osm_bright"         
#> 
#> $osm_thunderforest
#>  [1] "cycle"          "transport"      "landscape"      "outdoors"      
#>  [5] "transport_dark" "spinal"         "pioneer"        "mobile_atlas"  
#>  [9] "neighbourhood"  "atlas"         
#> 
#> $carto
#>  [1] "light"                "light_no_labels"      "light_only_labels"   
#>  [4] "dark"                 "dark_no_labels"       "dark_only_labels"    
#>  [7] "voyager"              "voyager_no_labels"    "voyager_only_labels" 
#> [10] "voyager_labels_under"
#> 
#> $mapbox
#> [1] "streets"   "outdoors"  "light"     "dark"      "satellite" "hybrid"   
#> [7] "terrain"  
#> 
#> $esri
#>  [1] "natgeo_world_map"                     
#>  [2] "usa_topo_maps"                        
#>  [3] "world_imagery"                        
#>  [4] "world_physical_map"                   
#>  [5] "world_shaded_relief"                  
#>  [6] "world_street_map"                     
#>  [7] "world_terrain_base"                   
#>  [8] "world_topo_map"                       
#>  [9] "world_dark_gray_base"                 
#> [10] "world_dark_gray_reference"            
#> [11] "world_light_gray_base"                
#> [12] "world_light_gray_reference"           
#> [13] "world_hillshade_dark"                 
#> [14] "world_hillshade"                      
#> [15] "world_ocean_base"                     
#> [16] "world_ocean_reference"                
#> [17] "antarctic_imagery"                    
#> [18] "arctic_imagery"                       
#> [19] "arctic_ocean_base"                    
#> [20] "arctic_ocean_reference"               
#> [21] "world_boundaries_and_places_alternate"
#> [22] "world_boundaries_and_places"          
#> [23] "world_reference_overlay"              
#> [24] "world_transportation"                 
#> [25] "delorme_world_base_map"               
#> [26] "world_navigation_charts"              
#> 
#> $maptiler
#>  [1] "aquarelle"       "aquarelle_dark"  "aquarelle_vivid" "backdrop"       
#>  [5] "basic"           "bright"          "dataviz"         "landscape"      
#>  [9] "ocean"           "outdoor"         "satellite"       "streets"        
#> [13] "toner"           "topo"            "winter"         
#> 

# set defaults for the basemap
set_defaults(map_service = "osm", map_type = "terrain_bg")
# for "osm_stamen", "osm_stadia", osm "thunderforest", "maptiler" and "mapbox" maps, 
# you need a API token. Register for free at stadiamaps.com, thunderforest.com, 
# maptiler.com and mapbox.com to get tokens.

if (FALSE) { # \dontrun{
# load and plot basemap (default)
map <- basemap(ext)

# or explicitely as different classes such as:
basemap_magick(ext)
basemap_raster(ext)
basemap_stars(ext)

# or as files:
basemap_geotif(ext)
basemap_png(ext)

# or as plots:
basemap_plot(ext)
basemap_mapview(ext)

# including ggplot2:
basemap_ggplot(ext)

# or as ggplot2 layer:
library(ggplot2)
ggplot() +
basemap_gglayer(ext) +
  scale_fill_identity() +
  coord_sf()

# or, when combined with an sf vector object,
# make sure to use Web/Pseudo Mercator (EPSG 3857), as this is
# the CRS in which all basemaps are returned (see "Value"):
library(sf)
ext <- st_transform(ext,  crs = st_crs(3857))
ggplot() +
  basemap_gglayer(ext) +
  geom_sf(data = ext, color = "red", fill = "transparent") +
  coord_sf() +
  scale_fill_identity()
} # }
```
