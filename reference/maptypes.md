# Get supported map types or add new map types

These functions manage map types.

## Usage

``` r
get_maptypes(map_service = NULL, as_df = FALSE, url_cols = FALSE)

add_maptypes(
  map_service,
  map_type,
  url_endpoint,
  url_xy = "xy",
  url_file_format = ".png",
  url_map_token = as.character(NA),
  auth_error_code = 401,
  url_website = as.character(NA)
)

save_maptypes(file)

load_maptypes(file)
```

## Source

`"osm"`: Open Street Map contributors
(<https://www.openstreetmap.org/copyright>), Open Topo Map
(<https://opentopomap.org/>)

`"osm_stamen"`: Stamen (<https://maps.stamen.com/>) via Stadia Maps
(<https://stadiamaps.com/>), Open Street Map contributors
(<https://www.openstreetmap.org/copyright>)

`"osm_stadia"`: Stadia Maps (<https://stadiamaps.com/>), Open Street Map
contributors (<https://www.openstreetmap.org/copyright>)

`"osm_thunderforest"`: Thunderforest (<https://www.thunderforest.com/>),
Open Street Map contributors (<https://www.openstreetmap.org/copyright>)

`"carto"`: Carto (<https://carto.com/>)

`"mapbox"`: Mapbox (<https://www.mapbox.com>)

`"esri"`: Esri (<https://www.esri.com/en-us/home>)

`"maptiler"`: Esri (<https://www.maptiler.com>)

## Arguments

- map_service:

  character.

  - for `get_maptypes()`: optional, either `"osm"`, `"osm_stamen"`,
    `"osm_stadia"`, `"osm_thunderforest"`, `"carto"`, `"mapbox"`,
    `"esri"` or `"maptiler"`. Otherwise, a list of map types for both
    services is returned.

  - for `add_maptypes()`: character, name of map service the map type to
    add belongs to

- as_df:

  logical, whether to return a data.frame instead of a list (defaults to
  `FALSE`)

- url_cols:

  logical, whether to include the endpoint URL and auxiliary URL columns
  (defults to `FALSE`). Ignored, if `as_df = FALSE`

- map_type:

  character, name of map type to add

- url_endpoint:

  character, endpoint URL for map service and type to add

- url_xy:

  character, either "xy" or "yx" defining the order the map service to
  add is expecting tile subscripts in (defaults to "xy")

- url_file_format:

  character, file format the endpoint to add is using (defaults to
  ".png").

- url_map_token:

  character, optional, request query string used by the endpoint to add
  for transmitting authentification tokens if required (defaults to
  `NA`). Run `get_maptypes(as_df = T, url_cols = T)$url_map_token` for
  examples.

- auth_error_code:

  numeric, http error code the endpoint to add uses if authentification
  failed (defaults to 401)

- url_website:

  character, optional, website URL for the service to add the user is
  directed to for registering (defaults to `NA`).

- file:

  character, file name ending on ".csv" that the map types table is
  saved to or loaded from.

## Value

A character vector of supported map types

## Details

`get_maptypes()` returns every supported map type that can be used as
input to the `map_type` argument of
[`set_defaults`](https://jakob.schwalb-willmann.de/basemaps/reference/defaults.md),
[`basemap`](https://jakob.schwalb-willmann.de/basemaps/reference/basemap.md)
or associated functions.

`add_maptypes()` adds custom user-defined map types.

`save_maptypes()` saves the current map types table (as printed by
`get_maptypes(as_df = T, url_cols = T)`) to a file.

`load_maptypes()` loads a map types table from file.

## See also

[`basemap`](https://jakob.schwalb-willmann.de/basemaps/reference/basemap.md)

## Examples

``` r
# for all services, as list (default:)
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

# for osm only
get_maptypes("osm")
#> [1] "streets"     "streets_de"  "topographic"
# or
get_maptypes()$osm
#> [1] "streets"     "streets_de"  "topographic"

# for mapbox only
get_maptypes("mapbox")
#> [1] "streets"   "outdoors"  "light"     "dark"      "satellite" "hybrid"   
#> [7] "terrain"  
# or
get_maptypes()$mapbox
#> [1] "streets"   "outdoors"  "light"     "dark"      "satellite" "hybrid"   
#> [7] "terrain"  

# same for all other map services

# or as data.frame:
get_maptypes(as_df = TRUE)
#>          map_service                              map_type
#> 1                osm                               streets
#> 2                osm                            streets_de
#> 3                osm                           topographic
#> 4         osm_stamen                                 toner
#> 5         osm_stamen                              toner_bg
#> 6         osm_stamen                               terrain
#> 7         osm_stamen                            terrain_bg
#> 8         osm_stamen                            watercolor
#> 9         osm_stadia                        alidade_smooth
#> 10        osm_stadia                   alidade_smooth_dark
#> 11        osm_stadia                              outdoors
#> 12        osm_stadia                            osm_bright
#> 13 osm_thunderforest                                 cycle
#> 14 osm_thunderforest                             transport
#> 15 osm_thunderforest                             landscape
#> 16 osm_thunderforest                              outdoors
#> 17 osm_thunderforest                        transport_dark
#> 18 osm_thunderforest                                spinal
#> 19 osm_thunderforest                               pioneer
#> 20 osm_thunderforest                          mobile_atlas
#> 21 osm_thunderforest                         neighbourhood
#> 22 osm_thunderforest                                 atlas
#> 23             carto                                 light
#> 24             carto                       light_no_labels
#> 25             carto                     light_only_labels
#> 26             carto                                  dark
#> 27             carto                        dark_no_labels
#> 28             carto                      dark_only_labels
#> 29             carto                               voyager
#> 30             carto                     voyager_no_labels
#> 31             carto                   voyager_only_labels
#> 32             carto                  voyager_labels_under
#> 33            mapbox                               streets
#> 34            mapbox                              outdoors
#> 35            mapbox                                 light
#> 36            mapbox                                  dark
#> 37            mapbox                             satellite
#> 38            mapbox                                hybrid
#> 39            mapbox                               terrain
#> 40              esri                      natgeo_world_map
#> 41              esri                         usa_topo_maps
#> 42              esri                         world_imagery
#> 43              esri                    world_physical_map
#> 44              esri                   world_shaded_relief
#> 45              esri                      world_street_map
#> 46              esri                    world_terrain_base
#> 47              esri                        world_topo_map
#> 48              esri                  world_dark_gray_base
#> 49              esri             world_dark_gray_reference
#> 50              esri                 world_light_gray_base
#> 51              esri            world_light_gray_reference
#> 52              esri                  world_hillshade_dark
#> 53              esri                       world_hillshade
#> 54              esri                      world_ocean_base
#> 55              esri                 world_ocean_reference
#> 56              esri                     antarctic_imagery
#> 57              esri                        arctic_imagery
#> 58              esri                     arctic_ocean_base
#> 59              esri                arctic_ocean_reference
#> 60              esri world_boundaries_and_places_alternate
#> 61              esri           world_boundaries_and_places
#> 62              esri               world_reference_overlay
#> 63              esri                  world_transportation
#> 64              esri                delorme_world_base_map
#> 65              esri               world_navigation_charts
#> 66          maptiler                             aquarelle
#> 67          maptiler                        aquarelle_dark
#> 68          maptiler                       aquarelle_vivid
#> 69          maptiler                              backdrop
#> 70          maptiler                                 basic
#> 71          maptiler                                bright
#> 72          maptiler                               dataviz
#> 73          maptiler                             landscape
#> 74          maptiler                                 ocean
#> 75          maptiler                               outdoor
#> 76          maptiler                             satellite
#> 77          maptiler                               streets
#> 78          maptiler                                 toner
#> 79          maptiler                                  topo
#> 80          maptiler                                winter

# or as data.frame including all url columns (endpoints):
get_maptypes(as_df = TRUE, url_cols = TRUE)
#>          map_service                              map_type
#> 1                osm                               streets
#> 2                osm                            streets_de
#> 3                osm                           topographic
#> 4         osm_stamen                                 toner
#> 5         osm_stamen                              toner_bg
#> 6         osm_stamen                               terrain
#> 7         osm_stamen                            terrain_bg
#> 8         osm_stamen                            watercolor
#> 9         osm_stadia                        alidade_smooth
#> 10        osm_stadia                   alidade_smooth_dark
#> 11        osm_stadia                              outdoors
#> 12        osm_stadia                            osm_bright
#> 13 osm_thunderforest                                 cycle
#> 14 osm_thunderforest                             transport
#> 15 osm_thunderforest                             landscape
#> 16 osm_thunderforest                              outdoors
#> 17 osm_thunderforest                        transport_dark
#> 18 osm_thunderforest                                spinal
#> 19 osm_thunderforest                               pioneer
#> 20 osm_thunderforest                          mobile_atlas
#> 21 osm_thunderforest                         neighbourhood
#> 22 osm_thunderforest                                 atlas
#> 23             carto                                 light
#> 24             carto                       light_no_labels
#> 25             carto                     light_only_labels
#> 26             carto                                  dark
#> 27             carto                        dark_no_labels
#> 28             carto                      dark_only_labels
#> 29             carto                               voyager
#> 30             carto                     voyager_no_labels
#> 31             carto                   voyager_only_labels
#> 32             carto                  voyager_labels_under
#> 33            mapbox                               streets
#> 34            mapbox                              outdoors
#> 35            mapbox                                 light
#> 36            mapbox                                  dark
#> 37            mapbox                             satellite
#> 38            mapbox                                hybrid
#> 39            mapbox                               terrain
#> 40              esri                      natgeo_world_map
#> 41              esri                         usa_topo_maps
#> 42              esri                         world_imagery
#> 43              esri                    world_physical_map
#> 44              esri                   world_shaded_relief
#> 45              esri                      world_street_map
#> 46              esri                    world_terrain_base
#> 47              esri                        world_topo_map
#> 48              esri                  world_dark_gray_base
#> 49              esri             world_dark_gray_reference
#> 50              esri                 world_light_gray_base
#> 51              esri            world_light_gray_reference
#> 52              esri                  world_hillshade_dark
#> 53              esri                       world_hillshade
#> 54              esri                      world_ocean_base
#> 55              esri                 world_ocean_reference
#> 56              esri                     antarctic_imagery
#> 57              esri                        arctic_imagery
#> 58              esri                     arctic_ocean_base
#> 59              esri                arctic_ocean_reference
#> 60              esri world_boundaries_and_places_alternate
#> 61              esri           world_boundaries_and_places
#> 62              esri               world_reference_overlay
#> 63              esri                  world_transportation
#> 64              esri                delorme_world_base_map
#> 65              esri               world_navigation_charts
#> 66          maptiler                             aquarelle
#> 67          maptiler                        aquarelle_dark
#> 68          maptiler                       aquarelle_vivid
#> 69          maptiler                              backdrop
#> 70          maptiler                                 basic
#> 71          maptiler                                bright
#> 72          maptiler                               dataviz
#> 73          maptiler                             landscape
#> 74          maptiler                                 ocean
#> 75          maptiler                               outdoor
#> 76          maptiler                             satellite
#> 77          maptiler                               streets
#> 78          maptiler                                 toner
#> 79          maptiler                                  topo
#> 80          maptiler                                winter
#>                                                                                                              url_endpoint
#> 1                                                                                         https://tile.openstreetmap.org/
#> 2                                                                               http://tile.openstreetmap.de/tiles/osmde/
#> 3                                                                                           https://tile.opentopomap.org/
#> 4                                                                        https://tiles.stadiamaps.com/tiles/stamen_toner/
#> 5                                                             https://tiles.stadiamaps.com/tiles/stamen_toner_background/
#> 6                                                                      https://tiles.stadiamaps.com/tiles/stamen_terrain/
#> 7                                                           https://tiles.stadiamaps.com/tiles/stamen_terrain_background/
#> 8                                                                   https://tiles.stadiamaps.com/tiles/stamen_watercolor/
#> 9                                                                      https://tiles.stadiamaps.com/tiles/alidade_smooth/
#> 10                                                                https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/
#> 11                                                                           https://tiles.stadiamaps.com/tiles/outdoors/
#> 12                                                                         https://tiles.stadiamaps.com/tiles/osm_bright/
#> 13                                                                                  https://tile.thunderforest.com/cycle/
#> 14                                                                              https://tile.thunderforest.com/transport/
#> 15                                                                              https://tile.thunderforest.com/landscape/
#> 16                                                                               https://tile.thunderforest.com/outdoors/
#> 17                                                                         https://tile.thunderforest.com/transport-dark/
#> 18                                                                             https://tile.thunderforest.com/spinal-map/
#> 19                                                                                https://tile.thunderforest.com/pioneer/
#> 20                                                                           https://tile.thunderforest.com/mobile-atlas/
#> 21                                                                          https://tile.thunderforest.com/neighbourhood/
#> 22                                                                                  https://tile.thunderforest.com/atlas/
#> 23                                                                               https://basemaps.cartocdn.com/light_all/
#> 24                                                                          https://basemaps.cartocdn.com/light_nolabels/
#> 25                                                                       https://basemaps.cartocdn.com/light_only_labels/
#> 26                                                                                https://basemaps.cartocdn.com/dark_all/
#> 27                                                                           https://basemaps.cartocdn.com/dark_nolabels/
#> 28                                                                        https://basemaps.cartocdn.com/dark_only_labels/
#> 29                                                                     https://basemaps.cartocdn.com/rastertiles/voyager/
#> 30                                                            https://basemaps.cartocdn.com/rastertiles/voyager_nolabels/
#> 31                                                         https://basemaps.cartocdn.com/rastertiles/voyager_only_labels/
#> 32                                                        https://basemaps.cartocdn.com/rastertiles/voyager_labels_under/
#> 33                                                             https://api.mapbox.com/styles/v1/mapbox/streets-v11/tiles/
#> 34                                                            https://api.mapbox.com/styles/v1/mapbox/outdoors-v11/tiles/
#> 35                                                               https://api.mapbox.com/styles/v1/mapbox/light-v10/tiles/
#> 36                                                                https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/
#> 37                                                            https://api.mapbox.com/styles/v1/mapbox/satellite-v9/tiles/
#> 38                                                    https://api.mapbox.com/styles/v1/mapbox/satellite-streets-v9/tiles/
#> 39                                                                          https://api.mapbox.com/v4/mapbox.terrain-rgb/
#> 40                                https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/
#> 41                                   https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer/tile/
#> 42                                   https://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/
#> 43                              https://services.arcgisonline.com/arcgis/rest/services/World_Physical_Map/MapServer/tile/
#> 44                             https://services.arcgisonline.com/arcgis/rest/services/World_Shaded_Relief/MapServer/tile/
#> 45                                https://services.arcgisonline.com/arcgis/rest/services/World_Street_Map/MapServer/tile/
#> 46                              https://services.arcgisonline.com/arcgis/rest/services/World_Terrain_Base/MapServer/tile/
#> 47                                  https://services.arcgisonline.com/arcgis/rest/services/World_Topo_Map/MapServer/tile/
#> 48                     https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/
#> 49                https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Dark_Gray_Reference/MapServer/tile/
#> 50                    https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/
#> 51               https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/
#> 52                  https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade_Dark/MapServer/tile/
#> 53                       https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer/tile/
#> 54                          https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/
#> 55                     https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/
#> 56                         https://services.arcgisonline.com/arcgis/rest/services/Polar/Antarctic_Imagery/MapServer/tile/
#> 57                            https://services.arcgisonline.com/arcgis/rest/services/Polar/Arctic_Imagery/MapServer/tile/
#> 58                         https://services.arcgisonline.com/arcgis/rest/services/Polar/Arctic_Ocean_Base/MapServer/tile/
#> 59                    https://services.arcgisonline.com/arcgis/rest/services/Polar/Arctic_Ocean_Reference/MapServer/tile/
#> 60 https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Boundaries_and_Places_Alternate/MapServer/tile/
#> 61           https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/
#> 62               https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Reference_Overlay/MapServer/tile/
#> 63                  https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Transportation/MapServer/tile/
#> 64                https://services.arcgisonline.com/arcgis/rest/services/Specialty/DeLorme_World_Base_Map/MapServer/tile/
#> 65               https://services.arcgisonline.com/arcgis/rest/services/Specialty/World_Navigation_Charts/MapServer/tile/
#> 66                                                                               https://api.maptiler.com/maps/aquarelle/
#> 67                                                                          https://api.maptiler.com/maps/aquarelle-dark/
#> 68                                                                         https://api.maptiler.com/maps/aquarelle-vivid/
#> 69                                                                                https://api.maptiler.com/maps/backdrop/
#> 70                                                                                https://api.maptiler.com/maps/basic-v2/
#> 71                                                                               https://api.maptiler.com/maps/bright-v2/
#> 72                                                                                 https://api.maptiler.com/maps/dataviz/
#> 73                                                                               https://api.maptiler.com/maps/landscape/
#> 74                                                                                   https://api.maptiler.com/maps/ocean/
#> 75                                                                              https://api.maptiler.com/maps/outdoor-v2/
#> 76                                                                               https://api.maptiler.com/maps/satellite/
#> 77                                                                              https://api.maptiler.com/maps/streets-v2/
#> 78                                                                                https://api.maptiler.com/maps/toner-v2/
#> 79                                                                                 https://api.maptiler.com/maps/topo-v2/
#> 80                                                                               https://api.maptiler.com/maps/winter-v2/
#>    url_xy url_file_format  url_map_token auth_error_code
#> 1      xy            .png           <NA>             401
#> 2      xy            .png           <NA>             401
#> 3      xy            .png           <NA>             401
#> 4      xy            .png      ?api_key=             401
#> 5      xy            .png      ?api_key=             401
#> 6      xy            .png      ?api_key=             401
#> 7      xy            .png      ?api_key=             401
#> 8      xy            .jpg      ?api_key=             401
#> 9      xy            .png      ?api_key=             401
#> 10     xy            .png      ?api_key=             401
#> 11     xy            .png      ?api_key=             401
#> 12     xy            .png      ?api_key=             401
#> 13     xy            .png       ?apikey=             403
#> 14     xy            .png       ?apikey=             403
#> 15     xy            .png       ?apikey=             403
#> 16     xy            .png       ?apikey=             403
#> 17     xy            .png       ?apikey=             403
#> 18     xy            .png       ?apikey=             403
#> 19     xy            .png       ?apikey=             403
#> 20     xy            .png       ?apikey=             403
#> 21     xy            .png       ?apikey=             403
#> 22     xy            .png       ?apikey=             403
#> 23     xy            .png           <NA>             401
#> 24     xy            .png           <NA>             401
#> 25     xy            .png           <NA>             401
#> 26     xy            .png           <NA>             401
#> 27     xy            .png           <NA>             401
#> 28     xy            .png           <NA>             401
#> 29     xy            .png           <NA>             401
#> 30     xy            .png           <NA>             401
#> 31     xy            .png           <NA>             401
#> 32     xy            .png           <NA>             401
#> 33     xy                 ?access_token=             401
#> 34     xy                 ?access_token=             401
#> 35     xy                 ?access_token=             401
#> 36     xy                 ?access_token=             401
#> 37     xy                 ?access_token=             401
#> 38     xy                 ?access_token=             401
#> 39     xy            .png ?access_token=             401
#> 40     yx            .png           <NA>             401
#> 41     yx            .png           <NA>             401
#> 42     yx            .png           <NA>             401
#> 43     yx            .png           <NA>             401
#> 44     yx            .png           <NA>             401
#> 45     yx            .png           <NA>             401
#> 46     yx            .png           <NA>             401
#> 47     yx            .png           <NA>             401
#> 48     yx            .png           <NA>             401
#> 49     yx            .png           <NA>             401
#> 50     yx            .png           <NA>             401
#> 51     yx            .png           <NA>             401
#> 52     yx            .png           <NA>             401
#> 53     yx            .png           <NA>             401
#> 54     yx            .png           <NA>             401
#> 55     yx            .png           <NA>             401
#> 56     yx            .png           <NA>             401
#> 57     yx            .png           <NA>             401
#> 58     yx            .png           <NA>             401
#> 59     yx            .png           <NA>             401
#> 60     yx            .png           <NA>             401
#> 61     yx            .png           <NA>             401
#> 62     yx            .png           <NA>             401
#> 63     yx            .png           <NA>             401
#> 64     yx            .png           <NA>             401
#> 65     yx            .png           <NA>             401
#> 66     xy            .png          ?key=             403
#> 67     xy            .png          ?key=             403
#> 68     xy            .png          ?key=             403
#> 69     xy            .png          ?key=             403
#> 70     xy            .png          ?key=             403
#> 71     xy            .png          ?key=             403
#> 72     xy            .png          ?key=             403
#> 73     xy            .png          ?key=             403
#> 74     xy            .png          ?key=             403
#> 75     xy            .png          ?key=             403
#> 76     xy            .jpg          ?key=             403
#> 77     xy            .png          ?key=             403
#> 78     xy            .png          ?key=             403
#> 79     xy            .png          ?key=             403
#> 80     xy            .png          ?key=             403
#>                       url_website
#> 1  https://www.openstreetmap.org/
#> 2  https://www.openstreetmap.org/
#> 3  https://www.openstreetmap.org/
#> 4          https://stadiamaps.com
#> 5          https://stadiamaps.com
#> 6          https://stadiamaps.com
#> 7          https://stadiamaps.com
#> 8          https://stadiamaps.com
#> 9          https://stadiamaps.com
#> 10         https://stadiamaps.com
#> 11         https://stadiamaps.com
#> 12         https://stadiamaps.com
#> 13      https://thunderforest.com
#> 14      https://thunderforest.com
#> 15      https://thunderforest.com
#> 16      https://thunderforest.com
#> 17      https://thunderforest.com
#> 18      https://thunderforest.com
#> 19      https://thunderforest.com
#> 20      https://thunderforest.com
#> 21      https://thunderforest.com
#> 22      https://thunderforest.com
#> 23                           <NA>
#> 24                           <NA>
#> 25                           <NA>
#> 26                           <NA>
#> 27                           <NA>
#> 28                           <NA>
#> 29                           <NA>
#> 30                           <NA>
#> 31                           <NA>
#> 32                           <NA>
#> 33        https://www.mapbox.com/
#> 34        https://www.mapbox.com/
#> 35        https://www.mapbox.com/
#> 36        https://www.mapbox.com/
#> 37        https://www.mapbox.com/
#> 38        https://www.mapbox.com/
#> 39        https://www.mapbox.com/
#> 40                           <NA>
#> 41                           <NA>
#> 42                           <NA>
#> 43                           <NA>
#> 44                           <NA>
#> 45                           <NA>
#> 46                           <NA>
#> 47                           <NA>
#> 48                           <NA>
#> 49                           <NA>
#> 50                           <NA>
#> 51                           <NA>
#> 52                           <NA>
#> 53                           <NA>
#> 54                           <NA>
#> 55                           <NA>
#> 56                           <NA>
#> 57                           <NA>
#> 58                           <NA>
#> 59                           <NA>
#> 60                           <NA>
#> 61                           <NA>
#> 62                           <NA>
#> 63                           <NA>
#> 64                           <NA>
#> 65                           <NA>
#> 66           https://maptiler.com
#> 67           https://maptiler.com
#> 68           https://maptiler.com
#> 69           https://maptiler.com
#> 70           https://maptiler.com
#> 71           https://maptiler.com
#> 72           https://maptiler.com
#> 73           https://maptiler.com
#> 74           https://maptiler.com
#> 75           https://maptiler.com
#> 76           https://maptiler.com
#> 77           https://maptiler.com
#> 78           https://maptiler.com
#> 79           https://maptiler.com
#> 80           https://maptiler.com

# add a custom map service and type yourself:
add_maptypes(
   map_service = "someservice", map_type = "terrain", 
   url_endpoint = "https://tile.someservice.org")

# control further aspects of a custom map service and type:
add_maptypes(
   map_service = "someservice", map_type = "terrain", 
   url_endpoint = "https://tile.someservice.org", 
   url_xy = "xy", #  order in which this service expects tile x and y id
   url_file_format = ".png", 
   url_map_token = "?authtoken=", # query params for auth token
   auth_error_code = 401, 
   url_website = "https://someservice.org"
)
```
