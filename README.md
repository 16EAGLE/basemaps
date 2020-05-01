# `basemaps`

A lightweight `R` package to load spatial basemaps from open sources such as OpenStreetMap, Carto and Mapbox and return them as many different object classes as needed.

## Get started

``` r
library(basemaps)
data(ext)

# view all available maps
get_maptypes()
#> $osm
#>  [1] "streets"      "streets_de"   "streets_fr"   "humanitarian" "topographic" 
#>  [6] "roads"        "hydda"        "hydda_base"   "hike"         "grayscale"   
#> [11] "no_labels"    "watercolor"   "toner"        "toner_bg"     "toner_lite"  
#> [16] "terrain"      "terrain_bg"   "mtb"         
#> 
#> $carto
#>  [1] "light"                "light_no_labels"      "light_only_labels"   
#>  [4] "dark"                 "dark_no_labels"       "dark_only_labels"    
#>  [7] "voyager"              "voyager_no_labels"    "voyager_only_labels" 
#> [10] "voyager_labels_under"
#> 
#> $mapbox
#>  [1] "satellite"     "terrain"       "streets"       "streets_basic"
#>  [5] "hybrid"        "light"         "dark"          "high_contrast"
#>  [9] "outdoors"      "hike"          "wheatpaste"    "pencil"       
#> [13] "comic"         "pirates"       "emerald"      
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
