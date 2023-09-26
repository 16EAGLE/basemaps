#' Get all supported map types
#'
#' This function returns every supported map type that can be used as input to the \code{map_type} argument of \code{\link{set_defaults}}, \code{\link{basemap}} or associated functions.
#'
#' @param map_service character, optional, either \code{"osm"}, \code{"osm_stamen"},  \code{"osm_thunderforest"},  \code{"carto"}, \code{"mapbox"} or \code{"esri"}. Otherwise, a list of map types for both services is returned.
#' @return A character vector of supported map types
#' 
#' 
#' @source 
#' 
#' \code{"osm"}: Open Street Map contributors (\url{https://www.openstreetmap.org/copyright}), Open Topo Map (\url{https://opentopomap.org/}), Martin Tesar (\url{http://mtbmap.cz/})
#' 
#' \code{"osm_stamen"}: Stamen (\url{http://maps.stamen.com/}), Open Street Map contributors (\url{https://www.openstreetmap.org/copyright})
#' 
#' \code{"osm_thunderforest"}: Thunderforest (\url{https://www.thunderforest.com/}), Open Street Map contributors (\url{https://www.openstreetmap.org/copyright})
#'
#' \code{"carto"}: Carto (\url{https://carto.com/})
#' 
#' \code{"mapbox"}: Mapbox (\url{https://www.mapbox.com})
#' 
#' \code{"esri"}: Esri (\url{https://www.esri.com/en-us/home})
#'
#' \code{"usgs"}: USGS (\url{https://basemap.nationalmap.gov/arcgis/rest/services})
#' 
#' @examples 
#' # for all services
#' get_maptypes()
#' 
#' # for osm only
#' get_maptypes("osm")
#' # or
#' get_maptypes()$osm
#' 
#' # for mapbox only
#' get_maptypes("mapbox")
#' # or
#' get_maptypes()$mapbox
#' 
#' # same for all other map services
#' 
#' @seealso \code{\link{basemap}}
#' @export

get_maptypes <- function(map_service = NULL){
  map_types <- lapply(getOption("basemaps.map_api"), names)
  if(!is.null(map_service)) map_types[[map_service]] else map_types
}
