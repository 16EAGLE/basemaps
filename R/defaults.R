#' Set, get and reset \code{basemaps} defaults
#'
#' These functions set, get or reset the defaults of all map arguments passed to \code{\link{basemap}} and associated functions.
#' 
#' @inheritParams basemap
#' @return For \code{get_defaults}, a list of defaults, otherwise none.
#' 
#' @examples
#' library(basemaps)
#' data(ext)
#' 
#' # set defaults for the basemap
#' set_defaults(ext = ext, map_service = "osm", map_type = "terrain_bg")
#' 
#' # get defaults
#' get_defaults()
#' 
#' \dontrun{
#' # load and return basemap map as raster (default)
#' map <- basemap()
#' }
#' 
#' # reset defaults
#' reset_defaults()
#' 
#' @name defaults
#' @export

set_defaults <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL){
  
  defaults <- getOption("basemaps.defaults")
  if(!is.null(ext)) defaults$ext <- ext
  if(!is.null(map_service)) defaults$map_service <- map_service
  if(!is.null(map_type)) defaults$map_type <- map_type
  if(!is.null(map_res)) defaults$map_res <- map_res
  if(!is.null(map_token)) defaults$map_token <- map_token
  if(!is.null(map_dir)) defaults$map_dir <- map_dir
  options(basemaps.defaults = defaults)
}

#' @rdname defaults
#' @export
get_defaults <- function(){
  getOption("basemaps.defaults")
}

#' @rdname defaults
#' @export
reset_defaults <- function(){
  options(basemaps.defaults = .defaults())
}


#' Get supported map types or add new map types
#'
#' These functions manage map types. 
#'
#' \code{get_maptypes()} returns every supported map type that can be used as input to the \code{map_type} argument of \code{\link{set_defaults}}, \code{\link{basemap}} or associated functions.
#' 
#' \code{add_maptypes()} adds custom user-defined map types.
#' 
#' \code{save_maptypes()} saves the current map types table (as printed by \code{get_maptypes(as_df = T, url_cols = T)}) to a file.
#' 
#' \code{load_maptypes()} loads a map types table from file.
#' 
#' @param map_service character. 
#' \itemize{
#'  \item for \code{get_maptypes()}: optional, either \code{"osm"}, \code{"osm_stamen"}, \code{"osm_stadia"}, \code{"osm_thunderforest"},  \code{"carto"}, \code{"mapbox"}, \code{"esri"} or \code{"maptiler"}. Otherwise, a list of map types for both services is returned.
#'  \item for \code{add_maptypes()}: character, name of map service the map type to add belongs to
#' }
#'  
#' @param as_df logical, whether to return a data.frame instead of a list (defaults to \code{FALSE})
#' @param url_cols logical, whether to include the endpoint URL and auxiliary URL columns (defults to \code{FALSE}). Ignored, if \code{as_df = FALSE}
#' @param map_type character, name of map type to add
#' @param url_endpoint character, endpoint URL for map service and type to add
#' @param url_xy character, either "xy" or "yx" defining the order the map service to add is expecting tile subscripts in (defaults to "xy")
#' @param url_file_format character, file format the endpoint to add is using (defaults to ".png").
#' @param url_map_token character, optional, request query string used by the endpoint to add for transmitting authentification tokens if required (defaults to \code{NA}). Run \code{get_maptypes(as_df = T, url_cols = T)$url_map_token} for examples.
#' @param auth_error_code numeric, http error code the endpoint to add uses if authentification failed (defaults to 401)
#' @param url_website character, optional, website URL for the service to add the user is directed to for registering (defaults to \code{NA}).
#' @param file character, file name ending on ".csv" that the map types table is saved to or loaded from.
#' 
#' @return A character vector of supported map types
#' 
#' 
#' @source 
#' 
#' \code{"osm"}: Open Street Map contributors (\url{https://www.openstreetmap.org/copyright}), Open Topo Map (\url{https://opentopomap.org/})
#' 
#' \code{"osm_stamen"}: Stamen (\url{https://maps.stamen.com/}) via Stadia Maps (\url{https://stadiamaps.com/}), Open Street Map contributors (\url{https://www.openstreetmap.org/copyright})
#' 
#' \code{"osm_stadia"}: Stadia Maps (\url{https://stadiamaps.com/}), Open Street Map contributors (\url{https://www.openstreetmap.org/copyright})
#' 
#' \code{"osm_thunderforest"}: Thunderforest (\url{https://www.thunderforest.com/}), Open Street Map contributors (\url{https://www.openstreetmap.org/copyright})
#'
#' \code{"carto"}: Carto (\url{https://carto.com/})
#' 
#' \code{"mapbox"}: Mapbox (\url{https://www.mapbox.com})
#' 
#' \code{"esri"}: Esri (\url{https://www.esri.com/en-us/home})
#' 
#' \code{"maptiler"}: Esri (\url{https://www.maptiler.com})
#'
#' @examples 
#' # for all services, as list (default:)
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
#' # or as data.frame:
#' get_maptypes(as_df = TRUE)
#' 
#' # or as data.frame including all url columns (endpoints):
#' get_maptypes(as_df = TRUE, url_cols = TRUE)
#' 
#' # add a custom map service and type yourself:
#' add_maptypes(
#'    map_service = "someservice", map_type = "terrain", 
#'    url_endpoint = "https://tile.someservice.org")
#' 
#' # control further aspects of a custom map service and type:
#' add_maptypes(
#'    map_service = "someservice", map_type = "terrain", 
#'    url_endpoint = "https://tile.someservice.org", 
#'    url_xy = "xy", #  order in which this service expects tile x and y id
#'    url_file_format = ".png", 
#'    url_map_token = "?authtoken=", # query params for auth token
#'    auth_error_code = 401, 
#'    url_website = "https://someservice.org"
#' )
#' 
#' @seealso \code{\link{basemap}}
#' @name maptypes
#' @export

get_maptypes <- function(map_service = NULL, as_df = FALSE, url_cols = FALSE){
  mt <- getOption("basemaps.map_api")
  if(!url_cols) mt <- mt[,c("map_service", "map_type")]
  if(!as_df){
    ms <- unique(mt$map_service)
    mt <- lapply(ms, function(.ms) mt[mt$map_service == .ms, "map_type"])
    names(mt) <- ms
    if(!is.null(map_service)) mt[[map_service]] else mt
  } else {
    if(!is.null(map_service)) mt[mt$map_service == map_service, ] else mt
  }
  
}

#' @rdname maptypes
#' @export
add_maptypes <- function(map_service, map_type, url_endpoint, url_xy = "xy", url_file_format = ".png", 
                         url_map_token = as.character(NA), auth_error_code = 401, url_website = as.character(NA)){
  
  # checks
  if(!is.character(map_service)) out("Argument 'map_service' must be a character.", type = 3)
  if(!is.character(map_type)) out("Argument 'map_type' must be a character.", type = 3)
  if(!is.character(url_endpoint)) out("Argument 'url_endpoint' must be a character.", type = 3)
  if(!is.character(url_xy)) out("Argument 'url_xy' must be a character.", type = 3)
  if(!is.character(url_file_format)) out("Argument 'url_file_format' must be a character.", type = 3)
  if(!is.character(url_map_token)) out("Argument 'url_map_token' must be a character.", type = 3)
  if(!is.numeric(auth_error_code)) out("Argument 'auth_error_code' must be numeric.", type = 3)
  if(!is.character(url_website)) out("Argument 'url_website' must be a character.", type = 3)
  
  if(!all(url_xy %in% c("xy", "yx"))) out("Argument 'url_xy' must only contain 'xy' and/or 'yx'.", type = 3)
  
  if(length(unique(c(length(map_service), length(map_type), length(url_endpoint)))) != 1){
    out("Length of arguments 'map_service', 'map_type' and 'url_endpoint' differ.", type = 3)
  }
  if(length(unique(length(url_xy), length(url_map_token), length(auth_error_code), length(url_website))) != 1){
    out("Length of arguments 'url_xy', 'url_map_token', 'auth_error_code' and 'url_website' differ.", type = 3)
  }
  if(length(url_xy) == 1){
    url_xy <- rep(url_xy, length(map_service))
    url_map_token <- rep(url_map_token, length(map_service))
    auth_error_code <- rep(auth_error_code, length(map_service))
    url_website <- rep(url_website, length(map_service))
  } else{
    if(length(url_xy) != length(map_service)) out("Length of arguments 'url_xy', 'url_map_token', 'auth_error_code' and 'url_website' are different from length of arguments 'map_service', 'map_type' and 'url_endpoint'.", type = 3)
  }
  
  mt_add <- do.call(rbind, lapply(1:length(map_service), function(i){
    out(paste0("Adding map service '", map_service[i], "' with map type '", map_type[i], "' and URL endpoint '", url_endpoint[i], "' to basemaps."))
    cbind.data.frame(
      map_service = map_service[i], 
      map_type = map_type[i],
      url_endpoint = url_endpoint[i], 
      url_xy = url_xy[i],
      url_file_format = url_file_format[i],
      url_map_token = url_map_token[i],
      auth_error_code = auth_error_code[i],
      url_website = url_website[i]
    )
  }))
  
  mt <- get_maptypes(as_df = T, url_cols = T)
  mt <- rbind(mt, mt_add)
  options(basemaps.map_api = mt)
  
}

#' @rdname maptypes
#' @importFrom utils write.csv
#' @export
save_maptypes <- function(file){
  mt <- get_maptypes(as_df = T, url_cols = T)
  write.csv(mt, file = file, row.names = F)
  out(paste0("Saved map types to '", file, "'."))
}


#' @rdname maptypes
#' @importFrom utils read.csv
#' @export
load_maptypes <- function(file){
  mt <- read.csv(file)
  options(basemaps.map_api = mt)
  out(paste0("Loaded map types from '", file, "'."))
}

