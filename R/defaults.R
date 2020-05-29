#' Set \code{basemaps} defaults
#'
#' These functions set or get the defaults of all map arguments passed to \code{\link{basemap}} and associated functions.
#' 
#' @inheritParams basemap
#' @return None
#' 
#' @examples
#' library(basemaps)
#' data(ext)
#' 
#' # set defaults for the basemap
#' set_defaults(ext = ext, map_service = "osm", map_type = "terrain_bg")
#' # for mapbox maps, you need a map_token. Register for free at mapbox.com to get a token
#' 
#' \dontrun{
#' # load and return basemap map as raster (default)
#' map <- basemap()
#' }
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