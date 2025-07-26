#' Draw extent
#'
#' This function lets you draw an extent on an interactive map. It is a simple wrapper around \code{mapedit::drawFeatures()} written by Tim Appelhans et al.
#' 
#' @return An \code{sf} object
#' 
#' @examples
#' \dontrun{
#' library(basemaps)
#' 
#' # draw extent interactively
#' ext <- draw_ext()
#' 
#' # set defaults for the basemap
#' set_defaults(ext = ext, map_service = "osm", map_type = "terrain_bg")
#' # for mapbox maps, you need a map_token. Register for free at mapbox.com to get a token
#' 
#' # load and return basemap map as raster (default)
#' map <- basemap()
#' }
#' 
#' @export
draw_ext <- function(){
  if(any(grepl("mapedit", rownames(utils::installed.packages())))){
    mapedit::drawFeatures(title = "Draw a basemap extent")
  } else{
    out("Package 'mapedit' was not found. Please install 'mapedit' to use this function.", type = 3)
  }
}