#' Set \code{basemaps} defaults
#'
#' These functions set or get the defaults of all map arguments passed to \code{\link{basemap}} and associated functions.
#' 
#' @inheritParams basemap
#' @return None
#' 
#' @name defaults
#' @export

set_defaults <- function(map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL){
  
  defaults <- getOption("basemaps.defaults")
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