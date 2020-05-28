#' Get a spatial basemap
#'
#' These functions (down)load and cach a basemap of a defined extent \code{ext}, \code{map_service} and \code{map_type} and return it as a object of the defined class.
#'
#' @param ext extent to be covered by the basemap. Any spatial class.
#' @param map_service character, 
#' @param map_type character, 
#' @param map_token character,
#' @param map_res numeric,
#' @param map_dir character, cache directory
#' @param class character, output class.
#' @param ... additional arguments.
#' @param verbose
#' 
#' @return
#' 
#' @importFrom sf st_bbox 
#' @importFrom raster nlayers plotRGB plot
#' @importFrom stars st_as_stars
#' @importFrom mapview mapview viewRGB
#' @export
#' 
basemap <- function(ext, map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, class = "raster", 
                    ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(basemaps.verbose = verbose)
  if(inherits(ext, "sf")) ext <- st_bbox(ext)
  if(is.na(map_service)) map_service = getOption("basemaps.defaults")$map_service
  if(is.na(map_type)) map_type = getOption("basemaps.defaults")$map_type
  if(is.na(map_res)) map_res = getOption("basemaps.defaults")$map_res
  if(is.na(map_token)) map_token = getOption("basemaps.defaults")$map_token
  
  if(!is.na(map_dir)) if(!dir.exists(map_dir)){
    out("Directory defined by argument 'map_dir' does not exist, using a temporary directory instead.", type = 2)
    map_dir <- NA
  }
  if(is.na(map_dir)) map_dir <- getOption("basemaps.defaults")$map_dir
  class <- tolower(class)
  
  ## get map
  out(paste0("Loading basemap '", map_type, "' from map service '", map_service, "'..."))
  ext <- st_bbox(ext)
  map <- .get_map(ext, map_service, map_type, map_token, map_dir, map_res)
  
  ## define class
  if("raster" %in% class) return(map)
  if("stars" %in% class) return(st_as_stars(map))
  if("mapview" %in% class) quiet(if(nlayers(map) == 3) return(viewRGB(map, 1, 2, 3)) else return(mapview(map)))
  if("plot" == class) if(nlayers(map) == 3) return(plotRGB(map)) else return(plot(map))
  if("ggplot" %in% class) if(nlayers(map) == 3) return(gg.bmap(r = map, r_type = "RGB", ...)) else return(gg.bmap(r = map, r_type = "gradient", ...))
  if("gglayer" %in% class) if(nlayers(map) == 3) return(gg.bmap(r = map, r_type = "RGB", gglayer = T, ...)) else return(gg.bmap(r = map, r_type = "gradient", gglayer = T, ...))
}

#' @inheritParams basemap
#' @export
basemap_raster <- function(ext,  map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "raster", ..., verbose = verbose)
}


#' @inheritParams basemap
#' @export
basemap_stars <- function(ext,  map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "stars", ..., verbose = verbose)
}

#' @inheritParams basemap
#' @export
basemap_mapview <- function(ext,  map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "mapview", ..., verbose = verbose)
}

#' @inheritParams basemap
#' @export
basemap_plot <- function(ext,  map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "plot", ..., verbose = verbose)
}

#' @inheritParams basemap
#' @export
basemap_ggplot <- function(ext,  map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "ggplot", ..., verbose = verbose)
}

#' @inheritParams basemap
#' @export
basemap_gglayer <- function(ext,  map_service = NA, map_type = NA, map_res = NA, map_token = NA, map_dir = NA, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "gglayer", ..., verbose = verbose)
}