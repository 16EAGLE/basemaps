#' Get a spatial basemap
#'
#' These functions (down)load and cache a basemap of a defined extent \code{ext}, \code{map_service} and \code{map_type} and return it as an object of the defined class. Alternatively to defining the following arguments, \code{\link{set_defaults}} can be used to define basemap preferences once for the running session.
#'
#' @param ext extent to be covered by the basemap as any spatial class supported by \code{st_bbox}.
#' @param map_service character, a map service, either \code{"osm"}, \code{"carto"} or \code{"mapbox"}. Default is \code{"osm"}.
#' @param map_type character, a map type, e.g. \code{"streets"}. For a full list of available map types, see \code{\link{get_maptypes}}.
#' @param map_token character, mapbox authentification token for mapbox basemaps. Register at \url{https://www.mapbox.com/} to get a mapbox token. Mapbox is free of charge after registration for up to 50.000 map requests per month. Ignored, if \code{map_service = "osm"}.
#' @param map_res numeric, resolution of base map in range from 0 to 1.
#' @param map_dir character, cache directory where downloaded basemap tiles will be stored. By default, a temporary directory is used, which is destroyed when the session is terminated.
#' @param class character, output class, either \code{"raster"}, \code{"stars"}, \code{"mapview"}, \code{"plot"}, \code{"ggplot"}, \code{"gglayer"}, \code{"magick"} or \code{"png"}.
#' @param ... additional arguments, including
#' \itemize{
#'    \item \code{browse}, logical, for \code{class = "png"} and interactive sessions only. Whether to open the png file in the system's default PNG viewer or not. Default is \code{TRUE}.
#'    \item \code{col}, character vector of colours for transforming single-layer basemaps into RGB, if \code{class = "png"} or \code{class = "magick"}. Default is \code{topo.colors(25)}.
#' }
#' @param verbose logical, if \code{TRUE}, messages and progress information are displayed on the console (default).
#' 
#' @return
#' A basemap of the defined class
#' 
#' 
#' @examples
#' library(basemaps)
#' 
#' # example extent
#' data(ext)
#' 
#' # view all available maps
#' get_maptypes()
#' 
#' # set defaults for the basemap
#' set_defaults(map_service = "osm", map_type = "terrain_bg")
#' # for mapbox maps, you need a map_token. Register for free at mapbox.com to get a token
#' 
#' \dontrun{
#' # load and return basemap map as raster (default)
#' map <- basemap(ext)
#' 
#' # or as many different classes such as:
#' basemap_magick(ext)
#' basemap_plot(ext)
#' }
#' @importFrom sf st_bbox 
#' @importFrom raster nlayers plotRGB plot ncell RGB
#' @importFrom slippymath raster_to_png
#' @importFrom magick image_read
#' @importFrom grDevices topo.colors
#' @importFrom utils installed.packages
#' @export
#' @name basemap
basemap <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, class = "raster", 
                    ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(basemaps.verbose = verbose)
  if(is.null(ext)) ext <- getOption("basemaps.defaults")$ext
  if(is.null(ext)) out("Argument 'ext' is not defined and there is no default defined using set_defaults().", type = 3)
  if(inherits(ext, "sf")) ext <- st_bbox(ext)
  if(is.null(map_service)) map_service <- getOption("basemaps.defaults")$map_service
  if(is.null(map_type)) map_type <- getOption("basemaps.defaults")$map_type
  if(is.null(map_res)) map_res <- getOption("basemaps.defaults")$map_res
  if(is.null(map_token)) map_token <- getOption("basemaps.defaults")$map_token

  extras <- list(...)
  if(!is.null(extras$browse)) browse <- extras$browse else browse <- TRUE
  if(!is.null(extras$col)) col <- extras$col else col <- topo.colors(25)
  
  
  if(!is.null(map_dir)) if(!dir.exists(map_dir)){
    out("Directory defined by argument 'map_dir' does not exist, using a temporary directory instead.", type = 2)
    map_dir <- NULL
  }
  if(is.null(map_dir)) map_dir <- getOption("basemaps.defaults")$map_dir
  class <- tolower(class)
  
  ## get map
  out(paste0("Loading basemap '", map_type, "' from map service '", map_service, "'..."))
  ext <- st_bbox(ext)
  map <- .get_map(ext, map_service, map_type, map_token, map_dir, map_res, ...)
  
  ## define class
  if("raster" %in% class) return(map)
  if("plot" == class) if(nlayers(map) == 3) return(plotRGB(map)) else return(plot(map))
  if(any("png" == class, "magick" == class)){
    file <- paste0(map_dir, "/", map_service, "_", map_type, "_", gsub(":", "-", gsub(" ", "_", Sys.time())), ".png")
    if(nlayers(map) == 1) map <- RGB(map, col = col)
    raster_to_png(map, file)
    
    if(grepl("png", class)){
      if(all(isTRUE(interactive()), isTRUE(browse))) utils::browseURL(file)
      return(file)
    }
    if(grepl("magick", class)){
      return(image_read(file))
    }
  }
  if("stars" %in% class){
    if(any(grepl("stars", rownames(installed.packages())))){
      return(stars::st_as_stars(map))
    } else{
      out("Package 'stars' is not installed. Please install 'stars' using install.packages('stars').")
    }
  }
  if("mapview" %in% class){
    if(any(grepl("mapview", rownames(installed.packages())))){
      quiet(if(nlayers(map) == 3) return(mapview::viewRGB(map, 1, 2, 3, layer.name = "Basemap", maxpixels = ncell(map))) else return(mapview::mapview(map)))
    } else{
      out("Package 'mapview' is not installed. Please install 'mapview' using install.packages('mapview').")
    }
  }
  if("ggplot" %in% class){
    if(any(grepl("ggplot", rownames(installed.packages())))){
      if(nlayers(map) == 3) return(gg.bmap(r = map, r_type = "RGB", ...)) else return(gg.bmap(r = map, r_type = "gradient", ...))
    } else{
      out("Package 'ggplot2' is not installed. Please install 'ggplot2' using install.packages('ggplot2').")
    }
  }
  if("gglayer" %in% class){
    if(any(grepl("ggplot", rownames(installed.packages())))){
      if(nlayers(map) == 3) return(gg.bmap(r = map, r_type = "RGB", gglayer = T, ...)) else return(gg.bmap(r = map, r_type = "gradient", gglayer = T, ...))
    } else{
      out("Package 'ggplot2' is not installed. Please install 'ggplot2' using install.packages('ggplot2').")
    }
  }
}

#' @rdname basemap
#' @export
basemap_raster <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "raster", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_stars <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "stars", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_mapview <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "mapview", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_plot <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "plot", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_ggplot <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "ggplot", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_gglayer <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "gglayer", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_magick <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "magick", ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_png <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "png", ..., verbose = verbose)
}