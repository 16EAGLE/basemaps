#' Get a spatial basemap
#'
#' These functions (down)load and cache a basemap of a defined extent \code{ext}, \code{map_service} and \code{map_type} and return it as an object of the defined class. Alternatively to defining the following arguments, \code{\link{set_defaults}} can be used to define basemap preferences once for the running session.
#'
#' @param ext extent to be covered by the basemap as any spatial class supported by \code{st_bbox}.
#' @param map_service character, a map service, either \code{"osm"}, \code{"carto"} or \code{"mapbox"}. Default is \code{"osm"}.
#' @param map_type character, a map type, e.g. \code{"streets"}. For a full list of available map types, see \code{\link{get_maptypes}}.
#' @param map_token character, authentification token for services that require registration, which are \code{"osm_thunderforest"} and \code{"mapbox"}. Register at \url{https://www.thunderforest.com/} and/or \url{https://www.mapbox.com/} to get tokens. Ignored for all other map services.
#' @param map_res numeric, resolution of base map in range from 0 to 1.
#' @param map_dir character, cache directory where downloaded basemap tiles will be stored. By default, a temporary directory is used, which is destroyed when the session is terminated.
#' @param class character, output class, either either \code{plot} (default), \code{magick}, \code{png}, \code{geotif} or if suggested packages are installed, \code{terra}, \code{raster}, \code{stars}, \code{ggplot}, \code{gglayer} or \code{mapview}.
#' @param force logical, whether to force download over cached files or not. Default is \code{FALSE}.
#' @param ... additional arguments, including
#' \itemize{
#'    \item \code{browse}, logical, for \code{class = "png"} and interactive sessions only. Whether to open the png file in the system's default PNG viewer or not. Default is \code{TRUE}.
#'    \item \code{col}, character vector of colours for transforming single-layer basemaps into RGB, if \code{class = "png"} or \code{class = "magick"}. Default is \code{topo.colors(25)}.
#'    \item \code{dpi},  numeric vector of length 1 or 2 specifying the resolution of the image in DPI (dots per inch) for x and y (in that order) - it is recycled to length 2.
#' }
#' @param verbose logical, if \code{TRUE}, messages and progress information are displayed on the console (default).
#' 
#' @return
#' A basemap of the defined class in Web/Pseudo Mercator Projection (EPSG: 3857)
#' 
#' @note 
#' 
#' See \link{get_maptypes} for available map services and their sources.
#' 
#' The use of the map services \code{"osm_thunderforest"} and \code{"mapbox"} require registration to obtain an API token/key which can be supplied to \code{map_token}. Register at \url{https://www.thunderforest.com/} and/or \url{https://www.mapbox.com/} to get a token.
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
#' # for osm_thunderforest and mapbox maps, you need a API token. 
#' # Register for free at thunderforest.com and mapbox.com to get a token
#' 
#' \dontrun{
#' # load and return basemap map as raster (default)
#' map <- basemap(ext)
#' 
#' # or explicitely as different classes such as:
#' basemap_magick(ext)
#' basemap_raster()
#' basemap_stars()
#' 
#' # or as files:
#' basemap_geotif()
#' basemap_png()
#' 
#' # or as plots:
#' basemap_plot(ext)
#' basemap_mapview()
#' 
#' # including ggplot2:
#' basemap_ggplot(ext)
#' 
#' # or as ggplot2 layer:
#' library(ggplot2)
#' ggplot() + 
#'   basemap_gglayer(ext) +
#'   scale_fill_identity() + 
#'   coord_sf()
#' 
#' # or, when combined with an sf vector object,
#' # make sure to use Web/Pseudo Mercator (EPSG 3857), as this is
#' # the CRS in which all basemaps are returned (see "Value"):
#' library(sf)
#' ext <- st_transform(ext,  crs = st_crs(3857))
#' ggplot() + 
#'   basemap_gglayer(ext) + 
#'   geom_sf(data = ext, color = "red", fill = "transparent") +
#'   coord_sf() +
#'   scale_fill_identity()
#'  }
#' @importFrom sf st_bbox 
#' @importFrom terra rast plotRGB plot as.array nlyr
#' @importFrom graphics plot
#' @importFrom magick image_read
#' @importFrom grDevices topo.colors col2rgb
#' @importFrom utils installed.packages
#' @export
#' @name basemap
basemap <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL,
                    map_dir = NULL, class = "plot", force = FALSE, ..., verbose = TRUE){
  
  ## checks
  if(inherits(verbose, "logical")) options(basemaps.verbose = verbose)
  if(is.null(ext)) ext <- getOption("basemaps.defaults")$ext
  if(is.null(ext)) out("Argument 'ext' is not defined and there is no default defined using set_defaults().", type = 3)
  if(!inherits(ext, "list")) ext <- list(ext)
  ext <- lapply(ext, function(x){
    if(inherits(x, "sf")) st_bbox(x) else x
  })
  if(is.null(map_service)) map_service <- getOption("basemaps.defaults")$map_service
  if(is.null(map_type)) map_type <- getOption("basemaps.defaults")$map_type
  if(is.null(map_res)) map_res <- getOption("basemaps.defaults")$map_res
  
  if(is.null(map_token)) map_token <- getOption("basemaps.defaults")$map_token
  if(map_service == "mapbox" & is.na(map_token)) out("You need to define 'map_token' to use map_service 'mapbox'. Register at https://www.mapbox.com/ to obtain a token.", type = 3)
  if(map_service == "osm_thunderforest" & is.na(map_token)) out("You need to define 'map_token' to use map_service 'osm_thunderforest'. Register at https://thunderforest.com to obtain a token.", type = 3)

  extras <- list(...)
  if(!is.null(extras$browse)) browse <- extras$browse else browse <- TRUE
  if(!is.null(extras$col)) col <- extras$col else col <- topo.colors(25)
  if(!is.null(extras$dpi)) dpi <- extras$dpi else dpi <- NULL
  
  if(!is.null(map_dir)) if(!dir.exists(map_dir)){
    out("Directory defined by argument 'map_dir' does not exist, using a temporary directory instead.", type = 2)
    map_dir <- NULL
  }
  if(is.null(map_dir)) map_dir <- getOption("basemaps.defaults")$map_dir
  map_dir <- .add_trailing(map_dir, char = "/")
  class <- tolower(class)
  
  ## transform ext if needed
  crs_webmerc <- st_crs(3857)
  if(any(sapply(ext, function(x) st_crs(x) != crs_webmerc, USE.NAMES = F))){
    out(paste0("Transforming 'ext' to Web Mercator (EPSG: 3857), since 'ext' has a different CRS. The CRS of the returned basemap will be Web Mercator, which is the default CRS used by the supported tile services."), type = 2)
  }
  ext <- lapply(ext, function(x){
    st_bbox(st_transform(st_as_sfc(st_bbox(x)), crs = crs_webmerc)) #bbox as web mercator, always
  })
  
  ## get map
  out(paste0("Loading basemap '", map_type, "' from map service '", map_service, "'..."))
  map_file <- .get_map(ext, map_service, map_type, map_token, map_dir, map_res, force, class, ...)
  
  # return file if needed
  if("geotif" %in% class) return(map_file)
  
  # return terra
  if(any(c("terra", "plot", "png", "magick", "ggplot", "gglayer") %in% class)){
    map <- terra::rast(map_file)
    
    if("terra" %in% class) return(map)
    
    if("plot" %in% class){
      dim_map <- dim(map) 
      
      if(dim_map[3] == 3){
        terra::plotRGB(map, r = 1, g = 2, b = 3, maxcell = ncell(map))
      } else{
        terra::plot(
          map[[1]],
          col = col, type = "continous",
          breaks = seq(min(map[[1]][]), max(map[[1]][]), length.out = length(col)+1)
        )
      }
    }
    
    if(any("png" == class, "magick" == class)){
      if(!any(grepl("png", rownames(installed.packages())))){
        out(paste0("Package 'png' is not installed, but needed for class='", class, "'. Please install 'png' using install.packages('png')."), type = 3)
      } else{
        
        file <- paste0(map_dir, "/", map_service, "_", map_type, "_", gsub(":", "-", gsub(" ", "_", Sys.time())), ".png")
        map_arr <- terra::as.array(map)
        
        if(dim(map_arr)[3] == 3){
          #for(i in 1:dim(map_arr)[3]) map_arr[,,i] <- t(map_arr[,,i])
          #map_arr <- aperm(map_arr, c(2, 1, 3)) ### ONLY WITH STARS
          map_arr <- sweep(map_arr, MARGIN = 3, STATS = max(map_arr), FUN = "/")
          png::writePNG(map_arr, target = file, dpi = dpi)
        } else{
          map_arr <- map_arr[,,1]
          # convert to range 0 to 1
          #map_arr <- sweep(map_arr, MARGIN = 1, STATS = max(map_arr), FUN = "/")
          map_arr <- ((map_arr - min(map_arr))/(max(map_arr) - min(map_arr)))
          # map col to value range
          map_arr_col <- col[findInterval(map_arr, seq(0, 1, length.out = length(col)))]
          # convert hex to rgb
          map_arr_rgb <- col2rgb(map_arr_col)
          # switch dimensions to fit writeRGB
          map_arr_rgb <- aperm(array(map_arr_rgb, c(3, dim(map_arr))), c(2,3,1))
          #map_arr_rgb <- array(map_arr_rgb, c(dim(map_arr), 3))
          # go back to 0 to 1 again
          map_arr_rgb <- sweep(map_arr_rgb, MARGIN = 3, STATS = max(map_arr_rgb), FUN = "/")
          png::writePNG(map_arr_rgb, target = file, dpi = dpi)
        }
        if(grepl("png", class)){
          if(all(isTRUE(interactive()), isTRUE(browse))) utils::browseURL(file)
          return(file)
        }
        if(grepl("magick", class)){
          return(image_read(file))
        }
      }
    }
    
    if("ggplot" %in% class){
      if(!any(grepl("ggplot", rownames(installed.packages())))){
        out(paste0("Package 'ggplot2' is not installed, but needed for class='", class, "'. Please install 'ggplot2' using install.packages('ggplot2')."), type = 3)
      } else{
        if(terra::nlyr(map) == 3){
          return(gg_raster(r = map, r_type = "RGB", ...))
        } else{
          return(gg_raster(r = map, r_type = "gradient", ...))
        }
      }
    }
    
    if("gglayer" %in% class){
      if(!any(grepl("ggplot", rownames(installed.packages())))){
        out(paste0("Package 'ggplot2' is not installed, but needed for class='", class, "'. Please install 'ggplot2' using install.packages('ggplot2')."), type = 3)
      } else{
        if(terra::nlyr(map) == 3) return(gg_raster(r = map, r_type = "RGB", gglayer = T, ...)) else return(gg_raster(r = map, r_type = "gradient", gglayer = T, ...))
      }
    }
  }
  
  # stars
  if("stars" %in% class){
    if(!any(grepl("stars", rownames(installed.packages())))){
      out(paste0("Package 'stars' is not installed, but needed for class='", class, "'. Please install 'stars' using install.packages('stars')."), type = 3)
    } else{
      map <- stars::read_stars(map_file)
      return(map)
    }
  }
  
  # raster-based
  if(any(c("raster", "mapview", "ggplot", "gglayer") %in% class)){
    if(!any(grepl("raster", rownames(installed.packages())))){
      out(paste0("Package 'raster' is not installed, but needed for class='", class, "'. Please install 'raster' using install.packages('raster')."), type = 3)
    } else{
      
      map <- quiet(raster::brick(map_file))
      if("raster" %in% class){
        if(raster::nlayers(map) == 1) map <- raster::raster(map)
        return(map)
      }
      
      if("mapview" %in% class){
        if(!any(grepl("mapview", rownames(installed.packages())))){
          out(paste0("Package 'mapview' is not installed, but needed for class='", class, "'. Please install 'mapview' using install.packages('mapview')."), type = 3)
        } else{
          quiet(if(raster::nlayers(map) == 3){
            return(mapview::viewRGB(map, 1, 2, 3, layer.name = "Basemap", maxpixels = raster::ncell(map), quantiles = NULL))
          } else return(mapview::mapview(map)))
        }
      }
    }
  }
}


#' @rdname basemap
#' @export
basemap_plot <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "plot", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_magick <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "magick", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_png <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "png", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_geotif <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "geotif", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_terra <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "terra", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_raster <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "raster", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_stars <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "stars", force, ..., verbose = verbose)
}


#' @rdname basemap
#' @export
basemap_ggplot <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "ggplot", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_gglayer <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "gglayer", force, ..., verbose = verbose)
}

#' @rdname basemap
#' @export
basemap_mapview <- function(ext = NULL, map_service = NULL, map_type = NULL, map_res = NULL, map_token = NULL, map_dir = NULL, force = NULL, ..., verbose = TRUE){
  basemap(ext, map_service, map_type, map_res, map_token, map_dir, class = "mapview", force, ..., verbose = verbose)
}
