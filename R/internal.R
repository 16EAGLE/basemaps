#' Suppress messages and warnings
#' @noRd 
quiet <- function(expr){
  #return(expr)
  return(suppressWarnings(suppressMessages(expr)))
}

#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("basemaps.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}


#' verbose apply
#'
#' @importFrom pbapply pbapply
#' @noRd 
.apply <- function(X, MARGIN, FUN, ...){
  verbose = getOption("basemaps.verbose")
  if(isTRUE(verbose)) pbapply(X, MARGIN, FUN, ...) else apply(X, MARGIN, FUN, ...)
}

#' combine two extents into one
#' @noRd 
.combine_ext <- function(ext.both){
  ext.combi <- ext.both[[1]]
  ext.combi@xmin <- min(c(ext.both[[1]]@xmin, ext.both[[2]]@xmin))
  ext.combi@xmax <- max(c(ext.both[[1]]@xmax, ext.both[[2]]@xmax))
  return(ext.combi)
}

#' expand two extents by the one with larger x range
#' @noRd 
.expand_ext <- function(ext.both, rg){
  if(which.min(rg) == 1){
    ext.both[[which.min(rg)]]@xmin <- -180+ext.both[[which.min(rg)]]@xmin-180
    ext.both[[which.min(rg)]]@xmax <- -180+ext.both[[which.min(rg)]]@xmax-180
  } else{
    ext.both[[which.max(rg)]]@xmin <- 180+ext.both[[which.max(rg)]]@xmin+180
    ext.both[[which.max(rg)]]@xmax <- 180+ext.both[[which.max(rg)]]@xmax+180
  }
  return(ext.both)
}

#' plot raster as ggplot
#' @importFrom raster ncell aggregate
#' @noRd 
gg.bmap <- function(r, r_type, gglayer = F, ...){
  extras <- list(...)
  if(!is.null(extras$maxpixels)) maxpixels <- extras$maxpixels else maxpixels <- 500000
  if(!is.null(extras$alpha)) alpha <- extras$alpha else alpha <- 1
  if(!is.null(extras$maxColorValue)) maxColorValue <- extras$maxColorValue else maxColorValue <- NA
  
  # aggregate raster if too large
  if(maxpixels < ncell(r)) r <- aggregate(r, fact = ceiling(ncell(r)/maxpixels))
  
  # transform into data.frame
  df <- data.frame(raster::as.data.frame(r, xy = T))
  colnames(df) <- c("x", "y", paste0("val", 1:(ncol(df)-2)))
  
  # factor if discrete to show categrocial legend
  df$fill <- df$val1
  if(r_type == "discrete") df$fill <- as.factor(df$fill)
  
  # transform to RGB colours
  if(r_type == "RGB"){
    if(is.na(maxColorValue)) maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = T)
    
    if(maxColorValue < max(c(df$val1, df$val2, df$val3), na.rm = T)){
      out("maxColorValue < maximum raster value. maxColorValue is set to maximum raster value.", type = 2)
      maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = T)
    }
    
    # remove NAs
    na.sel <- is.na(df$val1) & is.na(df$val2) & is.na(df$val3)
    if(any(na.sel)) df <- df[!na.sel,]
    
    df$fill <- grDevices::rgb(red = df$val1, green = df$val2, blue = df$val3, maxColorValue = maxColorValue)
  } else{
    
    # remove NAs
    na.sel <- is.na(df$val1)
    if(any(na.sel)) df <- df[!na.sel,]
  }
  # if NA gaps are there, use geom_tile, otherwise make it fast using geom_raster
  if(any(na.sel)){
    gg <- ggplot2::geom_tile(ggplot2::aes_string(x = "x", y = "y", fill = "fill"), data = df, alpha = alpha)
  } else{
    gg <- ggplot2::geom_raster(ggplot2::aes_string(x = "x", y = "y", fill = "fill"), data = df, alpha = alpha)
  }
  
  if(isFALSE(gglayer)){
    gg <- ggplot2::ggplot() + gg + ggplot2::coord_sf()
    if(r_type == "RGB") gg <- gg + ggplot2::scale_fill_identity() 
  }
  return(gg)
}


#' get map
#' @importFrom slippymath bbox_to_tile_grid compose_tile_grid
#' @importFrom raster projectRaster extent extent<- resample extend merge crs crop writeRaster nlayers brick
#' @importFrom magick image_read image_write image_convert
#' @importFrom curl curl_download
#' @importFrom httr http_error GET
#' @importFrom sf st_transform st_bbox st_as_sfc st_crs
#' @noRd 
.get_map <- function(ext, map_service, map_type, map_token, map_dir, map_res, ...){
  
  extras <- list(...)
  if(!is.null(extras$no_transform)) no_transform <- extras$no_transform else no_transform <- FALSE
  if(!is.null(extras$no_crop)) no_crop <- extras$no_crop else no_crop <- FALSE
  
  if(inherits(ext, "bbox")) ext <- list(ext)
  r <- lapply(ext, function(y){
    
    ## calculate needed slippy tiles using slippymath
    ext.ll <- st_bbox(st_transform(st_as_sfc(y), crs = st_crs(4326)))
    tg <- bbox_to_tile_grid(ext.ll, max_tiles = ceiling(map_res*20))
    tg$crs <- st_crs(y)
    tg$map_service <- map_service
    tg$map_type <- map_type
    tg$map_res <- map_res
    
    # manage cache
    cached <- getOption("basemaps.cached")
    cached.match <- if(length(cached) > 0) sapply(cached, function(x) identical(x$tg, tg)) else FALSE
    file_comp <- if(any(cached.match)){
      cached[[which(cached.match)]]$file_comp
    } else{
      paste0(map_dir, "basemap_", gsub(":", "", gsub(" ", "", gsub("-", "", Sys.time()))), ".tif")
    } 
    
    # load cached map or proceed
    if(file.exists(file_comp)){
      r <- brick(file_comp)
    } else{
      
      # get images
      images <- .apply(tg$tiles, MARGIN = 1, function(x){
        file <- paste0(map_dir, "/", map_service, "_", map_type, "_", x[1], "_", x[2], ".png")
        
        retry <- list(do = TRUE, count = 0)
        while(retry$do){
          
          # download tiles
          url <- paste0(getOption("basemaps.map_api")[[map_service]][[map_type]], tg$zoom, "/", 
                        if(map_service == "esri") paste0(x[2], "/", x[1]) else paste0(x[1], "/", x[2]), ".png", 
                        if(map_service == "mapbox") paste0("?access_token=", map_token) else NULL)
          if(isTRUE(http_error(url))){
            resp <- GET(url)
            status <- resp$status_code
            if(status == 401 & map_service == "mapbox") out("Authentification failed. Is your map_token correct?", type = 3)
          }
          if(!file.exists(file)){
            tryCatch(curl_download(url = url, destfile = file), error = function(e) out(paste0("Tile download failed: ", e$message), type = 3))
          }#utils::download.file(url = url, destfile = file, quiet = T) 
          
          # test if file can be loaded
          catch <- try(image_read(file), silent = T)
          if(inherits(catch, "try-error")){
            unlink(file)
            retry$count <- retry$count+1
            if(retry$count < 10) retry$do <- TRUE else out(paste0("Base map download failed: ", catch), type = 3)
          } else{
            retry$do <- FALSE
          }
        }
        
        # convert imagery
        image_write(image_convert(image_read(file), format = "PNG24"), file) # convert single channel png to multi channel png
        return(file)
      })
      
      # create composite
      r <- quiet(compose_tile_grid(tg, images))
      if(isFALSE(no_transform)){
        if(as.numeric(tg$crs$epsg) != 3857){
          r <- projectRaster(r, crs = crs(paste0("+init=epsg:", tg$crs$epsg)), method = "ngb")
        }
      }
      
      # crop composite
      if(isFALSE(no_crop)){
        r <- crop(r, extent(y[1], y[3], y[2], y[4]), snap = "out")
      }
      
      # decode terrain DEM
      if(all(map_service == "mapbox", map_type == "terrain")){
        r <-  -10000 + ((r[[1]] * 256 * 256 + r[[2]] * 256 + r[[3]]) * 0.1)
        #r_terr <- terrain(r, opt = c("slope", "aspect"))
        #r_hs <- hillShade(r_terr$slope, r_terr$aspect)
      }
      
      writeRaster(r, filename = file_comp, overwrite = T) # datatype = "INT1U",
      options(basemaps.cached = c(cached, list(list(tg = tg, file_comp = file_comp))))
    }
    return(r)
  })

  # assemble dateline crossing basemap
  if(length(r) > 1){
    
    # extend over dateline
    ext.both <- list(east = extent(r$east), west = extent(r$west))
    rg <- c("east"= diff(c(ext.both$east@xmin, ext.both$east@xmax)), "west" = diff(c(ext.both$west@xmin, ext.both$west@xmax)))
    
    ext.both <- .expand_ext(ext.both, rg)
    #ext.both <- .shift_ext(ext.both)
    extent(r$east) <- ext.both$east
    extent(r$west) <- ext.both$west
    
    # extend lower res raster, resample higher res raster and merge both
    ext.combi <- .combine_ext(ext.both)
    
    r[[which.min(rg)]] <- extend(r[[which.min(rg)]], ext.combi)
    r[[which.max(rg)]] <- resample(r[[which.max(rg)]], r[[which.min(rg)]])
    r <- list(merge(r[[1]], r[[2]]))
  }
  
  # rename layers
  r <- r[[1]]
  names(r) <- paste0("val", 1:nlayers(r))
  return(r)
  
  #projectRaster produces hidden warnings:
  # no non-missing arguments to max; returning -Inf
  # no non-missing arguments to min; returning -Inf
  # seems to be a bug
}


#' package startup
#' @importFrom pbapply pboptions
#' @noRd 
.onLoad <- function(libname, pkgname){
  pboptions(type = "timer", char = "=", txt.width = getOption("width")-30) # can be changed to "none"
  if(is.null(getOption("basemaps.verbose")))  options(basemaps.verbose = FALSE)
  if(is.null(getOption("basemaps.cached"))) options(basemaps.cached = list())
  if(is.null(getOption("basemaps.defaults")))  options(basemaps.defaults = list(map_service = "osm",
                                                                                map_type = "terrain",
                                                                                map_res = 1,
                                                                                map_token = NA))
  # overwrite temp dir
  defaults <- getOption("basemaps.defaults")
  defaults$map_dir <- paste0(tempdir(), "/basemaps/")
  options(basemaps.defaults = defaults)
  
  options(basemaps.map_api = list(osm = list(streets = "https://tile.openstreetmap.org/",
                                            streets_de = "http://a.tile.openstreetmap.de/tiles/osmde/",
                                            streets_fr = "https://a.tile.openstreetmap.fr/osmfr/",
                                            humanitarian = "http://a.tile.openstreetmap.fr/hot/",
                                            topographic = "https://a.tile.opentopomap.org/",
                                            #cycle = "https://a.tile.thunderforest.com/cycle/",
                                            #transport = "https://a.tile.thunderforest.com/transport/",
                                            #transport_dark = "https://a.tile.thunderforest.com/transport-dark/",
                                            #landscape = "https://a.tile.thunderforest.com/landscape/",
                                            #outdoors = "https://a.tile.thunderforest.com/outdoors/",
                                            roads = "https://maps.heigit.org/openmapsurfer/tiles/roads/webmercator/",
                                            hydda = "https://a.tile.openstreetmap.se/hydda/full/",
                                            hydda_base = "https://a.tile.openstreetmap.se/hydda/base/",
                                            hike = "http://toolserver.org/tiles/hikebike/",
                                            #hillshade = "http://c.tiles.wmflabs.org/hillshading/",
                                            grayscale = "https://tiles.wmflabs.org/bw-mapnik/",
                                            no_labels = "https://tiles.wmflabs.org/osm-no-labels/",
                                            watercolor = "http://c.tile.stamen.com/watercolor/",
                                            toner = "https://stamen-tiles-a.a.ssl.fastly.net/toner/",
                                            toner_bg = "https://stamen-tiles-a.a.ssl.fastly.net/toner-background/",
                                            toner_lite = "https://stamen-tiles-a.a.ssl.fastly.net/toner-lite/",
                                            terrain = "http://tile.stamen.com/terrain/",
                                            terrain_bg = "http://tile.stamen.com/terrain-background/",
                                            mtb = "http://tile.mtbmap.cz/mtbmap_tiles/"),
                                 carto = list(light = "https://a.basemaps.cartocdn.com/light_all/",
                                              light_no_labels = "https://a.basemaps.cartocdn.com/light_nolabels/",
                                              light_only_labels = "https://a.basemaps.cartocdn.com/light_only_labels/",
                                              dark = "https://a.basemaps.cartocdn.com/dark_all/",
                                              dark_no_labels = "https://a.basemaps.cartocdn.com/dark_nolabels/",
                                              dark_only_labels = "https://a.basemaps.cartocdn.com/dark_only_labels/",
                                              voyager = "https://a.basemaps.cartocdn.com/rastertiles/voyager/",
                                              voyager_no_labels = "https://a.basemaps.cartocdn.com/rastertiles/voyager_nolabels/",
                                              voyager_only_labels = "https://a.basemaps.cartocdn.com/rastertiles/voyager_only_labels/",
                                              voyager_labels_under = "https://a.basemaps.cartocdn.com/rastertiles/voyager_labels_under/"),
                                 mapbox = lapply(c(satellite = "mapbox.satellite",
                                                   terrain = "mapbox.terrain-rgb",
                                                   streets = "mapbox.streets",
                                                   streets_basic = "mapbox.streets-basic",
                                                   hybrid = "mapbox.streets-satellite",
                                                   light = "mapbox.light",
                                                   dark = "mapbox.dark",
                                                   high_contrast = "mapbox.high-contrast",
                                                   outdoors = "mapbox.outdoors",
                                                   hike = "mapbox.run-bike-hike",
                                                   wheatpaste = "mapbox.wheatpaste",
                                                   pencil = "mapbox.pencil",
                                                   comic = "mapbox.comic",
                                                   pirates = "mapbox.pirates",
                                                   emerald = "mapbox.emerald"), function(x) paste0("https://api.mapbox.com/v4/", x, "/")),
                                 esri = c(natgeo_world_map = "https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/",
                                          usa_topo_maps = "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer/tile/",
                                          world_imagery = "https://services.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer/tile/",
                                          world_physical_map = "https://services.arcgisonline.com/arcgis/rest/services/World_Physical_Map/MapServer/tile/",
                                          world_shaded_relief = "https://services.arcgisonline.com/arcgis/rest/services/World_Shaded_Relief/MapServer/tile/",
                                          world_street_map = "https://services.arcgisonline.com/arcgis/rest/services/World_Street_Map/MapServer/tile/",
                                          world_terrain_base = "https://services.arcgisonline.com/arcgis/rest/services/World_Terrain_Base/MapServer/tile/",
                                          world_topo_map = "https://services.arcgisonline.com/arcgis/rest/services/World_Topo_Map/MapServer/tile/",
                                          world_dark_gray_base = "https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Dark_Gray_Base/MapServer/tile/",
                                          world_dark_gray_reference = "https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Dark_Gray_Reference/MapServer/tile/",
                                          world_light_gray_base = "https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/",
                                          world_light_gray_reference = "https://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/",
                                          world_hillshade_dark = "https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade_Dark/MapServer/tile/",
                                          world_hillshade = "https://services.arcgisonline.com/arcgis/rest/services/Elevation/World_Hillshade/MapServer/tile/",
                                          world_ocean_base = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/",
                                          world_ocean_reference = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/",
                                          antarctic_imagery = "https://services.arcgisonline.com/arcgis/rest/services/Polar/Antarctic_Imagery/MapServer/tile/",
                                          arctic_imagery = "https://services.arcgisonline.com/arcgis/rest/services/Polar/Arctic_Imagery/MapServer/tile/",
                                          arctic_ocean_base = "https://services.arcgisonline.com/arcgis/rest/services/Polar/Arctic_Ocean_Base/MapServer/tile/",
                                          arctic_ocean_reference = "https://services.arcgisonline.com/arcgis/rest/services/Polar/Arctic_Ocean_Reference/MapServer/tile/",
                                          world_boundaries_and_places_alternate = "https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Boundaries_and_Places_Alternate/MapServer/tile/",
                                          world_boundaries_and_places = "https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/",
                                          world_reference_overlay = "https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Reference_Overlay/MapServer/tile/",
                                          world_transportation = "https://services.arcgisonline.com/arcgis/rest/services/Reference/World_Transportation/MapServer/tile/",
                                          delorme_world_base_map = "https://services.arcgisonline.com/arcgis/rest/services/Specialty/DeLorme_World_Base_Map/MapServer/tile/",
                                          world_navigation_charts = "https://services.arcgisonline.com/arcgis/rest/services/Specialty/World_Navigation_Charts/MapServer/tile/"))
  )
  if(!dir.exists(getOption("basemaps.defaults")$map_dir)) dir.create(getOption("basemaps.defaults")$map_dir)
}