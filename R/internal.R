#' Suppress messages and warnings
#' @keywords internal
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
#' @keywords internal
#' @noRd
.apply <- function(X, MARGIN, FUN, ...){
  verbose = getOption("basemaps.verbose")
  if(isTRUE(verbose)) pbapply(X, MARGIN, FUN, ...) else apply(X, MARGIN, FUN, ...)
}

#' strip filename from path
#'
#' @keywords internal
#' @importFrom utils head
#' @noRd
.strip_filename <- function(x){
  paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/")
}

#' strip a trialing char from a string if it is there
#'
#' @keywords internal
#' @noRd
.strip_trailing <- function(x, char = "/"){
  if(substr(x, nchar(x), nchar(x)) == char) substr(x, 1, nchar(x)-1) else x
}

#' add a trialing char to a string if it is not there
#'
#' @keywords internal
#' @noRd
.add_trailing <- function(x, char = "/"){
  if(substr(x, nchar(x), nchar(x)) == char) x else paste0(x, char)
}


#' combine two extents into one
#' @keywords internal
#' @noRd
.combine_ext <- function(ext.both){
  ext.combi <- ext.both[[1]]
  ext.combi@xmin <- min(c(ext.both[[1]]@xmin, ext.both[[2]]@xmin))
  ext.combi@xmax <- max(c(ext.both[[1]]@xmax, ext.both[[2]]@xmax))
  return(ext.combi)
}

#' expand two extents by the one with larger x range
#' @keywords internal
#' @noRd
.expand_ext <- function(ext.both, rg){
  
  ## STARS
  # if(ext.both[[which.max(rg)]]@xmin < 0){
  #   ext.both[[which.max(rg)]]@xmin <- ext.both[[which.max(rg)]]@xmin - rg[which.min(rg)]
  #   ext.both[[which.min(rg)]]@xmax <- ext.both[[which.min(rg)]]@xmax + rg[which.max(rg)]
  # }else{
  #   ext.both[[which.max(rg)]]@xmax <- ext.both[[which.max(rg)]]@xmax + rg[which.min(rg)]
  #   ext.both[[which.min(rg)]]@xnub <- ext.both[[which.min(rg)]]@xmin - rg[which.max(rg)]
  # }
  rg_mm <- .which_rg(rg)
  if(ext.both[[rg_mm$max]][1] < 0){
    ext.both[[rg_mm$max]][1] <- ext.both[[rg_mm$max]][1] - rg[rg_mm$min]
    ext.both[[rg_mm$min]][2] <- ext.both[[rg_mm$min]][2] + rg[rg_mm$max]
  }else{
    ext.both[[rg_mm$max]][2] <- ext.both[[rg_mm$max]][2] + rg[rg_mm$min]
    ext.both[[rg_mm$min]][1] <- ext.both[[rg_mm$min]][1] - rg[rg_mm$max]
  }
  
  return(ext.both)
}

#' handles case in which rg is equal
#' @keywords internal
#' @noRd
.which_rg <- function(rg){
  rg_mm <- list()
  if(rg[1] == rg[2]){
    rg_mm$min <- 1
    rg_mm$max <- 2
  } else{
    rg_mm$min <- which.min(rg)
    rg_mm$max <- which.max(rg)
  }
  return(rg_mm)
}

#' get map
#' @importFrom slippymath bbox_to_tile_grid tile_bbox
#' @importFrom magick image_read image_write image_convert
#' @importFrom curl curl_download
#' @importFrom httr http_error GET
#' @importFrom sf st_transform st_bbox st_as_sfc st_crs st_crs<- st_crop
#' @importFrom terra rast ext ext<- crs crs<- mosaic project crop writeRaster extend merge
#' @keywords internal
#' @noRd
.get_map <- function(ext, map_service, map_type, map_token, map_dir, map_res, force, class, ...){
  
  extras <- list(...)
  if(!is.null(extras$no_transform)) no_transform <- extras$no_transform else no_transform <- FALSE
  if(!is.null(extras$no_crop)) no_crop <- extras$no_crop else no_crop <- FALSE
  if(!is.null(extras$custom_crs)) custom_crs <- extras$custom_crs else custom_crs <- NA
  
  # Allow the user to specify a custom tile zoom level, to be provided to bbox_to_tile_grid()
  if(!is.null(extras$custom_zoom)) custom_zoom <- extras$custom_zoom else custom_zoom <- NULL

  if(inherits(ext, "bbox")) ext <- list(ext)
  file_comp <- lapply(ext, function(y){
    
    ## calculate needed slippy tiles using slippymath
    ext.ll <- st_bbox(st_transform(st_as_sfc(y), crs = st_crs(4326)))
    if(!is.null(custom_zoom)){
      tg <- bbox_to_tile_grid(ext.ll, zoom=custom_zoom)
    } else{
      tg <- bbox_to_tile_grid(ext.ll, max_tiles = ceiling(map_res*20))
    }
    tg$crs <- st_crs(y)
    tg$map_service <- map_service
    tg$map_type <- map_type
    tg$map_res <- map_res
    
    # manage cache
    cached <- getOption("basemaps.cached")
    # are cached items in current map_dir? If not, disregard them
    if(length(cached) > 0) cached <- cached[sapply(cached, function(x) .add_trailing(.strip_filename(x$file_comp))) == map_dir]
    
    cached.match <- if(length(cached) > 0) sapply(cached, function(x) identical(x$tg, tg)) else FALSE
    file_comp <- if(any(cached.match)){
      cached[[which(cached.match)]]$file_comp
    } else{
      file.path(map_dir, paste0("basemap_", gsub(":", "", gsub(" ", "", gsub("-", "", Sys.time()))), ".tif"))
    } 
    
    # load cached map or proceed
    if(any(!file.exists(file_comp), isTRUE(force))){
      
      # get images
      images <- .apply(tg$tiles, MARGIN = 1, function(x){
        file <- paste0(map_dir, "/", map_service, "_", map_type, "_", x[1], "_", x[2], ".png")
        
        retry <- list(do = TRUE, count = 0)
        while(retry$do){
          
          # download tiles
          url <- paste0(
            getOption("basemaps.map_api")[[map_service]][[map_type]], tg$zoom, "/", # base URL
            if(map_service == "esri") paste0(x[2], "/", x[1]) else paste0(x[1], "/", x[2]), # coordinate order
            if(any(map_service != "mapbox", all(map_service == "mapbox", map_type == "terrain"))) ".png", # file suffix or not
            if(map_service == "mapbox") paste0("?access_token=", map_token), # token or not
            if(map_service == "osm_thunderforest") paste0("?apikey=", map_token) # token or not
          )
          out(sprintf("URL: %s", url))
          
          if(isTRUE(http_error(url))){
            resp <- GET(url)
            status <- resp$status_code
            if(status == 401 & map_service == "mapbox") out("Authentification failed. Is your map_token correct?", type = 3)
            if(status == 403 & map_service == "osm_thunderforest") out("Authentification failed. Is your map_token correct?", type = 3)
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
        img_down <- image_convert(image_read(file), format = "PNG24")
        image_write(img_down, file) # convert single channel png to multi channel png
        return(file)
      })
      
      # create composite
      
      ## STARS VERSION
      # r <- mapply(img = images, x = tg$tiles$x, y = tg$tiles$y, function(img, x, y){
      #   box <- tile_bbox(x, y, tg$zoom)
      #   img_st <- read_stars(img)
      #   img_st <- st_set_bbox(img_st, box)
      #   st_crs(img_st) <- tg$crs
      #   return(img_st)
      # }, SIMPLIFY = F)
      
      ## TERRA VERSION
      r <- mapply(img = images, x = tg$tiles$x, y = tg$tiles$y, function(img, x, y){
        box <- tile_bbox(x, y, tg$zoom)
        img_rst <- quiet(terra::rast(img))
        terra::ext(img_rst) <- terra::ext(box[c("xmin", "xmax", "ymin", "ymax")])
        terra::crs(img_rst) <- as.character(tg$crs$wkt)
        return(img_rst)
      }, SIMPLIFY = F, USE.NAMES = F)
      
      r <- do.call(terra::mosaic, r)
      
      if(isFALSE(no_transform)){ ## needed?
        if(as.numeric(tg$crs$epsg) != 3857){
          #r <- st_transform(r, crs = tg$crs)
          r <- terra::project(r, y = as.character(tg$crs$wkt), method = "bilinear")
        }
      }
      
      # crop composite
      if(isFALSE(no_crop)){
        #r <- st_crop(r, y)
        r <- terra::crop(r, terra::ext(y[c("xmin", "xmax", "ymin", "ymax")]))
      }
      
      # decode terrain DEM
      if(all(map_service == "mapbox", map_type == "terrain")){
        #r <-  -10000 + ((r[,,,1] * 256 * 256 + r[,,,2] * 256 + r[,,,3]) * 0.1)
        
        ## STARS
        # r1 <- r[,,,1] * 256 * 256
        # r2 <- r[,,,2] * 256
        # r3 <- r[,,,3]
        # st_dimensions(r3) <- st_dimensions(r2) <- st_dimensions(r1)
        # 
        # r <-  -10000 + ((r1 + r2 + r3) * 0.1)
        
        ## TERRA
        r <-  -10000 + (((r[[1]] * 256 * 256) + (r[[2]] * 256) + r[[3]]) * 0.1)
      }
      
      # empty iamgery (inf case)
      if(identical(quiet(range(r, na.rm=T)), c(Inf, -Inf))){
        r[[1]][] <- 0
      }
      
      #unlink(file_comp)
      #write_stars(r, dsn = file_comp)
      terra::writeRaster(r, filename = file_comp, overwrite = TRUE, filetype = "GTiff")
      #gdal=c("COMPRESS=NONE", "TFW=YES")
      options(basemaps.cached = c(cached, list(list(tg = tg, file_comp = file_comp))))
    } 
    return(file_comp)
  })
  
  #assemble dateline crossing basemap
  if(length(file_comp) > 1){

    # load and name 
    r <- lapply(unname(file_comp), terra::rast)
    
    # get original extents of untouched rasters
    ext.both <- lapply(r, terra::ext)
    
    # measure x diff, which side should be preserved, whcih side should be extended to the other?
    rg <- sapply(ext.both, function(x) diff(c(x[1], x[2])))
    rg_mm <- .which_rg(rg) 
    
    # save the x end coodinate of this grid
    cc.xmin <- ext.both[[rg_mm$max]][1] #xmin
    
    # expand both extents
    ext.both.exp <- .expand_ext(ext.both, rg)
    
    # choose an extent
    ext.combi <- ext.both.exp[[rg_mm$max]]
    
    # extend the larger one
    r[[rg_mm$max]] <- terra::extend(r[[rg_mm$max]], ext.combi)
    
    # shift the smaller one over to the other side
    ext.min <- terra::ext(r[[rg_mm$min]])
    ext.min <- terra::ext(as.numeric(cc.xmin - min(rg)), cc.xmin, ext.min[3], ext.min[4])
    
    terra::ext(r[[rg_mm$min]]) <- ext.min
    
    # extent the smaller one too, resample to larger one
    r[[rg_mm$min]] <- terra::extend(r[[rg_mm$min]], ext.combi)
    r[[rg_mm$min]] <- terra::resample(r[[rg_mm$min]], r[[rg_mm$max]])
    
    # fuse rasters over grid end
    r <- terra::merge(r[[1]], r[[2]])
    
    # if another CRS is equested, we need to do some tricks, since we cannot reproject the "shifted" raster
    if(!is.na(custom_crs)){
      
      # shift extent onto one side of the coordinate line
      ext.repro <- terra::ext(r)
      if(cc.xmin < 0){
        ext.repro[1] <- ext.repro[1] + rg[rg_mm$min]
        ext.repro[2] <- ext.repro[2] + rg[rg_mm$min]
      } else{
        ext.repro[1] <- ext.repro[1] - rg[rg_mm$min]
        ext.repro[2] <- ext.repro[2] - rg[rg_mm$min]
      }
      # project shifted raster
      terra::ext(r) <- ext.repro
      r <- terra::project(r, y = custom_crs, method = "bilinear")
      
      # now project the original extents of the two rasters
      r_as_was <- lapply(unname(file_comp), terra::rast)
      ext.before <- lapply(r_as_was, function(x) terra::ext(terra::project(x, y = custom_crs,  method = "bilinear")))
      
      # combine these two as before
      rg <- sapply(ext.before, function(x) diff(c(x[1], x[2])))
      rg_mm <- .which_rg(rg) 
      ext.before.exp <- .expand_ext(ext.before, rg)
      
      # assign equivialnt extent
      terra::ext(r) <- ext.before.exp[[rg_mm$max]]
    }
    
    file_comp <- paste0(map_dir, "basemap_", gsub(":", "", gsub(" ", "", gsub("-", "", Sys.time()))), ".tif")
    #write_stars(st_as_stars(r), file_comp)
    terra::writeRaster(r, filename = file_comp, overwrite = TRUE, filetype = "GTiff")
  } else{
    
    # custom crs?
    if(!is.na(custom_crs)){
      ## RASTER
      # r <- brick(file_comp[[1]])
      # r <- projectRaster(r, crs = custom_crs)
      
      ## TERRA
      r <- terra::rast(file_comp[[1]])
      r <- terra::project(r, y = custom_crs, method = "bilinear")
      
      file_comp <- paste0(map_dir, "basemap_", gsub(":", "", gsub(" ", "", gsub("-", "", Sys.time()))), ".tif")
      #write_stars(st_as_stars(r), file_comp)
      terra::writeRaster(r, filename = file_comp, overwrite = TRUE, filetype = "GTiff")
    } else{
      
      file_comp <- file_comp[[1]]
    }
  }
  
  # rename layers
  #r <- r[[1]]
  #names(r) <- "val"
  #names(r) <- paste0("val", 1:nlayers(r))
  #if("raster" %in% class) brick(file_comp) else return(r)
  return(file_comp)
}

#' defaults
#' @keywords internal
#' @noRd
.defaults <- function(){
  list(map_service = "osm_stamen",
       map_type = "terrain",
       map_res = 1,
       map_token = NA)
}

#' create a markdown table of map types
#' @keywords internal
#' @noRd
.md_maptypes_table <- function(maptypes){
  x <- paste0(lapply(names(maptypes), function(service){
    paste0(unlist(lapply(maptypes[[service]], function(x, s = service){
      paste0("| `", s, "` | `", x, "` | ", if(any(grepl("mapbox", s), grepl("osm_thunderforest", s))) "yes" else "no", " |")
    })), collapse = "\n")
  }), collapse = "\n")
  cat(paste0("| `map_service` | `map_type` | `map_token` required? |\n | ------ |  ------ | ------ |\n", x))
}

#' package startup
#' @importFrom pbapply pboptions
#' @noRd 
.onLoad <- function(libname, pkgname){
  
  # avoid messages from unfound finalizer methods
  # see https://github.com/rspatial/terra/issues/30 but also elsewhere found
  # remove when issue seems resolved
  catch <- suppressMessages(trace(reg.finalizer, quote({
    
    classDef <- dynGet("classDef", ifnotfound = NULL)
    if (!is.null(classDef)) {
      f <- function(x) {
        method <- selectMethod("$", list(x = class(x$.self)))
        finalize <- method(x$.self, "finalize")
        finalize()
      }
    }
    
  }), print = FALSE))
  
  pboptions(type = "timer", char = "=", txt.width = getOption("width")-30) # can be changed to "none"
  if(is.null(getOption("basemaps.verbose")))  options(basemaps.verbose = FALSE)
  if(is.null(getOption("basemaps.cached"))) options(basemaps.cached = list())
  if(is.null(getOption("basemaps.defaults")))  options(basemaps.defaults = .defaults())
  # overwrite temp dir
  defaults <- getOption("basemaps.defaults")
  defaults$map_dir <- paste0(tempdir(), "/basemaps/")
  options(basemaps.defaults = defaults)
  
  options(basemaps.map_api = list(
    osm = list(
      streets = "https://tile.openstreetmap.org/",
      streets_de = "http://tile.openstreetmap.de/tiles/osmde/",
      #streets_fr = "https://tile.openstreetmap.fr/osmfr/", #down?
      #humanitarian = "http://tile.openstreetmap.fr/hot/", #down?
      topographic = "https://tile.opentopomap.org/"
      #hike = "https://tiles.wmflabs.org/hikebike/", #unmaintained
      #hillshade = "https://tiles.wmflabs.org/hillshading/", #unmaintained
      #grayscale = "https://tiles.wmflabs.org/bw-mapnik/", #unmaintained
      #no_labels = "https://tiles.wmflabs.org/osm-no-labels/", #unmaintained
      #mtb = "https://tile.mtbmap.cz/mtbmap_tiles/"),
      #hydda = "https://a.tile.openstreetmap.se/hydda/full/", # unresponsive
      #hydda_base = "https://a.tile.openstreetmap.se/hydda/base/",), # unresponsive
    ),
    osm_stamen = list(
      toner = "https://stamen-tiles-a.a.ssl.fastly.net/toner/",
      toner_bg = "https://stamen-tiles-a.a.ssl.fastly.net/toner-background/",
      toner_lite = "https://stamen-tiles-a.a.ssl.fastly.net/toner-lite/",
      terrain = "http://tile.stamen.com/terrain/",
      terrain_bg = "http://tile.stamen.com/terrain-background/",
      watercolor = "http://tile.stamen.com/watercolor/"
    ),
    osm_thunderforest = list(
      cycle = "https://tile.thunderforest.com/cycle/",
      transport = "https://tile.thunderforest.com/transport/",
      landscape = "https://tile.thunderforest.com/landscape/",
      outdoors = "https://tile.thunderforest.com/outdoors/",
      transport_dark = "https://tile.thunderforest.com/transport-dark/",
      spinal = "https://tile.thunderforest.com/spinal-map/",
      pioneer = "https://tile.thunderforest.com/pioneer/",
      mobile_atlas = "https://tile.thunderforest.com/mobile-atlas/",
      neighbourhood = "https://tile.thunderforest.com/neighbourhood/",
      atlas = "https://tile.thunderforest.com/atlas/"
    ),
    carto = list(
      light = "https://basemaps.cartocdn.com/light_all/",
      light_no_labels = "https://basemaps.cartocdn.com/light_nolabels/",
      light_only_labels = "https://basemaps.cartocdn.com/light_only_labels/",
      dark = "https://basemaps.cartocdn.com/dark_all/",
      dark_no_labels = "https://basemaps.cartocdn.com/dark_nolabels/",
      dark_only_labels = "https://basemaps.cartocdn.com/dark_only_labels/",
      voyager = "https://basemaps.cartocdn.com/rastertiles/voyager/",
      voyager_no_labels = "https://basemaps.cartocdn.com/rastertiles/voyager_nolabels/",
      voyager_only_labels = "https://basemaps.cartocdn.com/rastertiles/voyager_only_labels/",
      voyager_labels_under = "https://basemaps.cartocdn.com/rastertiles/voyager_labels_under/"
    ),
    mapbox = c(lapply(c(
      streets = "streets-v11",
      outdoors = "outdoors-v11",
      light = "light-v10",
      dark = "dark-v10",
      satellite = "satellite-v9",
      hybrid = "satellite-streets-v9"),
      function(x) paste0("https://api.mapbox.com/styles/v1/mapbox/", x, "/tiles/")),
      terrain = "https://api.mapbox.com/v4/mapbox.terrain-rgb/"
    ),
    esri = c(
      natgeo_world_map = "https://services.arcgisonline.com/arcgis/rest/services/NatGeo_World_Map/MapServer/tile/",
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
      world_navigation_charts = "https://services.arcgisonline.com/arcgis/rest/services/Specialty/World_Navigation_Charts/MapServer/tile/"
    ),
    usgs = list(
      hydro_cached = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer/tile/",
      imagery_only = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/",
      imagery_topo = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryTopo/MapServer/tile/",
      shaded_relief = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSShadedReliefOnly/MapServer/tile/",
      national_map = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTNMBlank/MapServer/tile/",
      topo = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/"
    )
  ))
  if(!dir.exists(getOption("basemaps.defaults")$map_dir)) dir.create(getOption("basemaps.defaults")$map_dir)
  
}

#' package detach
#' @noRd 
.onUnload <- function(libpath){
  catch <- untrace(reg.finalizer)
}
