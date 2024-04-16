skip_on_cran()
context("basemap")

test_that("basemap()", {
  # test debug
  expect_message(basemap(ext, debug_client = T, verbose = T, class = "png"))
  
  # test nominal
  map <- expect_output(expect_is(basemap(ext, map_dir = map_dir, verbose = T, class = "terra"), "SpatRaster"))
  expect_equal(dim(map), c(869, 986, 3))
  
  # test nominal terrain with col
  if(isTRUE(run_mapbox)){
    map <- expect_is(basemap(ext, map_service = "mapbox", map_type = "terrain", map_token = mapbox_token, map_dir = map_dir, 
                             col = grDevices::topo.colors(25), verbose = F, class = "terra"), "SpatRaster")
    # test nominal RGB calculatio for single layer magick
    map <- expect_is(basemap_magick(ext, map_service = "mapbox", map_type = "terrain", map_token = mapbox_token, map_dir = map_dir, 
                             col = grDevices::topo.colors(25), verbose = F), "magick-image")
  }
  
  # test hiddena arguments
  expect_is(basemap(ext, no_transform = T, no_crop = T, verbose = F, class = "terra"), "SpatRaster")
  expect_is(basemap(ext, no_transform = T, no_crop = T, verbose = F, class = "terra", col = topo.colors(22)), "SpatRaster")
  expect_is(basemap(ext, no_transform = T, no_crop = T, verbose = F, class = "png", dpi = 100, browse = F), "character")
  
  # test warning with false map_dir
  expect_warning(basemap_plot(ext, map_dir = "/this/is/nonsense/", verbose = F))
  
  # test ext error
  expect_error(basemap())
  
  # test no map_token error mapbox
  expect_error(basemap(ext, map_service = "mapbox", map_type = "streets", verbose = F))
  expect_error(basemap(ext, map_service = "osm_thunderforest", map_type = "cycle", verbose = F))
  expect_error(basemap(ext, map_service = "osm_stamen", map_type = "toner", verbose = F))
  expect_error(basemap(ext, map_service = "osm_stadia", map_type = "alidade_smooth", verbose = F))
  
  # test false map_token error mapbox
  expect_error(basemap(ext, map_service = "mapbox", map_type = "streets", map_token = "this_is_nonsense", verbose = F))
  
  # test ext warning on different CRS
  expect_warning(basemap(ext_eur, map_dir = map_dir, verbose = T))
  
  # test multiple extents (dateline crossing)
  expect_is(basemap_raster(list(ext), map_dir = map_dir, verbose = F), "RasterBrick")
  expect_warning(expect_is(basemap_raster(
    ext = ext_sc, map_service = "carto", map_type = "light", map_dir = map_dir,
    verbose = F, custom_crs = custom_crs), "RasterBrick"
  ))
  
  # test custom zoom
  expect_is(
    basemap(ext = ext, map_dir = map_dir, verbose = F, custom_zoom = 3, class = "terra"),
    "SpatRaster"
  )
  
  expect_is(
    basemap(ext = ext, map_dir = map_dir, verbose = F, custom_zoom = 2, class = "terra"),
    "SpatRaster"
  )
})

test_that("basemap_plot()", {
  expect_is(basemap_plot(ext, verbose = F), "NULL")
  if(isTRUE(run_mapbox)) expect_is(basemap_plot(ext, map_service = "mapbox", map_type = "terrain", map_token = mapbox_token, verbose = F), "NULL")
})

test_that("basemap_magick()", {
  expect_is(basemap_magick(ext, verbose = F), "magick-image")
})

test_that("basemap_png()", {
  file <- expect_is(basemap_png(ext, browse = F, verbose = F), "character")
  expect_true(file.exists(file))
  expect_equal(tail(strsplit(file, "[.]")[[1]], n=1), "png")
})

test_that("basemap_geotif()", {
  file <- expect_is(basemap_geotif(ext, verbose = F), "character")
  expect_true(file.exists(file))
  expect_equal(tail(strsplit(file, "[.]")[[1]], n=1), "tif")
  expect_true(!is.na(terra::crs(terra::rast(file))))
})

test_that("basemap_terra()", {
  map <- expect_is(basemap_terra(ext, verbose = F), "SpatRaster")
  expect_equal(dim(map), c(869, 986, 3))
})

if(isTRUE(test$raster)){
  test_that("basemap_raster()", {
    map <- expect_is(basemap_raster(ext, verbose = F), "RasterBrick")
    expect_equal(dim(map), c(869, 986, 3))
  })
}

if(isTRUE(test$stars)){
  test_that("basemap_stars()", {
    map <- expect_is(basemap_stars(ext, verbose = F), "stars")
    expect_equal(dim(map), c(x=986, y=869, band=3))
  })
}

if(isTRUE(test$ggplot)){
  test_that("basemap_ggplot()", {
    expect_is(basemap_ggplot(ext, verbose = F), "gg")
    
    # test nominal with hidden arguments and aggregation
    expect_warning(expect_is(basemap_ggplot(ext, maxColorValue = 200, verbose = F), "gg"))
    
    # test nominal with hidden arguments and aggregation
    if(isTRUE(run_mapbox)) expect_is(basemap_ggplot(ext, map_type = "terrain", map_service = "mapbox", map_token = mapbox_token, 
                                                    maxColorValue = NA, alpha = 1, maxpixels = 1000, verbose = F), "gg")
  })
  
  test_that("basemap_gglayer()", {
    expect_is(basemap_gglayer(ext, verbose = F), "LayerInstance")
    if(isTRUE(run_mapbox)) expect_is(basemap_gglayer(ext, map_type = "terrain", map_service = "mapbox", map_token = mapbox_token, verbose = F), "LayerInstance")
  })
}

if(isTRUE(test$mapview)){
  test_that("basemap_mapview()", {
    expect_is(basemap_mapview(ext, verbose = F), "mapview")
  })
}

if(isTRUE(test$maps)){
  test_services <- names(get_maptypes())
  if(isFALSE(run_mapbox)) test_services <- test_services[test_services != "mapbox"]
  if(isFALSE(run_osmtf)) test_services <- test_services[test_services != "osm_thunderforest"]
  if(isFALSE(run_stamen)) test_services <- test_services[test_services != "osm_stamen"]
  if(isFALSE(run_stadia)) test_services <- test_services[test_services != "osm_stadia"]
  if(isFALSE(run_esri)) test_services <- test_services[test_services != "esri"]
  
  # s <- service <- test_services[1]
  # x <- get_maptypes(service)[3]
  # 
  for(s in test_services){
    for(x in get_maptypes(s)){
      map_token <- if(s == "mapbox"){
        mapbox_token
      } else if(s == "osm_thunderforest"){
        osmtf_token
      } else if(s == "osm_stamen"){
        osmstamen_token
      } else if(s == "osm_stadia"){
        osmstadia_token
      } else {
        NULL
      }
      
      test_that(paste0("basemap (", s, ": ", x, ")"), {
        expect_is(basemap_png(ext, map_service = s, map_type = x, map_token = map_token, verbose = F, browse = F), "character")
      })
    }
  }
}
