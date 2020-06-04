skip_on_cran()
context("basemap")

test_that("basemap()", {
  # test nominal
  map <- expect_output(expect_is(basemap(ext, map_dir = map_dir, verbose = T), "RasterBrick"))
  expect_equal(dim(map), c(582, 623, 3))
  
  # test nominal terrain with col
  if(isTRUE(run_mapbox)){
    map <- expect_is(basemap(ext, map_service = "mapbox", map_type = "terrain", map_token = map_token, map_dir = map_dir, 
                           col = grDevices::topo.colors(25), verbose = F), "RasterLayer")
    # test nominal RGB calculatio for single layer magick
    map <- expect_is(basemap_magick(ext, map_service = "mapbox", map_type = "terrain", map_token = map_token, map_dir = map_dir, 
                             col = grDevices::topo.colors(25), verbose = F), "magick-image")
  }
  
  # test hiddena arguments
  expect_is(basemap(ext, no_transform = T, no_crop = T, verbose = F), "RasterBrick")
  
  # test warning with false map_dir
  expect_warning(basemap_plot(ext, map_dir = "/this/is/nonsense/", verbose = F))
  
  # test ext error
  expect_error(basemap())
  
})

test_that("basemap_raster()", {
  map <- expect_is(basemap_raster(ext, verbose = F), "RasterBrick")
  expect_equal(dim(map), c(582, 623, 3))
})

test_that("basemap_magick()", {
  expect_is(basemap_magick(ext, verbose = F), "magick-image")
})

test_that("basemap_png()", {
  file <- expect_is(basemap_png(ext, browse = F, verbose = F), "character")
  expect_true(file.exists(file))
  expect_equal(tail(strsplit(file, "[.]")[[1]], n=1), "png")
})

test_that("basemap_plot()", {
  expect_is(basemap_plot(ext, verbose = F), "NULL")
})

if(isTRUE(check_ggplot)){
  test_that("basemap_ggplot()", {
    expect_is(basemap_ggplot(ext, verbose = F), "gg")
    
    # test nominal with hidden arguments and aggregation
    expect_warning(expect_is(basemap_ggplot(ext, maxColorValue = 200, verbose = F), "gg"))
    
    # test nominal with hidden arguments and aggregation
    if(isTRUE(run_mapbox)) expect_is(basemap_ggplot(ext, map_type = "terrain", map_service = "mapbox", map_token = map_token, 
                                                    maxColorValue = NA, alpha = 1, maxpixels = 1000, verbose = F), "gg")
  })
  
  test_that("basemap_gglayer()", {
    expect_is(basemap_gglayer(ext, verbose = F), "LayerInstance")
    if(isTRUE(run_mapbox)) expect_is(basemap_gglayer(ext, map_type = "terrain", map_service = "mapbox", map_token = map_token, verbose = F), "LayerInstance")
  })
}

if(isTRUE(check_stars)){
  test_that("basemap_stars()", {
    expect_is(basemap_stars(ext, verbose = F), "stars")
  })
}

if(isTRUE(check_stars)){
  test_that("basemap_stars()", {
    map <- expect_is(basemap_stars(ext, verbose = F), "stars")
    expect_equal(dim(map), c(x=623, y=582, band=3))
  })
}

if(isTRUE(check_mapview)){
  test_that("basemap_mapview()", {
    expect_is(basemap_mapview(ext, verbose = F), "mapview")
  })
}

if(isTRUE(test_maps)){
  test_services <- names(get_maptypes())
  if(isFALSE(run_mapbox)) test_services <- test_services[test_services != "mapbox"]
  if(isFALSE(run_esri)) test_services <- test_services[test_services != "esri"]
  
  test_that("basemap (test map types)", {
    
    catch <- lapply(test_services, function(service) lapply(get_maptypes(service), function(x, s = service){
      #tryCatch({
      cat(paste0(" ", s, ": ", x, "\n"))
      map <- basemap(ext, map_service = s, map_type = x, map_token = Sys.getenv("moveVis_map_token"), verbose = F)
      expect_is(map, "RasterBrick")
      return(NULL)
    }))
  })
}

