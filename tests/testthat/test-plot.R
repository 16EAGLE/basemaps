skip_on_cran()
context("plot")

test_that("gg_raster()", {
  map <- basemap_raster(ext, map_dir = map_dir, verbose = F)
  expect_is(gg_raster(map, r_type = "RGB"), "gg")
  expect_is(gg_raster(map, r_type = "RGB", maxpixels = 500000, alpha = 0.5, interpolate = FALSE, add_coord = F), "gg")
  
  expect_is(gg_raster(map, r_type = "RGB", gglayer = F), "gg")
  
  map <- basemap_stars(ext, map_dir = map_dir, verbose = F)
  expect_error(gg_raster(map, r_type = "RGB", gglayer = F))
})