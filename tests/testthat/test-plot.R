skip_on_cran()
context("plot")

test_that("gg_raster()", {
  map <- basemap_raster(ext, map_dir = map_dir, verbose = F)
  expect_is(gg_raster(map, r_type = "RGB"), "gg")
  expect_is(gg_raster(map, r_type = "RGB", gglayer = F), "gg")
})