skip_on_cran()
context("cache")

test_that("flush_cache()", {
  map <- expect_output(expect_is(basemap(ext, map_dir = map_dir, verbose = T), "SpatRaster"))
  expect_output(flush_cache())
})