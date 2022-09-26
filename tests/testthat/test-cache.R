skip_on_cran()
context("cache")

test_that("flush_cache()", {
  expect_output(expect_is(basemap(ext, map_dir = map_dir, verbose = T), "NULL"))
  expect_output(flush_cache())
})