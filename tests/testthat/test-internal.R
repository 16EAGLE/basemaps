skip_on_cran()
context("internal")

test_that("out", {
  expect_output(basemaps:::out("cat", verbose = T))
  expect_message(basemaps:::out("msg", msg = T, verbose = T))
  expect_warning(basemaps:::out("warning", type = 2, verbose = T))
  expect_error(basemaps:::out("error", type = 3, verbose = T))
})

test_that(".expand_ext & .combine_ext", {
  ext.both <- list(east = raster::extent(ext), west = raster::extent(ext))
  rg <- c("east"= diff(c(ext.both$east@xmin, ext.both$east@xmax)), "west" = diff(c(ext.both$west@xmin, ext.both$west@xmax)))
  ext.both <- expect_is(basemaps:::.expand_ext(ext.both, rg), "list")
  expect_is(basemaps:::.combine_ext(ext.both), "Extent")
})

test_that("onLoad", {
  expect_null(basemaps:::.onLoad())
})

test_that(".md_maptypes_table", {
  expect_output(basemaps:::.md_maptypes_table(get_maptypes()))
})

test_that("string manipulation", {
  x <- "/some/path/to/nowhere/filename.xyz"
  x <- expect_equal(.strip_filename(x), "/some/path/to/nowhere")
  x <- expect_equal(.add_trailing(x), "/some/path/to/nowhere/")
  x <- expect_equal(.add_trailing(x), "/some/path/to/nowhere/") #again without changes
  x <- expect_equal(.strip_trailing(x), "/some/path/to/nowhere")
  x <- expect_equal(.strip_trailing(x), "/some/path/to/nowhere") #again without changes
})