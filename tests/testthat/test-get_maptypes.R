skip_on_cran()
context("get_maptypes")

test_that("get_maptypes()", {
  expect_named(expect_is(expect_length(get_maptypes(), 4), "list"), c("osm", "carto", "mapbox", "esri"))
  expect_is(get_maptypes("osm"), "character")
})