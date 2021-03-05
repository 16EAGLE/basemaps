skip_on_cran()
context("get_maptypes")

test_that("get_maptypes()", {
  expect_named(expect_is(expect_length(get_maptypes(), 6), "list"), c("osm", "osm_stamen", "osm_thunderforest", 
                                                                      "carto", "mapbox", "esri"))
  expect_is(get_maptypes("osm"), "character")
})