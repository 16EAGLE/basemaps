skip_on_cran()
context("get_maptypes")

test_that("get_maptypes()", {
  services <- c("osm", "osm_stamen", "osm_stadia", "osm_thunderforest", "carto", "mapbox", "esri", "maptiler")
  expect_named(expect_is(expect_length(get_maptypes(), 8), "list"), services)
  expect_is(get_maptypes("osm"), "character")
  
  df <- get_maptypes(as_df = T)
  expect_equal(ncol(df), 2)
  expect_named(expect_is(df, "data.frame"), c("map_service", "map_type"))
  
  df <- get_maptypes(as_df = T, url_cols = T)
  expect_equal(ncol(df), 7)
  expect_is(df, "data.frame")
  
  expect_equal(unique(df$map_service), services)
})