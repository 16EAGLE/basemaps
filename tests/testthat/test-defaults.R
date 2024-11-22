skip_on_cran()
context("defaults")

test_that("set_defaults()", {
  expect_is(set_defaults(ext = ext, map_service = "carto", map_type = "dark",
                         map_token = NA, map_dir = paste0(tempdir(), "/basemaps/"), map_res = 0.5), "list")
})

test_that("get_defaults()", {
  defaults_expected <- list(map_service = "carto",
                            map_type = "dark",
                            map_res = 0.5,
                            map_token = NA,
                            map_dir = paste0(tempdir(), "/basemaps/"),
                            ext = ext)
  defaults <- expect_is(get_defaults(), "list")
  expect_true(all(mapply(x = defaults_expected, y = defaults, function(x, y) all(identical(x, y)))))
})

test_that("reset_defaults()", {
  expect_is(reset_defaults(), "list")
})

test_that("get_maptypes()", {
  services <- c("osm", "osm_stamen", "osm_stadia", "osm_thunderforest", "carto", "mapbox", "esri", "maptiler")
  expect_named(expect_is(expect_length(get_maptypes(), 8), "list"), services)
  expect_is(get_maptypes("osm"), "character")
  
  df <- get_maptypes(as_df = T)
  expect_equal(ncol(df), 2)
  expect_named(expect_is(df, "data.frame"), c("map_service", "map_type"))
  
  df <- get_maptypes(as_df = T, url_cols = T)
  expect_equal(ncol(df), 8)
  expect_is(df, "data.frame")
  
  expect_equal(unique(df$map_service), services)
})

test_that("add_maptypes()", {
  expect_output(add_maptypes(
    map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", 
    url_xy = "xy",
    url_file_format = ".png", 
    url_map_token = "?authtoken=",
    auth_error_code = 401, 
    url_website = "https://someservice.org"
  ))
  expect_error(add_maptypes(map_service = 123, map_type = "terrain", url_endpoint = "https://tile.someservice.org"))
  expect_error(add_maptypes(map_service = "someservice", map_type = 123, url_endpoint = "https://tile.someservice.org"))
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = 123))
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", url_xy = 123))
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", url_file_format = 123))
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", url_map_token = 123))
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", auth_error_code = "123"))
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", url_website = 123))
  
  expect_error(add_maptypes(map_service = "someservice", map_type = "terrain", url_endpoint = "https://tile.someservice.org", url_xy = "z"))
  expect_error(add_maptypes(map_service = c("someservice1", "someservice2"), map_type = "terrain", url_endpoint = "https://tile.someservice.org"))
  expect_error(add_maptypes(map_service = "someservice1", map_type = "terrain", url_endpoint = "https://tile.someservice.org", url_xy = c("xy", "xy")))
  
  expect_error(add_maptypes(map_service = "someservice1", map_type = "terrain", url_endpoint = "https://tile.someservice.org", 
                            url_xy = c("xy", "xy"), url_map_token = c("?authtoken=", "?authtoken="), url_file_format = c(".png", ".png"), 
                            auth_error_code = c(401, 401), url_website = c("xyz", "xyz")))
  
})

file <- tempfile(fileext = ".csv")
test_that("save_maptypes()", {
  expect_output(save_maptypes(file))
  expect_true(file.exists(file))
})

test_that("load_maptypes()", {
  expect_output(load_maptypes(file))
  expect_equal(ncol(get_maptypes(as_df = T, url_cols = T)), 8)
})