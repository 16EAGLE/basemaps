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