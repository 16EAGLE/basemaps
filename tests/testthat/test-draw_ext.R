skip_on_cran()
context("defaults")

test_that("draw_ext()", {
  if(!any(grepl("mapedit", rownames(installed.packages())))) expect_error(draw_ext()) else expect_true(TRUE)
})
