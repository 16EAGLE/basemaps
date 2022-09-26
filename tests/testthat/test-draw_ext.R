skip_on_cran()
context("draw_ext")

test_that("draw_ext()", {
  if(isTRUE(test$mapedit)) expect_true(TRUE) else expect_error(draw_ext())
})

