context("position_fillnudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_fillnudge(kept.origin = "", vjust = 0.5, x = -0.3),
    "`kept.origin` must be one of \"stacked\", \"original\", or \"none\", not \"\"."
  )
})

test_that("correct kept.origin used", {
  position <- position_fillnudge(kept.origin = "none", vjust = 0.5, x = -0.3)
  expect_no_error(position)
})

test_that("correct reverse, vjust and x assigned", {
  position <- position_fillnudge(kept.origin = "none", vjust = 0.5, x = -0.3)
  expect_false(position$reverse)
  expect_identical(position$vjust, 0.5)
  expect_identical(position$x, -0.3)
})
