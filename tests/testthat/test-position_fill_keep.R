context("position_fill_keep")

test_that("test vjust, reverse, x, y and kept.origin arguments", {
  position <- position_fill_keep()
  expect_identical(position$vjust, 1)
  expect_identical(position$reverse, FALSE)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_identical(position$kept.origin, "original")
})
