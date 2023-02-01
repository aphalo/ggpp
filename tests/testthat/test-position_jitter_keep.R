context("position_jitter_keep")

test_that("test expected arguments", {
  position <- position_jitter_keep()
  expect_null(position$width)
  expect_null(position$height)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_identical(position$kept.origin, "original")
})

