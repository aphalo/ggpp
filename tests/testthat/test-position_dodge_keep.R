context("position_dodge_keep")

test_that("test expected arguments", {
  position <- position_dodge_keep()
  expect_identical(position$width, 1)
  expect_identical(position$preserve, "total")
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_identical(position$kept.origin, "original")
})
