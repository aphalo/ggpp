context("position_dodge2nudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_dodge2nudge(kept.origin = "")
    #, "`kept.origin` must be one of \"dodged\", \"original\", or \"none\", not \"\"."
  )
})

test_that("correct kept.origin used", {
  position <- position_dodge2nudge(kept.origin = "dodged")
  expect_no_error(position)
})

test_that("test if correct arguments are assigned", {
  position <- position_dodge2nudge(kept.origin = "none")
  expect_false(position$reverse)
  expect_identical(position$padding, 0.1)
  expect_identical(position$preserve, "total")
  expect_identical(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
})

