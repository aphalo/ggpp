context("position_stack_minmax")

test_that("incorrect kept.origin used", {
  expect_error(
    position_stack_minmax(kept.origin = "") #,
    #    "`kept.origin` must be one of \"stacked\", \"original\", or \"none\", not \"\"."
    # rlang arg_match() adds to the error message characters that break expect_error()
  )
})

test_that("correct kept.origin used", {
  expect_no_error(position <- position_stack_minmax(kept.origin = "none"))
  expect_identical(position$kept.origin, "none")
})

test_that("correct kept.origin used", {
  expect_no_error(position <- position_stack_minmax(kept.origin = "original"))
  expect_identical(position$kept.origin, "original")
})

test_that("incorrect direction used", {
  expect_error(
    position_stack_minmax(direction = "") #,
    #    "`kept.origin` must be one of \"stacked\", \"original\", or \"none\", not \"\"."
    # rlang arg_match() adds to the error message characters that break expect_error()
  )
})

test_that("correct direction used 'none'", {
  expect_no_error(position_stack_minmax(direction = "none"))
})

test_that("correct direction used 'split'", {
  expect_no_error(position_stack_minmax(direction = "split"))
})

test_that("correct direction used 'split.x'", {
  expect_no_error(position_stack_minmax(direction = "split.x"))
})

test_that("correct direction used 'split.y'", {
  expect_no_error(position_stack_minmax(direction = "split.y"))
})

test_that("test vjust, reverse, x, y and kept.origin arguments", {
  position <- position_stack_minmax()
  expect_identical(position$vjust, 1)
  expect_identical(position$reverse, FALSE)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_identical(position$kept.origin, "stacked")
  expect_identical(position$var, "y")
  expect_identical(position$fill, FALSE)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
  skip_if(condition = utils::packageVersion("ggplot2") < "4.0.0",
          message = "'ggplot2' < 4.0.0")
  expect_false(position$reverse)
  expect_identical(position$required_aes, character(0))
  expect_type(position$aesthetics, "closure")
  expect_type(position$use_defaults, "closure")
})
