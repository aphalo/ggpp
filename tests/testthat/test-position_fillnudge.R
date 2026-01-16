context("position_fillnudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_fillnudge(kept.origin = "") #,
#    "`kept.origin` must be one of \"stacked\", \"original\", or \"none\", not \"\"."
# rlang arg_match() adds to the error message characters that break expect_error()
  )
})

test_that("correct kept.origin used", {
  position <- position_fillnudge(kept.origin = "none")
  expect_no_error(position)
})

test_that("correct kept.origin used", {
  position <- position_fillnudge(kept.origin = "original")
  expect_no_error(position)
})

test_that("incorrect direction used", {
  expect_error(
    position_stack_minmax(direction = "") #,
    #    "`kept.origin` must be one of \"stacked\", \"original\", or \"none\", not \"\"."
    # rlang arg_match() adds to the error message characters that break expect_error()
  )
})

test_that("correct direction used", {
  position <- position_stack_minmax(direction = "none")
  expect_no_error(position)
})

test_that("correct direction used", {
  position <- position_stack_minmax(direction = "split")
  expect_no_error(position)
})

test_that("correct direction used", {
  position <- position_stack_minmax(direction = "split.x")
  expect_no_error(position)
})

test_that("correct direction used", {
  position <- position_stack_minmax(direction = "split.y")
  expect_no_error(position)
})

test_that("correct reverse, vjust and x assigned", {
  position <- position_fillnudge(kept.origin = "none", vjust = 0.5, x = -0.3)
  expect_identical(position$kept.origin, "none")
  expect_false(position$reverse)
  expect_identical(position$vjust, 0.5)
  expect_identical(position$y, 0)
  expect_identical(position$x, -0.3)
  expect_identical(position$fill, TRUE)
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

test_that("warn on x or y longer than data", {
  expect_no_warning(
  ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
    geom_text_s(
      position = position_stacknudge(x = 0.05, y = rep(5, nrow(mtcars))),
      size = 2.5
    )
)

expect_no_warning(
  ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
    geom_text_s(
      position = position_stacknudge(y = 0, x = rep(0.05, nrow(mtcars))),
      size = 2.5
    )
)
})

