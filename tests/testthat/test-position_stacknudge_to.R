context("position_stacknudge_to")

test_that("incorrect kept.origin used", {
  expect_error(
    position_stacknudge_to(kept.origin = "") #,
#    "`kept.origin` must be one of \"stacked\", \"original\", or \"none\", not \"\"."
# rlang arg_match() adds to the error message characters that break expect_error()
  )
})

test_that("correct kept.origin used", {
  expect_no_error(
    position <- position_stacknudge_to(kept.origin = "none")
  )
})

test_that("correct kept.origin used", {
  expect_no_error(
    position <- position_stacknudge_to(kept.origin = "original")
  )
})

test_that("correct kept.origin used", {
  expect_no_error(
    position <- position_stacknudge_to(kept.origin = "stacked")
  )
})

test_that("non-numeric y used", {
  expect_error(
    position_stacknudge_to(y = "bad")
    #, "'y' must be NULL or of mode numeric"
  )
})

test_that("NA y used", {
  expect_error(
    position_stacknudge_to(y = NA)
    #, "'y' must be NULL or of mode numeric"
  )
})

test_that("non-numeric x used", {
  expect_error(
    position_stacknudge_to(x = "bad")
    #, "'x' must be NULL or of mode numeric"
  )
})

test_that("NA x used", {
  expect_error(
    position_stacknudge_to(x = "NA")
    #, "'x' must be NULL or of mode numeric"
  )
})

test_that("test if correct arguments are assigned", {
  position <- position_stacknudge_to()
  expect_false(position$reverse)
  expect_identical(position$vjust, 1)
  expect_null(position$x)
  expect_null(position$y)
  expect_identical(position$x.action, "none")
  expect_identical(position$y.action, "none")
  expect_identical(position$x.distance, "equal")
  expect_identical(position$y.distance, "equal")
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'y.action'", {
  position <- position_stacknudge_to(y.action = "spread")
  expect_false(position$reverse)
  expect_identical(position$vjust, 1)
  expect_null(position$x)
  expect_null(position$y)
  expect_identical(position$x.action, "none")
  expect_identical(position$y.action, "spread")
  expect_identical(position$x.distance, "equal")
  expect_identical(position$y.distance, "equal")
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'x.action'", {
  position <- position_stacknudge_to(x.action = "spread")
  expect_false(position$reverse)
  expect_identical(position$vjust, 1)
  expect_null(position$x)
  expect_null(position$y)
  expect_identical(position$x.action, "spread")
  expect_identical(position$y.action, "none")
  expect_identical(position$x.distance, "equal")
  expect_identical(position$y.distance, "equal")
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})


test_that("warn on x or y longer than data", {
  expect_no_warning(
  ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
    geom_point(position = "fill") +
    geom_text_s(
      position = position_stacknudge_to(x = (1:nrow(mtcars)) * 0.9 / nrow(mtcars)),
      size = 2.5
    )
)

expect_no_warning(
  ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
    geom_point(position = "fill") +
    geom_text_s(
      position = position_stacknudge_to(x = c(0.3, 0.7)),
      size = 2.5
    )
)

# warning is issue but not "seen" by 'testthat'
# expect_warning(
#   ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
#     geom_point(position = "fill") +
#     geom_text_s(
#       position = position_stacknudge_to(x = rep(c(0.3, 0.7), nrow(mtcars))),
#       size = 2.5
#     )
# )

})

