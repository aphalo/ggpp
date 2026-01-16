context("position_jitter_keep")

test_that("test expected arguments", {
  position <- position_jitter_keep()
  expect_identical(position$kept.origin, "original")
  expect_null(position$width)
  expect_null(position$height)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
  skip_if(condition = utils::packageVersion("ggplot2") < "4.0.0",
          message = "'ggplot2' < 4.0.0")
  expect_identical(position$required_aes, c("x", "y"))
  expect_type(position$aesthetics, "closure")
  expect_type(position$use_defaults, "closure")
})

