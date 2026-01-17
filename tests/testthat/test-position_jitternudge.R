context("position_jitternudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_jitternudge(kept.origin = "wrong")
    #,  "`kept.origin` must be one of \"jittered\", \"original\", or \"none\", not\n\"wrong\"."
  )
})

test_that("correct kept.origin used", {
  expect_no_error(position <- position_jitternudge())
  expect_identical(position$kept.origin, "jittered")
})

test_that("correct kept.origin used", {
  expect_no_error(position <- position_jitternudge(kept.origin = "jittered"))
  expect_identical(position$kept.origin, "jittered")
})

test_that("correct kept.origin used", {
  expect_no_error(position <- position_jitternudge(kept.origin = "original"))
  expect_identical(position$kept.origin, "original")
})

test_that("correct kept.origin used", {
  expect_no_error(position <- position_jitternudge(kept.origin = "none"))
  expect_identical(position$kept.origin, "none")
})

test_that("incorrect nudge.from used", {
  expect_error(
    position_jitternudge(nudge.from = "original.z")
    #, "`nudge.from` must be one of \"original\", \"original.x\", \"original.y\",\n\"jittered\", \"jittered.y\", or \"jittered.x\", not \"original.z\".\nℹ Did you mean \"original.x\"?"
  )
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge())
  expect_identical(position$nudge.from, "original")
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge(nudge.from = "original"))
  expect_identical(position$nudge.from, "original")
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge(nudge.from = "original.x"))
  expect_identical(position$nudge.from, "original.x")
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge(nudge.from = "original.y"))
  expect_identical(position$nudge.from, "original.y")
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge(nudge.from = "jittered"))
  expect_identical(position$nudge.from, "jittered")
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge(nudge.from = "jittered.x"))
  expect_identical(position$nudge.from, "jittered.x")
})

test_that("correct nudge.from used", {
  expect_no_error(position <- position_jitternudge(nudge.from = "jittered.y"))
  expect_identical(position$nudge.from, "jittered.y")
})

test_that("test if correct arguments are assigned", {
  position <- position_jitternudge(width = 0.2, height = 2,
                                   seed = 123, x = 0.35,
                                   direction = "split",
                                   nudge.from = "original.x")

  expect_identical(position$kept.origin, "jittered")
  expect_identical(position$nudge.from, "original.x")
  expect_identical(position$width, 0.2)
  expect_identical(position$height, 2)
  expect_identical(position$seed, 123)
  expect_identical(position$x, 0.35)
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

test_that("test if correct arguments are assigned with x < 0", {
  position <- position_jitternudge(width = 0.2, height = 2,
                                   seed = 123, x = -0.35, y = -1,
                                   direction = "alternate",
                                   nudge.from = "original.x")

  expect_identical(position$kept.origin, "jittered")
  expect_identical(position$nudge.from, "original.x")
  expect_identical(position$width, 0.2)
  expect_identical(position$height, 2)
  expect_identical(position$seed, 123)
  expect_identical(position$x, -0.35)
  expect_identical(position$y, -1)
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

test_that("test if correct arguments are assigned with x < 0", {
  position <- position_jitternudge(width = 0.2, height = 2,
                                   seed = 123, x = -0.35, y = -1,
                                   direction = "as.is",
                                   nudge.from = "original.y",
                                   kept.origin = "original")

  expect_identical(position$kept.origin, "original")
  expect_identical(position$nudge.from, "original.y")
  expect_identical(position$width, 0.2)
  expect_identical(position$height, 2)
  expect_identical(position$seed, 123)
  expect_identical(position$x, -0.35)
  expect_identical(position$y, -1)
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

test_that("warn on x or y longer than data", {
  expect_no_warning(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
      geom_text_s(
        position = position_jitternudge(x = 0, y = rep(0.5, nrow(mtcars))),
        size = 2.5
      )
  )

  expect_no_warning(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
      geom_text_s(
        position = position_jitternudge(y = 0, x = rep(0.05, nrow(mtcars))),
        size = 2.5
      )
  )
})
