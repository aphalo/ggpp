context("position_dodgenudge")

test_that("incorrect kept.origin used", {
  expect_error(
    position_dodgenudge(kept.origin = "")
    #, "`kept.origin` must be one of \"dodged\", \"original\", or \"none\", not \"\"."
  )
})

test_that("incorrect direction used", {
  expect_error(
    position_dodgenudge(direction = "")
    #, "`kept.origin` must be one of \"dodged\", \"original\", or \"none\", not \"\"."
  )
})

test_that("correct kept.origin used", {
  position <- position_dodgenudge(kept.origin = "dodged")
  expect_no_error(position)
})

test_that("test if correct arguments are assigned", {
  position <- position_dodgenudge(kept.origin = "none")
  expect_identical(position$preserve, "total")
  expect_identical(position$width, 1)
  expect_equal(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'split.y'", {
  position <- position_dodgenudge(kept.origin = "none", direction = "split.y")
  expect_identical(position$width, 1)
  expect_equal(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'split.x'", {
  position <- position_dodgenudge(kept.origin = "none", direction = "split.x")
  expect_identical(position$width, 1)
  expect_equal(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'split'", {
  position <- position_dodgenudge(kept.origin = "none", direction = "split")
  expect_identical(position$width, 1)
  expect_equal(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'none'", {
  position <- position_dodgenudge(kept.origin = "none", direction = "none")
  expect_identical(position$width, 1)
  expect_equal(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("test if correct arguments are assigned with 'center'", {
  position <- position_dodgenudge(kept.origin = "none", direction = "center")
  expect_identical(position$width, 1)
  expect_equal(position$width, 1)
  expect_identical(position$x, 0)
  expect_identical(position$y, 0)
  expect_type(position$compute_panel, "closure")
  expect_type(position$compute_layer, "closure")
  expect_type(position$setup_data, "closure")
  expect_type(position$setup_params, "closure")
})

test_that("dodgenudge plots are correct", {
  expect_no_warning(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
      geom_text_s(
        position = position_dodgenudge(width = 0.85,
                                        x = 0, y = rep(0.5, nrow(mtcars))),
        size = 2.5
      )
  )

  expect_no_warning(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
      geom_text_s(
        position = position_dodgenudge(width = 0.85,
                                        y = 0, x = rep(0.05, nrow(mtcars))),
        size = 2.5
      )
  )

  vdiffr::expect_doppelganger("dodge-nudge1",
                              ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
                                geom_text_s(
                                  mapping = aes(group = cyl),
                                  position = position_dodgenudge(width = 0.85,
                                                                 y = 0, x = c(0.1)),
                                  size = 2.5
                                )
  )

  vdiffr::expect_doppelganger("dodge-nudge2",
                              ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
                                geom_text_s(
                                  mapping = aes(group = cyl),
                                  position = position_dodgenudge(width = 0.85,
                                                                 y = 0, x = c(-0.05, 0.05)),
                                  size = 2.5
                                )
  )

  vdiffr::expect_doppelganger("dodge-nudge3",
                              ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
                                geom_text_s(
                                  mapping = aes(group = cyl),
                                  position = position_dodgenudge(width = 0.85,
                                                                 direction = "split.x",
                                                                 y = 0, x = 0.05),
                                  size = 2.5
                                )
  )
}
)
