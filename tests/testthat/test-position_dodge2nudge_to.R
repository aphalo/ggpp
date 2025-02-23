context("position_dodge2nudge_to")

test_that("incorrect kept.origin used", {
  expect_error(
    position_dodge2nudge_to(kept.origin = "")
    #, "`kept.origin` must be one of \"dodged\", \"original\", or \"none\", not \"\"."
  )
})

test_that("incorrect preserve used", {
  expect_error(
    position_dodge2nudge_to(preserve = "")
    #, "`preserve` must be one of \"total\", or \"single\", not \"\"."
  )
})

test_that("non-numeric y used", {
  expect_error(
    position_dodge2nudge_to(y = "bad")
    #, "'y' must be NULL or of mode numeric"
  )
})

test_that("NA y used", {
  expect_error(
    position_dodge2nudge_to(y = NA)
    #, "'y' must be NULL or of mode numeric"
  )
})

test_that("non-numeric x used", {
  expect_error(
    position_dodge2nudge_to(y = "bad")
    #, "'x' must be NULL or of mode numeric"
  )
})

test_that("NA x used", {
  expect_error(
    position_dodge2nudge_to(x = "NA")
    #, "'x' must be NULL or of mode numeric"
  )
})

test_that("correct kept.origin used", {
  expect_error(
    position_dodge2nudge_to(kept.origin = "stacked")
  )
})

test_that("correct kept.origin used", {
  expect_no_error(
    position_dodge2nudge_to(kept.origin = "original")
  )
})

test_that("correct kept.origin used", {
  expect_no_error(
    position_dodge2nudge_to(kept.origin = "none")
  )
})

test_that("correct kept.origin used", {
  expect_no_error(
    position_dodge2nudge_to(kept.origin = "dodged")
  )
})


test_that("test if correct arguments are assigned", {
  position <- position_dodge2nudge_to(kept.origin = "none")
  expect_false(position$reverse)
  expect_identical(position$padding, 0.1)
  expect_identical(position$preserve, "total")
  expect_identical(position$width, 1)
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
  position <- position_dodge2nudge_to(kept.origin = "none", y.action = "spread")
  expect_false(position$reverse)
  expect_identical(position$padding, 0.1)
  expect_identical(position$preserve, "total")
  expect_identical(position$width, 1)
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
  position <- position_dodge2nudge_to(kept.origin = "none", y.action = "spread")
  expect_false(position$reverse)
  expect_identical(position$padding, 0.1)
  expect_identical(position$preserve, "total")
  expect_identical(position$width, 1)
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

test_that("dodge2nudge_to plots are correct", {
    expect_silent(
      ggplot(mtcars, aes(x = am, y = mpg, label = mpg)) +
        geom_text_s(
          position = position_dodge2nudge_to(width = 0.85),
          size = 2.5
        )
    )

  expect_silent(
    ggplot(mtcars, aes(x = am, y = mpg, label = mpg, group = am)) +
      geom_point(position = position_dodge2(width = 0.1)) +
      geom_text_s(
        position = position_dodge2nudge_to(width = 0.1,
                                           x = 0.5,
                                           kept.origin = "dodged"),
        size = 2.5
      )
  )

  vdiffr::expect_doppelganger("dodge2-nudge-to-1",
                              ggplot(mtcars, aes(x = am, y = mpg, label = mpg, group = am)) +
                                geom_point(position = position_dodge2(width = 0.1)) +
                                geom_text_s(
                                  position = position_dodge2nudge_to(width = 0.1,
                                                                     x = 0.5,
                                                                     kept.origin = "dodged"),
                                  size = 2.5
                                )
  )

  vdiffr::expect_doppelganger("dodge2-nudge-to-2",
                              ggplot(mtcars, aes(y = am, x = mpg, label = mpg, group = am)) +
                                geom_point(position = position_dodge2(width = 0.1)) +
                                geom_text_s(
                                  position = position_dodge2nudge_to(width = 0.1,
                                                                     y = 0.5,
                                                                     kept.origin = "dodged"),
                                  size = 2.5
                                )
  )

#  skip_on_os(os = "mac")
  vdiffr::expect_doppelganger("dodge3-nudge-to-3",
                              ggplot(mtcars, aes(x = am, y = mpg, label = mpg, group = am)) +
                                geom_point(position = position_dodge2(width = 0.1,
                                                                      padding = 0.5)) +
                                geom_text_s(
                                  position = position_dodge2nudge_to(width = 0.1,
                                                                     padding = 0.5,
                                                                     x = 0.5,
                                                                     y.expansion = 0.1,
                                                                     kept.origin = "dodged"),
                                  size = 2.5
                                )
  )

  vdiffr::expect_doppelganger("dodge3-nudge-to-4",
                              ggplot(mtcars, aes(x = am, y = mpg, label = mpg, group = am)) +
                                geom_point(position = position_dodge2(width = 0.1)) +
                                geom_text_s(
                                  position = position_dodge2nudge_to(width = 0.1,
                                                                     padding = 0.5,
                                                                     y = 2,
                                                                     kept.origin = "dodged"),
                                  size = 2.5
                                )
  )


}
)
