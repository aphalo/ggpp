context("geom_point_s")

test_that("geom_point_s runs successfully", {
  expect_no_error(
    ggplot(mpg[1:20, ],
           aes(cyl, hwy, label = drv)) +
      geom_point_s(position = position_nudge_keep(x = 0.2),
                   color = "red") +
      geom_point_s()
  )
})


test_that("geom_point_s pos_or_nudge", {
  expect_error(
    ggplot(mpg[1:20, ],
           aes(cyl, hwy, label = drv)) +
      geom_point_s(position = "",
                   nudge_x = 0.5),
    "You must specify either `position` or `nudge_x`/`nudge_y`."
  )

  expect_silent(
    ggplot(mpg[1:20, ],
           aes(cyl, hwy, label = drv)) +
      geom_point_s(nudge_x = 0.2, nudge_y = -0.2)
    )

})

test_that("translate_shape_string maps correctly", {
  expected <- c(0:14, 14:25)
  shape.strings <- c("square open",
  "circle open",
  "triangle open",
  "plus",
  "cross",
  "diamond open",
  "triangle down open",
  "square cross",
  "asterisk",
  "diamond plus",
  "circle plus",
  "star",
  "square plus",
  "circle cross",
  "square triangle",
  "triangle square",
  "square",
  "circle small",
  "triangle",
  "diamond",
  "circle",
  "bullet",
  "circle filled",
  "square filled",
  "diamond filled",
  "triangle filled",
  "triangle down filled")
  result <- translate_shape_string(shape.strings)
  expect_equal(expected, result)

  expect_error(translate_shape_string("wrong shape")) # no match
  expect_error(translate_shape_string("circ")) # multiple matches

  # chars are passed through
  expect_equal(translate_shape_string("c"), "c")
  expect_equal(translate_shape_string(""), "")
  expect_equal(translate_shape_string("1"), "1")
  expect_error(translate_shape_string("11"))
})
