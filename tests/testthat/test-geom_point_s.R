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
})
