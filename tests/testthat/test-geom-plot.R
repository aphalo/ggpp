context("geom_plot")

library(tibble)

test_that("character label gives error", {
   my.df <- data.frame(x = 1:10, y = 1:10, tb = letters[1:10])
   expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                  geom_plot()))
})


test_that("numeric label gives error", {
  my.df <- data.frame(x = 1:10, y = 1:10, tb = 1:10)
  expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                       geom_plot()))
})

test_that("geom_plot works as expected", {
  p1 <- ggplot(mpg, aes(displ, cty)) + geom_point()

  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point()
  # tibble
  tb <- tibble(x = 5.45, y = 34, plot = list(p1))
  result <- expect_silent(
    p + geom_plot(data = tb, aes(x = x, y = y, label = plot))
  )
  expect_s3_class(result, "ggplot")
  # data.frame
  df <- data.frame(x = 5.45, y = 34, plot = I(list(p1)))
  result1 <- expect_silent(
    p + geom_plot(data = df, aes(x = x, y = y, label = plot))
  )
  expect_s3_class(result1, "ggplot")
})

test_that("geom_plot_npc works as expected", {
  p1 <- ggplot(mpg, aes(displ, cty)) + geom_point()

  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point()
  # tibble
  tbnpc <- tibble(x = 0.95, y = 0.95,  plot = list(p1))
  result <- expect_silent(
    p + geom_plot_npc(data = tbnpc, aes(npcx = x, npcy = y, label = plot))
  )
  expect_s3_class(result, "ggplot")

  dfnpc <- data.frame(x = 0.95, y = 0.95,  plot = I(list(p1)))
  result1 <- expect_silent(
    p + geom_plot_npc(data = dfnpc, aes(npcx = x, npcy = y, label = plot))
  )
  expect_s3_class(result1, "ggplot")
})


test_that("multiple_ggplot", {
  p1 <- ggplot(mpg, aes(displ, cty)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p3 <- ggplot(mpg, aes(cyl, displ)) + geom_point()
  my.tb <- tibble(x = 0:2, y = 0:2, tb = list(p1, p2, p3))
#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_three_plots",
                              ggplot() +
                                geom_plot(data = my.tb,
                                           mapping = aes(x, y, label = tb)) +
                                lims(x = c(0, 2), y = c(0, 2)))
#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_three_angles",
                              ggplot() +
                                geom_plot(data = my.tb,
                                          mapping = aes(x, y, label = tb),
                                          angle = 90, vjust = c(1, 0.5, 0)) +
                                lims(x = c(0, 2), y = c(0, 2)))
  # expect_warning(print(ggplot() +
  #                  geom_plot(data = my.tb[2, ],
  #                            mapping = aes(x, y, label = tb),
  #                            angle = 45) +
  #                  lims(x = c(0, 2), y = c(0, 2))))
})

test_that("examples_geom_plot", {
  p <-
    ggplot(data = mtcars, mapping = aes(wt, mpg)) +
    geom_point()

  df <- tibble(x = 0.01, y = 0.01,
               plot = list(p +
                           coord_cartesian(xlim = c(3, 4),
                                           ylim = c(13, 16)) +
                           labs(x = NULL, y = NULL)))

#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_npc_1",
                              p +
                                expand_limits(x = 0, y = 0) +
                                geom_plot_npc(data = df,
                                              aes(npcx = x, npcy = y, label = plot))
  )
#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_npc_2",
                              p +
                                expand_limits(x = 0, y = 0) +
                                geom_plot_npc(data = df,
                                              aes(npcx = x, npcy = y, label = plot),
                                              angle = 90, vjust = 1)
  )
})

test_that("nudge_x_geom_plot", {
  p1 <- ggplot(mpg, aes(displ, cty)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p3 <- ggplot(mpg, aes(cyl, displ)) + geom_point()
  my.tb <- tibble(x = 0:2, y = 0:2, tb = list(p1, p2, p3))
  p <- ggplot() +
    geom_plot(data = my.tb, mapping = aes(x, y, label = tb), nudge_x = 0.1) +
    lims(x = c(0, 2), y = c(0, 2))
  result <- layer_data(p)[, c("x_orig", "y_orig")]
  expected <- data.frame(
    x_orig = c(0, 1, 2),
    y_orig = c(0, 1, 2)
  )
  expect_identical(result, expected)
})

test_that("nudge_x_and_position_geom_plot_fails", {
  p1 <- ggplot(mpg, aes(displ, cty)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p3 <- ggplot(mpg, aes(cyl, displ)) + geom_point()
  my.tb <- tibble(x = 0:2, y = 0:2, tb = list(p1, p2, p3))
  expect_error(
    ggplot() +
      geom_plot(data = my.tb, mapping = aes(x, y, label = tb), nudge_x = 0.1, position = position_nudge_keep) +
      lims(x = c(0, 2), y = c(0, 2)),
    "You must specify either `position` or `nudge_x`/`nudge_y`."
  )
})
