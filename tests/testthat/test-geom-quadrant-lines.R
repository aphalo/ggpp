context("geom_quadrant_lines")

library(ggplot2)
  tst.df <- data.frame(
    x = c(5, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )

test_that("geom_quadrant_lines works as expected with default values", {
  res <- ggplot(tst.df, aes(x = x, y = y)) +
    geom_point() +
    geom_quadrant_lines()

  df_res <- layer_data(res, 2)
  gp <- -1L
  attr(gp, "n") <- 1

  expected <- data.frame(
    xintercept = 0,
    yintercept = 0,
    PANEL = factor(1),
    group = gp,
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  )

  expect_equal(df_res, expected)
})

test_that("geom_quadrant_lines works as expected with custom parameter values", {
  res <- ggplot(tst.df, aes(x = x, y = y)) +
    geom_point() +
    geom_quadrant_lines(
      xintercept = 0.5,
      yintercept = 0.5,
      colour = "blue",
      linetype = "dashed"
    )

  df_res <- layer_data(res, 2)
  gp <- -1L
  attr(gp, "n") <- 1

  expected <- data.frame(
    xintercept = 0.5,
    yintercept = 0.5,
    PANEL = factor(1),
    group = gp,
    colour = "blue",
    linewidth = 0.5,
    linetype = "dashed",
    alpha = NA
  )

  expect_equal(df_res, expected)

  # pool.along works as expected.
  res <- ggplot(tst.df, aes(x = x, y = y)) +
    geom_point() +
    geom_quadrant_lines(
      pool.along = "y",
      xintercept = 0.5,
      yintercept = 10,
      colour = "blue",
      linetype = "dashed"
    )

  vdiffr::expect_doppelganger("geom_quadrant_lines_pool_y", res)

  res <- ggplot(tst.df, aes(x = x, y = y)) +
    geom_point() +
    geom_quadrant_lines(
      pool.along = "x",
      xintercept = 0.5,
      yintercept = 10,
      colour = "blue",
      linetype = "dashed"
    )

  vdiffr::expect_doppelganger("geom_quadrant_lines_pool_x", res)
})

test_that("geom_vhlines returns an error if neither x nor y intercept is provided", {
  expect_error(
    print(
      ggplot(tst.df, aes(x = x, y = y)) +
        geom_point() +
        geom_vhlines()
    )
  )
})

test_that("geom_vhlines works as expected with multiple intercepts", {
  res <- ggplot(tst.df, aes(x = x, y = y)) +
    geom_point() +
    geom_vhlines(
      xintercept = c(-1, 1),
      yintercept = c(0.5, 1)
    )

  df_res <- layer_data(res, 2)
  gp <- c(-1L, -1L)
  attr(gp, "n") <- 1

  expected <- data.frame(
    xintercept = c(-1, 1),
    yintercept = c(0.5, 1),
    PANEL = factor(c(1, 1)),
    group = gp,
    colour = c("black", "black"),
    linewidth = c(0.5, 0.5),
    linetype = c(1, 1),
    alpha = c(NA, NA)
  )

  expect_equal(df_res, expected)
})
