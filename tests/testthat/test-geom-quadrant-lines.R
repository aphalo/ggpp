context("geom_quadrant_lines")

library(ggplot2)

test_that("geom_quadrant_lines", {
  tst.df <- data.frame(
    x = c(5, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )

  # Test default behaviour.
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
    size = 0.5,
    linewidth = 0.5,
    linetype = "dashed",
    alpha = NA
  )

  expect_equal(df_res, expected)

  # Test custom parameters values.
  res <- ggplot(tst.df, aes(x = x, y = y)) +
    geom_point() +
    geom_quadrant_lines(
      xintercept = 0.5,
      yintercept = 0.5,
      colour = "blue",
      linetype = "dashed"
    )

  df_res <- layer_data(res, 2)
  expected$xintercept <- 0.5
  expected$yintercept <- 0.5
  expected$colour <- "blue"
  expected$linetype <- "dashed"

  expect_equal(df_res, expected)
})


test_that("geom_vhlines", {
  tst.df <- data.frame(
    x = c(5, -0.2, -0.2, -2.5, -0.6, -0.1),
    y = c(1.1, -0.6, -0.9, -1.6, 0.3, 1.6),
    group = c("A", "A", "A", "B", "B", "B")
  )

  # geom_vhlines requires xintercept or yintercept.
  expect_error(
    print(
      ggplot(tst.df, aes(x = x, y = y)) +
        geom_point() +
        geom_vhlines()
    )
  )

  # geom_vhlines works with custome values.
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
    size = c(0.5, 0.5),
    linewidth = c(0.5, 0.5),
    linetype = c(1, 1),
    alpha = c(NA, NA)
  )

  expect_equal(df_res, expected)
})
